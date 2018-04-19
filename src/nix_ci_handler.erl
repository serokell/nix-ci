-module(nix_ci_handler).

-export([init/2]).

secret() -> os:getenv("NIX_CI_GITHUB_SECRET").

init(Req0, Opts) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Event = cowboy_req:header(<<"x-github-event">>, Req1),
    Signature = cowboy_req:header(<<"x-hub-signature">>, Req1),
    Resp = handle(Event, Body, Req1, Signature),
    {ok, Resp, Opts}.

handle(Event, Body, Req, Signature0) ->
    Signature1 = encode_signature(crypto:hmac(sha, secret(), Body)),
    if Signature0 == Signature1 ->
	    handle_trusted(Event, Body, Req);
       true ->
	    cowboy_req:reply(400, Req)
    end.

binary_to_hex(Bin) ->
    lists:flatten([io_lib:format("~2.16.0b", [X]) ||
		      X <- binary_to_list(Bin)]).

encode_signature(Bin) ->
    list_to_binary("sha1=" ++ binary_to_hex(Bin)).

handle_trusted(<<"pull_request">>, Body, Req) ->
    #{<<"pull_request">> := #{<<"head">> := HEAD}} = jsone:decode(Body),
    #{<<"repo">> := #{<<"full_name">> := Name}} = HEAD,
    #{<<"ref">> := Ref} = HEAD,
    #{<<"sha">> := Rev} = HEAD,
    spawn(fun() -> build({Name, Ref, Rev}) end),
    cowboy_req:reply(202, Req);
handle_trusted(_, _, Req) ->
    cowboy_req:reply(200, Req).

build(Coordinates = {Name, Ref, Rev}) ->
    URL0 = io_lib:format(<<"https://ci.serokell.io/log/?pid=~w">>, [self()]).
    nix_ci_github:status(Coordinates, <<"Building...">>, <<"pending">>, URL0),
    Expr = nix_ci_builder:git_expression(nix_ci_github:ssh_url(Name), Ref, Rev),
    {Status, Output} = nix_ci_builder:build(Expr),
    Description = list_to_binary(lists:last(string:tokens(Output, "\n"))),
    URL = nix_ci_github:gist(iolist_to_binary(Output)),
    nix_ci_github:status(Coordinates, Description, encode_status(Status), URL).

encode_status(0) ->
    <<"success">>;
encode_status(_) ->
    <<"failure">>.
