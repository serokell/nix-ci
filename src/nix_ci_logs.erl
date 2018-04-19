-module(nix_ci_logs).
-export([init/2]).
-export([info/3]).

-record(state, {pid, ref, monref, exists}).

init(Req0, _) ->
    Ref = make_ref(),
    {PidBin, Req1} = cowboy_req:qs_val(<<"pid">>, Req0),
    Pid = erlang:list_to_pid(PidBin),
    MonRef = erlang:monitor(process, Pid),
    Pid ! {subscribe, Ref, self()},
    {cowboy_loop, Req1, #state{pid=Pid, ref=Ref, monref=MonRef, exists=false}}.

info({start, Ref, Bytes}, Req0, State=#state{ref=Ref, exists=false}) ->
    Req1 = cowboy_req:stream_reply(200, #{
                                     <<"content-type">> => <<"text/event-stream">>
                                         % connection: close?
                                    }, Req0),
    cowboy_req:stream_body(Bytes, nofin, Req1),
    {ok, Req1, State#state{exists=true}};

info({data, Ref, Bytes}, Req, State=#state{ref=Ref}) ->
    cowboy_req:stream_body(Bytes, nofin, Req),
    {ok, Req, State};

info({done, Ref}, Req, State=#state{ref=Ref, monref=MonRef}) ->
    erlang:demonitor(MonRef, [flush]),
    cowboy_req:stream_body(<<>>, fin, Req),
    {ok, Req, State};

info({'DOWN', MonRef, process, Pid, _Reason}, Req, #state{pid=Pid, monref=MonRef, exists=true}) ->
    % todo: use reason
    cowboy_req:stream_body(<<"build process terminated unexpectedly">>, fin, Req),
    {ok, Req, {}};

info({'DOWN', MonRef, process, Pid, _Reason}, Req, #state{pid=Pid, monref=MonRef, exists=false}) ->
    % todo: use reason
    {ok, cowboy_req:reply(404, #{}, <<"no such builder, check github">>, Req), {}}.
