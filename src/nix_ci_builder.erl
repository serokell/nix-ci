-module(nix_ci_builder).

-export([build/1, git_expression/3, tarball_expression/1]).

git_expression(URL, Ref, Rev) ->
    io_lib:format(<<"import (builtins.fetchGit { url = \"~s\"; ref = \"~s\"; rev = \"~s\"; })">>,
		  [URL, Ref, Rev]).

tarball_expression(URL) ->
    io_lib:format(<<"import (builtins.fetchTarball ~s)">>, [URL]).

consume_port(Port) ->
    consume_port(Port, [], []).

consume_port(Port, Output, Subscribers) ->
    receive
        {Port, {data, Bytes}} ->
            [Pid ! {data, Ref, Bytes} || {Pid, Ref} <- Subscribers],
            consume_port(Port, [Output|Bytes]);
        {Port, {exit_status, Status}} ->
            [Pid ! {done, Ref} || {Pid, Ref} <- Subscribers],
            {Status, Output};
        {subscribe, Ref, Pid} ->
            % would be nice if we had some more info on this builder
            _MonRef = erlang:monitor(process, Pid),
            Pid ! {start, Ref, Output},
            consume_port(Port, Output, [Subscribers|{Pid, Ref}]);
        {'DOWN', _MonRef, process, DownPid, _Reason} ->
            Subscribers1 = [{Pid, Ref} || {Pid, Ref} <- Subscribers, Pid =/= DownPid],
            consume_port(Port, Output, Subscribers1)
    end.

build(Expr) ->
    consume_port(erlang:open_port({spawn_executable, os:find_executable("nix-build")},
				  [{args, [<<"-E">>, Expr, <<"--no-out-link">>, <<"--show-trace">>]},
				   exit_status, stderr_to_stdout])).
