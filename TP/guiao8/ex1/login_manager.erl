-module(login_manager).
-export([start/0, create_account/2, close_account/2, login/2, logout/1, online/0]).

start() -> register(login_manager, spawn(fun() -> loop (#{}) end)).

invoke_rpc(Request) ->
    ?MODULE ! {Request, self()},
    receive {Res, ?MODULE} -> Res end.

create_account(User, Pass) -> invoke_rpc({create_account, User, Pass}).
close_account(User, Pass) -> invoke_rpc({close_account, User, Pass}).
login(User, Pass) -> invoke_rpc({login, User, Pass}).
logout(User) -> invoke_rpc({logout, User}).
online() -> invoke_rpc(online).

loop(Map) ->
    receive
        {{create_account, User, Pass}, From} ->
            case maps:is_key(User, Map) of
                true ->
                    From ! {user_exists, ?MODULE},
                    loop(Map);
                false ->
                    From ! {ok, ?MODULE},
                    loop(maps:put(User, {Pass, false}, Map))
            end;
        {{close_account, User, Provided_Pass}, From} ->
            case maps:find(User, Map) of
                {ok, {Provided_Pass, _}} ->
                    From ! {ok, ?MODULE},
                    loop(maps:remove(User, Map));
                _ ->
                    From ! {invalid, ?MODULE},
                    loop(Map)
            end;

        {{login, User, Provided_Pass}, From} ->
            case maps:find(User, Map) of
                {ok, {Stored_Pass, false}} when Stored_Pass =:= Provided_Pass  ->
                    From ! {ok, ?MODULE},
                    loop(maps:put(User, {Stored_Pass, true}, Map));
                _ ->
                    From ! {invalid, ?MODULE},
                    loop(Map)
            end;

        {{logout, User}, From} ->
            From ! {ok, ?MODULE},
            case maps:find(User, Map) of
                {ok, {P, true}} ->
                    loop(maps:put(User, {P, false}, Map));
                _ -> loop(Map)
            end;

        {online, From} ->
            F = fun (User, {_Pass, true}, Accs) -> [User | Accs];
                    (_User, _, Accs)            -> Accs
                end,
            Res = maps:fold(F, [], Map),
            % Same result with list comprehensions.
            %Res2 = [U || {U, {_P, true}} <- maps:to_List(Map)],
            From ! {Res, ?MODULE},
            loop(maps:fold(F, [], Map))
    end.