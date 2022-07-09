-module(room_manager).
-export([start/0, create_account/1, close_account/1, login/1, logout/1, online/0]).

start() -> register(room_manager, spawn(fun() -> loop (#{}) end)).

invoke_rpc(Request) ->
    ?MODULE ! {Request, self()},
    receive {Res, ?MODULE} -> Res end.

create_account(User) -> invoke_rpc({create_account, User}).
close_account(User) -> invoke_rpc({close_account, User}).
login(User) -> invoke_rpc({login, User}).
change_name(Old, New) -> invoke_rpc({change_name, {Old, New}}).
logout(User) -> invoke_rpc({logout, User}).
online() -> invoke_rpc(online).

loop(Map) ->
    receive
        {{create_account, User}, From} ->
            case maps:is_key(User, Map) of
                true ->
                    From ! {user_exists, ?MODULE},
                    loop(Map);
                false ->
                    From ! {ok, ?MODULE},
                    loop(maps:put(User, false, Map))
            end;
        {{close_account, User}, From} ->
            case maps:find(User, Map) of
                {ok, _} ->
                    From ! {ok, ?MODULE},
                    loop(maps:remove(User, Map));
                _ ->
                    From ! {invalid, ?MODULE},
                    loop(Map)
            end;

        {{login, User}, From} ->
            case maps:find(User, Map) of
                {ok, false} ->
                    From ! {ok, ?MODULE},
                    loop(maps:put(User, true, Map));
                _ ->
                    From ! {invalid, ?MODULE},
                    loop(Map)
            end;

        change_name({change_name, {OldName, NewName}}, From) ->
            case maps:find(OldName, Map) of
                {ok, bool} ->
                    case maps_find(NewName, Map) of
                        {ok, {}}
                    loop(maps:put(User, P, Map));
                _ -> loop(Map)
            end;

        {{logout, User}, From} ->
            From ! {ok, ?MODULE},
            case maps:find(User, Map) of
                {ok, P} ->
                    loop(maps:put(User, P, Map));
                _ -> loop(Map)
            end;

        {online, From} ->
            F = fun (User, true, Accs) -> [User | Accs];
                    (_User, _, Accs)   -> Accs
                end,
            Res = maps:fold(F, [], Map),
            % Same result with list comprehensions.
            %Res2 = [U || {U, {_P, true}} <- maps:to_List(Map)],
            From ! {Res, ?MODULE},
            loop(maps:fold(F, [], Map))
    end.