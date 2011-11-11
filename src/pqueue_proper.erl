-module(pqueue_proper).

-include_lib("proper/include/proper.hrl").

-behaviour(proper_statem).

-export([qc_pq/0, qc_pq2/0, correct/1]).

-export([command/1, initial_state/0, next_state/3, postcondition/3,
         precondition/2]).

-type value() :: integer().
-record(state, { in_queue :: [{value(), term()}] }).
-define(SERVER, queue_srv).

priority() ->
    integer(-20, 20).

value() ->
    integer().

initial_state() ->
    #state { in_queue = [] }.

command(_S) ->
    oneof([{call, ?SERVER, in, [value()]},
           {call, ?SERVER, in, [value(), priority()]},
           {call, ?SERVER, is_empty, []},
%           {call, ?SERVER, is_queue, [PQ]}, %% Always assume we are calling with a Q
           {call, ?SERVER, len, []}
%           {call, ?SERVER, out, [PQ]},
%%           {call, ?SERVER, out, [priority(InQ), PQ]},
%%           {call, ?SERVER, pout, [priority(InQ), PQ]},
%           {call, ?SERVER, to_list, [PQ]}]
]).

next_state(S, _V, {call, _, is_empty, _}) -> S;
next_state(S, _V, {call, _, len, _}) -> S;
next_state(#state { in_queue = InQ } = S, _V, {call, _, in, [Value, Prio]}) ->
    S#state { in_queue = [{Prio, Value} | InQ] };
next_state(#state { in_queue = InQ } = S, _V, {call, _, in, [Value]}) ->
    S#state { in_queue = [{0, Value} | InQ] }.

precondition(_S, {call, _, out, _}) -> true;
precondition(_S, {call, _, to_list, _}) -> true;
precondition(_S, {call, _, len, _}) -> true;
precondition(_S, {call, _, is_queue, _}) -> true;
precondition(_S, {call, _, is_empty, _}) -> true;
precondition(_S, {call, _, in, _}) -> true.

postcondition(S, {call, _, out, _}, R) ->
    case R of
        {empty, _} ->
            S#state.in_queue == [];
        {{value, V}, _} ->
            is_minimum(V, S#state.in_queue)
    end;
postcondition(S, {call, _, to_list, _}, R) ->
    R == lists:sort(S#state.in_queue);
postcondition(S, {call, _, len, _}, L) ->
    L == length(S#state.in_queue);
postcondition(_S, {call, _, is_queue, _}, true) -> true;
postcondition(S, {call, _, is_empty, _}, Res) ->
    Res == (S#state.in_queue == []);
postcondition(_S, {call, _, in, _}, _) ->
    true;
postcondition(_, _, _) ->
    false.

correct(M) ->
    ?FORALL(Cmds, commands(?MODULE),
            ?TRAPEXIT(
                begin
                    ?SERVER:start_link(M),
                    {History,State,Result} = run_commands(?MODULE, Cmds),
                    ?SERVER:stop(),
                    ?WHENFAIL(io:format("History: ~w\nState: ~w\nResult: ~w\n",
                                        [History,State,Result]),
                              aggregate(command_names(Cmds), Result =:= ok))
                end)).

qc_pq() ->
    proper:quickcheck(pqueue_proper:correct(pqueue)).

qc_pq2() ->
    proper:quickcheck(pqueue_proper:correct(pqueue2)).

%% ----------------------------------------------------------------------

remove_min(V, L) ->
    case find_min(L) of
        empty ->
            exit(error);
        {P, _} ->
            remove_min(P, V, L)
    end.


remove_min(P, V, [{P, V} | R]) -> R;
remove_min(P, V, [K | R]) -> 
    [K | remove_min(P, V, R)];
remove_min(_P, _V, []) -> exit(error).

is_minimum(V, L) ->
    case find_min(L) of
        empty -> empty;
        {_, V} -> true;
        {_, _} -> false
    end.

find_min([]) ->
    empty;
find_min([{P, V} | R]) ->
    find_min(P, V, R).

%% TODO: This one will stabilize-bug all over the place
find_min(P, _V, [{P1, V1} | R]) when P1 =< P ->
    find_min(P1, V1, R);
find_min(P, V, [{P1, _V1} | R]) when P < P1 ->
    find_min(P, V, R);
find_min(P, V, []) ->
    {P, V}.

