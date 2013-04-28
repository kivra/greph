%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% @todo default arguments
%%% @todo workflow graphs
%%% @todo lazy evaluation
%%% @todo parallel evaluation
%%% @todo logging & reporting
%%% @todo decorators
%%% @todo debugging tools: schema, plan
%%% @todo fine grained topsort: basic blocks
%%% @todo graph templates (generate behaviour .erl)
%%% @todo nested graphs
%%% @todo graph composition: inline, ++
%%% @todo api: compile + eval
%%% @todo applyfun (fluffy:call())
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(greph).

%%%_* Exports ==========================================================
-export([ compile/1
	]).

-export_type([
             ]).

%%%_* Includes =========================================================
-include("greph.hrl").

%%%_* Code =============================================================
-type graphspec() :: eon:object(atom(), funspec()).       %
-type funspec()   :: {[arg()], func()}.                   %
-type arg()       :: atom()                               %
                   | {atom(), _}                          %Default
-type func()      :: fun()                                %
                   | {atom(), atom()}                     %Mod:Fun
                   | mfa().                               %
-type graphfun()  :: fun((eon:object()) -> eon:object()). %

-record(node,
        { label
        , args
        , func
        }).


-spec compile(graphspec()) -> maybe(graphfun(), _).
compile(Spec) ->
  ?do(?thunk(Spec),
      fun check/1,
      fun spec2graph/1,
      fun kahn_sort/1,
      fun do_compile/1
      ).


%% Input.
check(Spec) ->eon:new(Spec, fun(K, V) -> is_atom(K) andalso is_funspec(V) end).

is_funspec({Args, Func}) ->
  lists:all(fun is_arg/1, Args) andalso is_func(Func).

is_arg(A)      when is_atom(A) -> true;
is_arg({A, _}) when is_atom(A) -> true;
is_arg(_)                      -> false.

is_func(Fun)       when is_function(Fun)                     -> true;
is_func({M, F})    when is_atom(M) , is_atom(F)              -> true;
is_func({M, F, A}) when is_atom(M) , is_atom(F) , is_list(A) -> true;
is_func(_)                                                   -> false.


%% Graph.
spec2graph(Spec) -> {nodes(Spec), edges(Spec)}.

nodes(Spec) ->
  eon:map(fun(Label, {Args, Func}) ->
            #node{label=Label, args=Args, func=Func}
          end, Spec).

edges(Spec) ->
  eon:fold(fun(Label, {Args, _Func}, Acc) ->
             [{Arg, Label} || Arg <- Args] ++ Acc
           end, [], Spec).

leaves(Edges) ->
  [N || N <- lists:usort(lists:flatten([[A, B] || {A, B} <- Edges])),
        [N_ || {N_, N} <- Edges] =:= []].


kahn_sort({Nodes, Edges}) ->
  lists:flatmap(
    fun(N) ->
      case lists:keyfind(N, #node.label, Nodes) of
        false -> [];
        Node  -> [Node]
      end
    end, kahn_sort(leaves(Edges), Edges, [])).

kahn_sort([], [], L) ->
  L;
kahn_sort([], [_|_], _L) ->
  throw({error, cyclic});
kahn_sort([N|S], Edges0, L) ->
  Edges = Edges0 -- [E || {N, _M} = E <- Edges]
  kahn_sort(S ++ leaves(Edges), Edges, [N|L]).


%% Evaluation.
do_compile(Nodes) ->
  fun(Input) ->
    s2_maybe:reduce(
      fun(#node{label=L, args=A, func=F}, Acc) ->
        eon:set(Acc, Label, call(F, [eon:get(Acc, Arg) || Arg <- Args]))
      end, Input, Nodes)
  end.

call(F, A) when is_function(F) -> F(A);
call({M, F},     A)            -> apply(M, F, A);
call({M, F, A1}, A2)           -> apply(M, F, A1 ++ A2).

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

stats_greph_test() ->
  Greph =
    compile(
      [ n,  {[xs],    fun erlang:length/1}
      , m,  {[xs, n], fun(Xs, N) -> lists:sum(Xs) / N end}
      , m2, {[xs, n], fun(Xs, N) -> lists:sum([X * X || X <- Xs]) / N end}
      , v,  {[m, m2], fun(M, M2) -> M2 - (M * M) end}
      ]),
  {ok, Res} = Greph([xs, [1, 2, 3, 6]]),
  4 = eon:get_(Res, n),
  3 = eon:get_(Res, m),
  3 = eon:get_(Res, m2), %25/2
  3 = eon:get_(Res, v),  %7/2
  ok.

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
