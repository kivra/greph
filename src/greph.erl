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
-compile({no_auto_import, [nodes/1]}).

%%%_* Exports ==========================================================
-export([compile/1]).
-export([compile/2]).

-export_type([]).

%%%_* Includes =========================================================
-include("greph.hrl").

%%%_* Code =============================================================
%%%_ * Types -----------------------------------------------------------
-type graphspec() :: eon:object(atom(), funspec()).       %
-type opts()      :: proplists:proplist().                %
-type funspec()   :: {[arg()], func()}.                   %
-type arg()       :: atom()                               %
                   | {atom(), _}.                         %Default
-type func()      :: fun()                                %
                   | {atom(), atom()}                     %Mod:Fun
                   | mfa().                               %
-type graphfun()  :: fun((eon:object()) -> maybe(eon:object(), _)). %

-record(node,
        { label
        , args
        , func
        }).

%%%_ * API -------------------------------------------------------------
-spec compile(graphspec()) -> maybe(graphfun(), _).
compile(Spec) ->
    compile(Spec, []).

-spec compile(graphspec(), opts()) -> maybe(graphfun(), _).
compile(Spec, Opts) ->
  ?do(?thunk({Spec, Opts}),
      fun check/1,
      fun spec2graph/1,
      fun kahn_sort/1,
      fun do_compile/1
      ).

%%%_* Private functions ================================================
%% Input.
check({Spec, Opts}) ->
  {eon:new(Spec, fun(K, V) -> is_atom(K) andalso is_funspec(V) end), Opts}.

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
spec2graph({Spec, Opts}) -> {{snodes(Spec), edges(Spec)}, Opts}.

snodes(Spec) ->
  eon:map(fun(Label, {Args, Func}) ->
            #node{label=Label, args=Args, func=Func}
          end, Spec).

edges(Spec) ->
  eon:fold(fun(Label, {Args, _Func}, Acc) ->
             [{Arg, Label} || Arg <- Args] ++ Acc
           end, [], Spec).

leaves(Nodes, Edges) ->
  [N || N <- Nodes, [N1 || {N1, N2} <- Edges, N2 =:= N] =:= []].

nodes(Edges) -> lists:usort(lists:flatten([[A, B] || {A, B} <- Edges])).

kahn_sort({{Nodes, Edges}, Opts}) ->
  { lists:flatmap(
      fun(N) ->
        case eon:get(Nodes, N) of
          {ok, Node}        -> [Node];
          {error, notfound} -> []
        end
      end, lists:reverse(kahn_sort(leaves(nodes(Edges), Edges), Edges, [])))
  , Opts }.

kahn_sort([], [], L) ->
  L;
kahn_sort([], [_|_], _L) ->
  throw({error, cyclic});
kahn_sort([N|S], Edges0, L) ->
  Edges = Edges0 -- [E || {Node, _M} = E <- Edges0, Node =:= N],
  kahn_sort(S ++ leaves(nodes(Edges0), Edges), Edges, [N|L]).


%% Evaluation.
do_compile({Nodes, Opts}) ->
  fun(Input) ->
    s2_maybe:reduce(
      fun(#node{label=L, args=A, func=F}, Acc) ->
          case eon:get(Acc, L) of
              {error, notfound} ->
                % Label doesnt exists, evaluate and set
                Args = eval_opts(get_args(Acc, A), Opts, pre_fun),
                Res  = eval_opts(eval(L, F, Args), Opts, post_fun),
                eon:set(Acc, L, Res);
              _                 ->
                % Label has either been generated or is part of the input/Acc
                Acc
          end
      end, Input, Nodes)
  end.

get_args(Obj, As) ->
  [case eon:get(Obj, A) of
     {ok, Res} ->
       Res;
     {error, notfound} = Err ->
       ?info("missing ~p", [A]),
       throw(Err)
   end || A <- As].

eval(Label, F, Args) ->
  case ?lift(?time(Label, call(F, Args))) of
    {ok, Res} ->
      Res;
    {error, Rsn} = Err ->
      ?info("~p: failed with ~p", [Label, Rsn]),
      throw(Err)
  end.

eval_opts(Args, Opts, Key) ->
  case eon:get(eon:new(Opts), Key) of
    {error, notfound} -> Args;
    {ok, F}           -> call(F, [Args])
  end.

call(F, A) when is_function(F) -> apply(F, A);
call({M, F},     A)            -> apply(M, F, A);
call({M, F, A1}, A2)           -> apply(M, F, A1 ++ A2).

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

stats_greph_test() ->
  {ok, Greph} =
    compile(
      [ n,  {[xs],    fun erlang:length/1}
      , m,  {[xs, n], fun(Xs, N) -> lists:sum(Xs) / N end}
      , m2, {[xs, n], fun(Xs, N) -> lists:sum([X * X || X <- Xs]) / N end}
      , v,  {[m, m2], fun(M, M2) -> M2 - (M * M) end}
      ]),
  {ok, Res} = Greph([xs, [1, 2, 3, 6]]),
  4    = eon:get_(Res, n),
  3.0  = eon:get_(Res, m),
  12.5 = eon:get_(Res, m2),
  3.5  = eon:get_(Res, v),
  ok.

opts_greph_test() ->
  {ok, Greph} =
    compile(
      [ n,  {[xs],    fun erlang:length/1}
      , m,  {[xs, n], fun(Xs, N) -> lists:sum(Xs) / N end}
      , m2, {[xs, n], fun(Xs, N) -> lists:sum([X * X || X <- Xs]) / N end}
      , v,  {[m, m2], fun(M, M2) -> M2 - (M * M) end}
      ],
      [ {pre_fun, fun(A) -> A end}
      , {post_fun, fun(Ret) -> Ret end}]),
  {ok, Res} = Greph([xs, [1, 2, 3, 6]]),
  4    = eon:get_(Res, n),
  3.0  = eon:get_(Res, m),
  12.5 = eon:get_(Res, m2),
  3.5  = eon:get_(Res, v),
  ok.
-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
