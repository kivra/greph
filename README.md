Overview
========

Usage
=====
```erlang
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
  3.5  = eon:get_(Res, v).

opts_greph_test() ->
  {ok, Greph} =
    compile(
      [ n,  {[xs],    fun erlang:length/1}
      , m,  {[xs, n], fun(Xs, N) -> lists:sum(Xs) / N end}
      , m2, {[xs, n], fun(Xs, N) -> lists:sum([X * X || X <- Xs]) / N end}
      , v,  {[m, m2], fun(M, M2) -> M2 - (M * M) end}
      ],
      [ {pre_fun, fun(A) -> {pre_args, A} end}
      , {post_fun, fun(pre_args, Ret) -> Ret end}]),
  {ok, Res} = Greph([xs, [1, 2, 3, 6]]),
  4    = eon:get_(Res, n),
  3.0  = eon:get_(Res, m),
  12.5 = eon:get_(Res, m2),
  3.5  = eon:get_(Res, v),
  ok.
```

Manifest
========
