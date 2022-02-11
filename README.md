# LinLam: a Haskell library for experimental linear lambda calculus

A collection of Haskell routines for generating, normalizing, typing, diagrammifying, and otherwise playing with linear lambda terms.

# Installation

```shell
cabal configure
cabal build
cabal install --lib
```

The library has a few dependencies (including [data-memocombinators](https://hackage.haskell.org/package/data-memocombinators) for memoized term generation, [diagrams](https://hackage.haskell.org/package/diagrams) for diagram generation, and [combinat](https://hackage.haskell.org/package/combinat) for permutation generation).
These dependencies should be (hopefully!) automatically resolved by cabal.

# Documentation

Not much documentation on the library for now!
However, you can see the example sessions below for illustrations of how to use the library to do experimental lambda calculus.
Also, please have a look in the [experiments](experiments) directory, which contains some standalone compilable experiments.

For more background, you may also have a look at some of the following papers:

* [Asymptotics and random sampling for BCI and BCK lambda terms](https://dmg.tuwien.ac.at/dgardy/Papers/LogiqueQuantitative/BCI.pdf) by O. Bodini, D. Gardy, and A. Jacquot
* [Linear lambda terms as invariants of rooted trivalent maps](https://arxiv.org/abs/1512.06751) by N. Zeilberger
* [A theory of linear typings as flows on 3-valent graphs](https://arxiv.org/abs/1804.10540) by N. Zeilberger

# Example sessions

## Enumerating some terms, printing and normalizing them

```haskell
Prelude> import LinLam
Prelude LinLam> allLT 5 0
[A (L 0 (V 0)) (L 1 (V 1)),L 0 (A (V 0) (L 1 (V 1))),L 1 (A (L 0 (V 0)) (V 1)),L 0 (L 1 (A (V 0) (V 1))),L 1 (L 0 (A (V 0) (V 1)))]
Prelude LinLam> printLTs (allLT 5 0)
(\a.a)(\b.b)
\a.a(\b.b)
\b.(\a.a)(b)
\a.\b.a(b)
\b.\a.a(b)
Prelude LinLam> printLTs (allPT 5 0)
(\a.a)(\b.b)
\a.a(\b.b)
\b.(\a.a)(b)
\a.\b.a(b)
Prelude LinLam> printLTs (allBPT 7 1)
\b.a(\c.b(c))
\c.(\b.a(b))(c)
\b.\c.a(b(c))
\b.\c.a(b)(c)
Prelude LinLam> [length (allLT (3*n+2) 0) | n <- [0..4]]
[1,5,60,1105,27120]
Prelude LinLam> [length (allPT (3*n+2) 0) | n <- [0..6]]
[1,4,32,336,4096,54912,786432]
Prelude LinLam> [length (allBPT (3*n+1) 1) | n <- [0..6]]
[1,1,4,24,176,1456,13056]
Prelude LinLam> normalize (L 0 $ A (L 1 $ V 1) (V 0))
L 0 (V 0)
Prelude LinLam> printLTs $ filter (betaEq (L 0 $ V 0)) (allLT 5 0)
(\a.a)(\b.b)
\b.(\a.a)(b)
Prelude LinLam> 
```

## Generating a random closed term and making some observations, or running a repeated experiment to generate a histogram

```haskell
Prelude LinLam> t <- randomLT (3*100+2)
Prelude LinLam> printLT t
\a.\b.\c.\d.\e.\f.\g.\h.\i.\j.\k.\l.\m.\n.\o.\p.\q.\r.\s.\t.\u.(\v.\w.\x.\y.\z.\X0.\X1.\X2.\X3.\X4.\X5.\X6.\X7.\X8.\X9.X6(\X10.\X11.\X12.e(p(\X13.\X14.\X15.\X16.\X17.(\X18.\X19.\X20.j(\X21.\X22.\X23.\X24.\X25.\X26.\X27.\X28.\X29.\X30.\X31.\X32.\X33.\X34.g((\X35.X33(\X36.\X37.\X38.\X39.\X40.\X41.\X42.\X43.d(\X44.\X45.\X46.(\X47.\X48.\X49.X32(r)(\X50.X41(X20(\X51.\X52.\X53.(\X54.\X55.\X56.X23(\X57.\X58.\X59.\X60.(\X61.X61(v))(X42)(X60(n))(X36(X58)(X55)(o)(\X62.X13(\X63.\X64.y(X38(X49(X57)(X26(X48)(h(\X65.X5(X34)(a(\X66.X43(X29(X10))(q)(X65)((\X67.(\X68.w(X37)(c(X56(X3(X15))(X18)(X67)(X39((\X69.(\X70.\X71.X59(X63((\X72.\X73.(\X74.X35(X74)(X68))(X54)(X73(X44)(X22(X19))(X72)(X27(X0))(X47)))(X52(X50)(m)(X25)(f(X2(X4)(X69)(k)(x(X45(z))(X21)(X24)))(X62(X46(X1)))(X12))(X70))))(X8(X71)))(i(X53)))(u))))))(X64))(X14(X66)))))))))(X16(X51)))))(X9))(X31)(X40)))(l)(X11)))))(t(s(X30))))(X28)))(X17))))(X7)))))(b)
Prelude LinLam> size (normalize t)
263
Prelude LinLam> size t - size (normalize t)
39
Prelude LinLam> histogram <$> experimentLT (\t -> size t - size(normalize t)) 302 100
[(18,1),(21,2),(24,5),(27,3),(30,1),(33,7),(36,8),(39,14),(42,11),(45,7),(48,11),(51,6),(54,11),(57,5),(60,2),(63,1),(66,2),(69,1),(75,1),(78,1)]
```

## Type inference

```haskell
Prelude LinLam> mapM_ (\t -> putStrLn (prettyLT t ++ " : " ++ prettyType (synthClosed t))) (allNLT 8 0)
\a.a(\b.b(\c.c)) : (((((γ -> γ) -> β) -> β) -> α) -> α)
\a.a(\b.\c.b(c)) : ((((γ -> β) -> (γ -> β)) -> α) -> α)
\a.a(\c.\b.b(c)) : (((γ -> ((γ -> β) -> β)) -> α) -> α)
\a.a(\b.b)(\c.c) : (((γ -> γ) -> ((β -> β) -> α)) -> α)
\a.\b.a(b(\c.c)) : ((β -> α) -> (((γ -> γ) -> β) -> α))
\b.\a.a(b(\c.c)) : (((γ -> γ) -> β) -> ((β -> α) -> α))
\a.\b.a(\c.b(c)) : (((γ -> β) -> α) -> ((γ -> β) -> α))
\b.\a.a(\c.b(c)) : ((γ -> β) -> (((γ -> β) -> α) -> α))
\a.\c.a(\b.b(c)) : ((((γ -> β) -> β) -> α) -> (γ -> α))
\c.\a.a(\b.b(c)) : (γ -> ((((γ -> β) -> β) -> α) -> α))
\a.\b.a(b)(\c.c) : ((γ -> ((β -> β) -> α)) -> (γ -> α))
\b.\a.a(b)(\c.c) : (γ -> ((γ -> ((β -> β) -> α)) -> α))
\a.\c.a(\b.b)(c) : (((γ -> γ) -> (β -> α)) -> (β -> α))
\c.\a.a(\b.b)(c) : (β -> (((γ -> γ) -> (β -> α)) -> α))
\a.\b.\c.a(b(c)) : ((β -> α) -> ((γ -> β) -> (γ -> α)))
\b.\a.\c.a(b(c)) : ((γ -> β) -> ((β -> α) -> (γ -> α)))
\a.\c.\b.a(b(c)) : ((β -> α) -> (γ -> ((γ -> β) -> α)))
\c.\a.\b.a(b(c)) : (γ -> ((β -> α) -> ((γ -> β) -> α)))
\b.\c.\a.a(b(c)) : ((γ -> β) -> (γ -> ((β -> α) -> α)))
\c.\b.\a.a(b(c)) : (γ -> ((γ -> β) -> ((β -> α) -> α)))
\a.\b.\c.a(b)(c) : ((γ -> (β -> α)) -> (γ -> (β -> α)))
\b.\a.\c.a(b)(c) : (γ -> ((γ -> (β -> α)) -> (β -> α)))
\a.\c.\b.a(b)(c) : ((γ -> (β -> α)) -> (β -> (γ -> α)))
\c.\a.\b.a(b)(c) : (β -> ((γ -> (β -> α)) -> (γ -> α)))
\b.\c.\a.a(b)(c) : (γ -> (β -> ((γ -> (β -> α)) -> α)))
\c.\b.\a.a(b)(c) : (β -> (γ -> ((γ -> (β -> α)) -> α)))
```

## Making diagrams

Generate a table of string diagrams (λ-graphs) representing the term structure of all closed planar terms of size 8 (in a file named `diagrams/pt8,0.svg`):

```haskell
Prelude LinLam> renderLTs' (allPT 8 0) "diagrams/pt8,0"
```
![pt8,0](diagrams/pt8,0.svg)

Generate a table of string diagrams (proof-nets) representing the type structure of all one-variable-open normal bridgeless terms of size 7:

```haskell
Prelude LinLam> trenderNLTs' (allNBLT 7 1) "diagrams/nblt7,1"
```
![nblt7,1](diagrams/nblt7,1.svg)

Generate a random one-variable-open bridgeless term of size 451, normalize it, and diagram its type structure (just the pure graphical diagram, without any Greek annotations):

```haskell
Prelude LinLam> t <- randomBLT (3*150+1)
Prelude LinLam> trenderNLT (normalize t) "diagrams/randomnlt"
```
![randomnlt](diagrams/randomnlt.svg)

# Some related tools

* George Kaye's [λ-term visualiser](https://www.georgejkaye.com/lamviz/) and [gallery](https://www.georgejkaye.com/lamviz/gallery)
* Jason Reed's [Interactive Lambda Maps Toy](https://jcreedcmu.github.io/demo/lambda-map-drawer/public/index.html)

# License

Free to use under an [MIT License](LICENSE).
