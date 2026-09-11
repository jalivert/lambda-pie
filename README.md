# lambda-pie

[![CI](https://github.com/jalivert/lambda-pie/actions/workflows/ci.yml/badge.svg)](https://github.com/jalivert/lambda-pie/actions/workflows/ci.yml)

Simply typed lambda calculus λ→, System F λ2, and dependently typed λ-calculus λΠ —
each with a parser and an interactive REPL that typechecks and evaluates as you type.


## Prerequisites

- GHC (CI covers 9.8, 9.10, 9.12)
- `cabal-install`
- `alex` and `happy` (lexer/parser generators, pulled in via `build-tool-depends`)

## Build

```
cabal build all
```

## Test

```
cabal test all
```

## Run

There is one REPL per calculus:

```
cabal run simply
cabal run systemf
cabal run dependently
```

Type `:exit` to quit a REPL. Typing in the REPLs:

- `λ` is typed as `λ`, `\`, or `lambda`
- `Λ` is typed as `/\`
- type arguments in System F are wrapped in brackets, e.g. `[Nat]`
- `assume` introduces a type or a term, e.g. `assume Bool :: *`

### Simply typed lambda calculus (λ→)

```
λ-> >> assume (id :: T -> T) (T :: *) (a :: T) (b :: T)
λ-> >> id a
       (id a) :: T
λ-> >> id b
       (id b) :: T
```

### System F (λ2)

```
λ2 >> (/\ T . (\ (t :: T) -> t))
       <type lambda> :: (forall T . (T -> T))
```

```
λ2 >> assume (Nat :: *) (fst :: forall T . T -> T -> T) (snd :: forall T . T -> T -> T)
λ2 >> assume pickone :: (forall T . T -> T -> T) -> (forall T . T -> T -> T) -> (forall T . T -> T -> T)
λ2 >> pickone fst snd
      ((pickone fst) snd) :: (forall T . (T -> (T -> T)))
```

### Dependently typed lambda calculus (λΠ)

```
λΠ >> (lambda t x -> x) :: (forall (t :: *) . (forall (x :: t) . t))
       (λ t -> (λ x -> x)) :: (Π t :: * . (Π x :: t . t))
```

## Project layout

```
app/          REPL entry points (simply, systemf, dependently)
src/          one module tree per calculus: Simply, SystemF, Dependently
test/         hspec suite (cabal test all)
```
