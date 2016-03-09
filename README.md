# Haskell fun
Miscellaneous Haskell explorations.

**Bold** means polished!

<br/>
<br/>

#### Math-ish
- [Peano numbers] (./Peano.hs)
- *TODO*: [Cartesian Closed] (./CCC.hs)
– *TODO*: [Tensor] (./Tensor.hs)

#### Semantics
- [Abstract Machines via defunctionalization] (./Defunctionalization.hs)
- [CEK Machine for the untyped λ-calculus] (./SmallSteps.hs)

#### Types!
- **[Row Polymorphism] (./RowPoly.hs)**
- [F-algebras and inductive types via μ] (./Mu.hs)
- *TODO*: [Cata-, Ana-, Hylo-, Para- ...] (./RecSchemes.hs)


#### Patterns
- [sketches of an FRP] (./ElmFrp.hs)
- ["Middleware"] (./Middleware.hs)

#### Misc
- *TODO*: [Datalog] (./Datalog.hs)
- *TODO*: [Prolog] (./Prolog.hs)


<br/>
<br/>


### instructions:
1. install [nix](http://nixos.org/nix) and [stack](http://docs.haskellstack.org/en/stable/README/)
2. `stack build` to download deps
3. `nix-shell --run "ghci RowPoly.hs" to play with a file
