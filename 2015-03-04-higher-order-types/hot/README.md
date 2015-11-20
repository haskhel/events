# Type magic

[Helsinki Haskell User Group](http://www.meetup.com/Helsinki-Haskell-Users-Group/) &mdash; [2015-03-04](http://www.meetup.com/Helsinki-Haskell-Users-Group/events/220347373/)

[@phadej](https://twitter.com/phadej) &ndash; Oleg Grenrus &ndash; [http://oleg.fi](http://oleg.fi)

These slides are available at:

- [http://oleg.fi/slides/helhug-types/](http://oleg.fi/slides/helhug-types/)
- and [GitHub](https://github.com/phadej/helhug-types)

---

![foo](https://igcdn-photos-g-a.akamaihd.net/hphotos-ak-xfa1/t51.2885-15/11017568_893787943985846_982141368_n.jpg)

Näin käy kun leikit vahvojen tyypien kanssa.

---

## Why types

- They make invalid states unrepresentable
    - But some constraints are really hard to encode
- They carry some of the information on the type level
    - But please, don't try to put *everything* there

With expressive type system we could:

- Make the *most* of invalid states unrepresentable
- Carry all *static* information on the type level
   - Maybe even some *quasi-static*<sup>&dagger;</sup> information.

<small><sup>&dagger;</sup> quasi-static: my own term, something that doesn't change but isn't known at compile-time. e.g. configuration.</small>

---

### Type-level information

For example phantom types:

```hs
data USDollar
data Euro
newtype Money c = Money Rational deriving (Eq)

-- >>> (Money 100 :: Money USDollar) == (Money 100 :: Money Euro)
-- Compile time error!
-- Couldn't match type ‘Euro’ with ‘USDollar’
```

---

### No type-level information :(

```js
// >>> _.isEqual({ currency: "usdollar", amount: 100},
//               { currency: "euro", amount: 100});
// false

function moneyIsEqual(a, b) {
  if (a.currency !== b.currency)
    throw new Error("different currencies");
  return a.amount === b.amount;
}
// >>> moneyIsEqual({ currency: "usdollar", amount: 100},
//                  { currency: "euro", amount: 100});
// Run-time error!
// Uncaught Error:
```

```hs
data Money = Money Rational Currency
```

Hopefully [no-one](https://github.com/paypal/PayPal-node-SDK) handles money in JavaScript.

---

## Functional programming

Abstraction method: **function**

We have different functions:

- from values to values (&#8727;, &#8727;) &mdash; functions
- from types to values  (&#9744;, &#8727;) &mdash; type classes, singletons
- from types to types   (&#9744;, &#9744;) &mdash; type families
- from values to types  (&#8727;, &#9744;) &mdash; magic tricks

They all are still functions, yet they look different.

---

## The real world problem

> Working with entity database / api.

### Entity database

Easiest to compare with document database

- Document database: *entityId* &#8658; *json-ish blob*
- Entity database: *entityId* &times; *attributeName* &#8658; *primitive*, where *primitive* could be e.g:
    - *int*
    - *string*
    - *collection* of references to other *entities*

In other words: like [datomic](http://www.datomic.com/).

---

## Schema and dataset

We'll use a simple schema:

- Only one entity type: *programming language*
- Fields: *name*, *wikipedia url*, *typing discipline*
- &hellip; and *influenced languages*

Data set is scrapped from [Wikipedia](http://en.wikipedia.org/wiki/ML_%28programming_language%29)

![ml](images/ml.png)

---

## Part 1

Traversing the structure:

```hs
data PL = PL
  { plId         :: EntityId
  , plName       :: String
  , plUrl        :: String
  , plAppearedIn :: Maybe Int
  , plTypingDis  :: [String]
* , plInfluenced :: [EntityId]
  }
  deriving (Eq, Ord, Show, Read)
```

---

## Part 1: DBMonad

Using *i-can-do-anything monad* `DBMonad`:

```hs
askEntity :: EntityId -> DBMonad Entity

data Entity = Entity
  { entityId    :: EntityId
  , entityAttrs :: Map AttrName AttrValue
  }
```

In the real world running `DBMonad` will require `IO`,
so we'll try to avoid it.

---

## Part 1: Demo

Start with [Prologue.hs](https://github.com/phadej/helhug-types/blob/master/src/HelHUG/Prologue.hs)

How to encode "deeper" PL?

- Write special classes for each level, using typeclasses to abstract over?
    - Lot's of code expected

- Parametrising over childs:
    - `PL = PLF (Reference PL)`
    - `PL' = PLF PL`
    - `PL'' = PLF PL'` &hellip;

---

## Part 1: Demo cont.

- Our approach: `PL (n :: Nat)`, where `n` tells how deep we can recurse into the structure.
- [Part1.hs](https://github.com/phadej/helhug-types/blob/master/src/HelHUG/Part1.hs) has the original version
- [Part1b.hs](https://github.com/phadej/helhug-types/blob/master/src/HelHUG/Part1b.hs) is cleened up and uses `DataKinds`

```hs
data PL n = PL
  { plId :: EntityId
  , plName :: String
  , plUrl :: String
  , plAppearedIn :: Maybe Int
  , plTypingDis  :: [String]
* , plInfluenced :: [Ref n PL]
  }
```

---

## Part 1: Outcome

Now we can write functions without hardcoding "unwrapping-level" of the entities.

This is handy, as we could separate data fetching (IO) from rendering (would-like-it-to-be-pure):

```hs
type Inc3 n = Succ (Succ (Succ n))

fetchPage :: Request -> SuperMonad (Page Zero)
glue :: Page Zero -> SuperMonad (Page Three)
renderPage :: Page (Inc3 n) -> Html

handle :: Request -> IO Html
handle req = runSuperMonad (fetchPage req >>= glue <&> renderPage)
```

- `glue` is provided by the library.
- BTW: the same can be done in [C++, Scala or (obviously) Agda](https://gist.github.com/phadej/bdc67048df66a574df5a)

---

## Part 2

Magic tricks by *singletons* library:
[hackage](https://hackage.haskell.org/package/singletons),
[paper.pdf](http://www.cis.upenn.edu/~eir/papers/2012/singletons/paper.pdf).

```hs
-- | Peano numbers. Promotable data.
*data Nat = Zero | Succ Nat

-- | Singleton type for `Nat`
data SNat :: Nat -> * where
  SZero :: SNat 'Zero
  SSucc :: forall (n :: Nat). SNat n -> SNat ('Succ n)

-- | Implicit construction of `SNat` from type-level `Nat`.
class INat (n :: Nat) where
  snat :: SNat n

instance INat 'Zero where
  snat = SZero

instance INat n => INat (Succ n) where
  snat = SSucc snat
```

---

## Part 2: magic tricks

We can promote (promotable) values to type-level in "run-time".

> from values to types  (&#8727;, &#9744;) &mdash; magic tricks

```hs
data SomeSNat :: KProxy Nat -> * where
  SomeSNat :: SNat (n :: Nat) -> SomeSNat ('KProxy :: KProxy Nat)

toSNat :: kparam ~ 'KProxy => Nat -> SomeSNat (kparam :: KProxy Nat)
toSNat Zero = SomeSNat SZero
toSNat (Succ n) = case toSNat n of
                    SomeSNat sn -> SomeSNat (SSucc sn)

-- Existentiality ensures this is actually safe,
-- i.e. makes sense to be able to do
*withSNat :: Nat -> (forall (n :: Nat). SNat n -> b) -> b
withSNat n f = case toSNat n of
                 SomeSNat sn -> f sn
```

- [Terse explanation SomeSNat works](https://github.com/goldfirere/singletons/#definitions-used-to-support-singletons)
- [GHC.TypeLits](https://hackage.haskell.org/package/base-4.7.0.2/docs/GHC-TypeLits.html) [uses magic](https://github.com/ghc/ghc/blob/f66e0e695b0377c469fbe877d4850fc0ebca2010/compiler/basicTypes/MkId.hs#L1279)

---

## Part 3

The problem is, that approach in the first part has still too much boilerplate in the user-land.

We generalised over *unwrapping-level*
Yet there are other dimensions:
- other entities, we have only one
    - e.g. typing discipline could be turned into entity itself
- other operations, we have only unwrapping
    - e.g. `Eq` needs `UndecidableInstances` now, we could generate it ourselves.

---

## Part 3: Demo

Idea:

```hs
data R { i :: Int, s :: String }
-- is isomorphic to
type RHList = HList '[Int, String]
```

- `IsRecord` type-class provides method to transform entity data types to the isomorphic `HList`
- *Note:* our `HList` has a twist:
    - it's kind is: `Nat -> [Either * (Nat -> *)] -> *`
    - `Left` is concrete data, `Right` is unwrappable

We can write our operations generically on `HList`s!


---

## Part 3: Outcome

Our `HList` is tailored-for-our-purpose way to write generic code.  It's
essentially what [shapeless](https://github.com/milessabin/shapeless) is about
in the Scala world.

Haskell has e.g. [syb](https://hackage.haskell.org/package/syb) and
[GHC.Generics](http://hackage.haskell.org/package/base-4.7.0.2/docs/GHC-Generics.html).
They doesn't work as our primary source of truth is `Spec`, not the *data
definition*. Latter doesn't contain all of the necessary information: which
data we would like to unwrap.

---

## Part 3: Code lines

Code sec. | No HList | No HList with wrap | [HList](https://github.com/phadej/helhug-types/blob/master/src/HelHUG/Part3.hs) | [HList with wrap](https://github.com/phadej/helhug-types/blob/master/src/HelHUG/Part3b.hs)
----------|----------|--------------------|---------|-------------------
Library   | 24       | exercise          | 89      | 105
User      | 31 (23)  | exercise          | 35 (19) | 36 (20)

- Less user-land code (code lines in parenthesis)
    - Also non-trivial parts are simpler (?)
- Only one additional line in the user-land code for a new operation, when using HList approach.
    - 

---

## Questions?

- Why not everything in Template Haskell?
    - Its error message is even harder to debug

- Error messages?
   - [Could get kudos - Haskell-cafe: Domain specific error messages](https://mail.haskell.org/pipermail/haskell-cafe/2015-February/118031.html)
   - in future we could write (GHC) plugins to get better error messages!


