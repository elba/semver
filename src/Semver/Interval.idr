module Semver.Interval

import Semver.Version

%access public export

data Interval = Closed Version Bool | Open Version Bool | Unbounded

Eq Interval where
    Unbounded == Unbounded = True
    (Closed a ap) == (Closed b bp) = a == b && ap == bp
    (Open a ap) == (Open b bp) = a == b && ap == bp
    _ == _ = False

compare : (lower : Bool) -> Interval -> Interval -> Ordering
compare _ Unbounded Unbounded = EQ
compare lower Unbounded _ = if lower then LT else GT
compare lower _ Unbounded = if lower then GT else LT
compare lower (Open a ap) (Open b bp) = if a == b
    then if lower then EQ else ap `compare` bp
    else a `compare` b
compare lower (Closed a ap) (Closed b bp) = if a == b
    then if lower then bp `compare` ap else ap `compare` bp
    else a `compare` b
compare lower (Open a _) (Closed b _) = if a == b
    then if lower then GT else LT
    else a `compare` b
compare lower (Closed a _) (Open b _) = if a == b
    then if lower then LT else GT
    else a `compare` b

min : (lower : Bool) -> Interval -> Interval -> Interval
min lower a b = if compare lower b a == LT then b else a

max : (lower : Bool) -> Interval -> Interval -> Interval
max lower a b = if compare lower b a == GT then b else a

flip : Interval -> Interval
flip (Closed v p) = Open v (not p)
flip (Open v p) = Closed v (not p)
flip Unbounded = Unbounded

preOk : Interval -> Bool
preOk (Closed _ p) = p
preOk (Open _ p) = p
preOk Unbounded = True

show : (lower : Bool) -> Interval -> String
show lower Unbounded = ""
show lower (Closed v p) =
    let bang = if (not $ isPre v) && p then "!" else "" in
    if lower
        then ">=" ++ bang ++ " " ++ show v
        else "<= " ++ show v
show lower (Open v p) =
    let bang = if (not $ isPre v) && p then "!" else "" in
    if lower
        then "> " ++ show v
        else "<" ++ bang ++ " " ++ show v

isOpen : Interval -> Bool
isOpen (Open _ _) = True
isOpen _ = False

isClosed : Interval -> Bool
isClosed (Closed _ _) = True
isClosed _ = False
