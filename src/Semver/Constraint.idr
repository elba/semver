module Semver.Constraint

import Lightyear
import Lightyear.Char
import Lightyear.Strings
import Semver.Version
import Semver.Interval
import Semver.Range
import Data.AVL.Set

%access public export

data Relation = Superset | Subset | Overlapping | Disjoint | Equal

data Constraint = MkConstraint (Set Range)

Semigroup Constraint where
    (MkConstraint a) <+> (MkConstraint b) = MkConstraint $ union a b

Monoid Constraint where
    neutral = MkConstraint empty

unify : Constraint -> Constraint
unify (MkConstraint s) = MkConstraint . Set.fromList $ go st []
    where
        st : List Range
        st = sortBy (\x, y => compare True (lower x) (lower y)) $ Set.toList s

        go : List Range -> List Range -> List Range
        go [] ys = reverse ys
        go (x::xs) [] = go xs [x] 
        go (b::xs) (a::ys) = case compare False (upper a) (lower b) of
            GT => go xs ((MkRange (lower a) (upper b))::ys)
            EQ => if (isOpen $ upper a) && (isOpen $ lower b)
                then go xs (b::a::ys)
                else go xs ((MkRange (lower a) (upper b))::ys)
            LT => case (upper a, lower b) of
                (Open v vp, Closed w wp) => if v == w
                    then go xs ((MkRange (lower a) (upper b))::ys)
                    else go xs (b::a::ys) 
                _ => go xs (b::a::ys)


Cast Range Constraint where
    cast r = MkConstraint $ insert r empty

Eq Constraint where
    (MkConstraint a) == (MkConstraint b) = a == b

Cast (Set Range) Constraint where
    cast = unify . MkConstraint

Cast (List Range) Constraint where
    cast = cast . Set.fromList

any : Constraint
any = cast $ MkRange Unbounded Unbounded

set : Constraint -> Set Range
set (MkConstraint s) = s

Show Constraint where
    show = concat . intersperse ", " . map show . Set.toList . set

insert : Constraint -> Range -> Constraint
insert (MkConstraint s) r = cast $ insert r s

isImpossible : Constraint -> Bool
isImpossible (MkConstraint s) = empty == s

difference : Constraint -> Constraint -> Constraint
difference (MkConstraint a) (MkConstraint b) = cast $ go (Set.toList a) (Set.toList b)
    where
        sub : Range -> List Range -> List Range -> List Range
        sub r [] m = m ++ [r]
        sub r (s::xs) m = case compare True (lower r) (lower s) of
            GT => case compare False (lower r) (upper s) of
                GT => sub r xs m
                EQ => sub (MkRange (if isOpen $ upper s then upper s else flip $ upper s) (upper r)) xs m
                LT => if (upper s) == Unbounded
                    then m
                    else case range (flip $ upper s) (upper r) of
                        Just r => sub r xs m
                        Nothing => m
            LT => case compare False (upper r) (lower s) of
                LT => sub r xs m
                EQ => sub (MkRange (lower r) $ case upper r of
                    Closed a ap => Open a ap
                    x => x) xs m
                GT => if compare False (upper r) (upper s) /= GT
                    then sub (MkRange (lower r) (flip $ lower s)) xs m
                    else sub (MkRange (flip $ upper s) (upper r)) xs ((MkRange (lower r) (flip $ lower s))::m)
            EQ => if (upper s) == (upper r) then m
                else if (upper s) == Unbounded then m
                else case range (flip $ upper s) (upper r) of
                    Just r => sub r xs m
                    Nothing => m

        go : List Range -> List Range -> List Range
        go xs ys = join $ map (\x => sub x ys []) xs 

intersect : Constraint -> Constraint -> Constraint
intersect a b = difference a (difference a b)

union : Constraint -> Constraint -> Constraint
union (MkConstraint a) (MkConstraint b) = cast $ union a b

complement : Constraint -> Constraint
complement = difference any

relation : Constraint -> Constraint -> Relation
relation a b = if i == b && i == a
    then Equal
    else if i == b
    then Superset
    else if i == a
    then Subset
    else if i == neutral
    then Disjoint
    else Overlapping
    where
        i : Constraint
        i = intersect a b

namespace Parse
    constraint : Parser $ Maybe Constraint
    constraint = do
        ls <- sepBy range comma
        pure $ cast <$> Set.fromList <$> sequence ls