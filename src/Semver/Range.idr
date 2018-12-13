module Semver.Range

import Lightyear
import Lightyear.Char
import Lightyear.Strings
import Semver.Interval
import Semver.Version

%access public export

data Range = MkRange Interval Interval

Show Range where
    show (MkRange Unbounded Unbounded) = "any"
    show (MkRange Unbounded b) = show False b
    show (MkRange a Unbounded) = show True a
    show (MkRange a b) = show True a ++ " " ++ show False b

Eq Range where
    (MkRange al au) == (MkRange bl bu) = al == bl && au == bu

Ord Range where
    compare (MkRange al au) (MkRange bl bu) =
        compare True al bl `thenCompare` compare False au bu

rangeCmp : Interval -> Interval -> Maybe Range
rangeCmp a b = if not $ compare True a b == GT
    then Just $ MkRange a b
    else Nothing

range : Interval -> Interval -> Maybe Range
range Unbounded b = Just $ MkRange Unbounded b
range a Unbounded = Just $ MkRange a Unbounded
range a@(Open av ap) b@(Closed bv bp) = if av == bv
    then Nothing
    else rangeCmp a b
range a@(Closed av ap) b@(Open bv bp) = if av == bv && not (ap && bp)
    then Nothing
    else rangeCmp a b
range a@(Open av ap) b@(Open bv bp) = if av == bv
    then Nothing
    else rangeCmp a b
range a b = rangeCmp a b

upper : Range -> Interval
upper (MkRange _ u) = u

lower : Range -> Interval
lower (MkRange l _) = l

satisfied : Range -> Version -> Bool
satisfied r v = satisLower (lower r) && satisUpper (upper r) && (not (isPre v || upperPreOk (upper r)))
    where
        satisLower : Interval -> Bool
        satisLower (Open l False) = v > l
        satisLower (Closed l False) = v >= l
        satisLower (Open l True) = v > l
        satisLower (Closed l True) = (major v, minor v, patch v) >= (major l, minor l, patch l)
        satisLower Unbounded = True

        satisUpper : Interval -> Bool
        satisUpper (Open u _) = v < u
        satisUpper (Closed u _) = v <= u
        satisUpper Unbounded = True

        upperPreOk : Interval -> Bool
        upperPreOk (Open u p) = isPre u || p
        upperPreOk _ = True

intersect : Range -> Range -> Maybe Range
intersect (MkRange al au) (MkRange bl bu) = range (max True al bl) (min False au bu)

namespace Parse
    incCaret : Version -> Bool -> Bool -> Version
    incCaret v m p = if major v > 0 || ((not m) && (not p))
        then incMajor v
        else if minor v > 0
        then incMinor v
        else incPatch v

    incTilde : Version -> Bool -> Bool -> Version
    incTilde v m p = if (not m) && (not p)
        then incMajor v
        else incMinor v

    caret : Parser $ Maybe Range 
    caret = do
        opt $ char '^'
        (version, m, p) <- bareVersion
        pure $ range (Closed version False) (Open (incCaret version m p) False)

    tilde : Parser $ Maybe Range
    tilde = do
        char '~'
        (version, m, p) <- bareVersion
        pure $ range (Closed version False) (Open (incTilde version m p) False)

    any : Parser $ Maybe Range
    any = do
        string "any"
        pure $ range Unbounded Unbounded

    interval : Parser $ Maybe Range
    interval = do
        lower <- maybe Unbounded id <$> (opt $ do
            token ">"
            eq <- map isJust $ opt $ token "="
            inf <- map isJust $ opt $ token "!"
            (vers, _, _) <- bareVersion
            let inf = inf && (not $ isPre vers)
            pure $ if eq then Closed vers inf else Open vers inf)
        spaces
        upper <- maybe Unbounded id <$> (opt $ do
            token "<"
            eq <- map isJust $ opt $ token "="
            inf <- map isJust $ opt $ token "!"
            (vers, _, _) <- bareVersion
            let inf = inf && (not $ isPre vers)
            pure $ if eq then Closed vers inf else Open vers inf)
        pure $ if lower == Unbounded && upper == Unbounded 
            then Nothing
            else range lower upper

    range : Parser $ Maybe Range
    range = caret <|> tilde <|> any <|> interval