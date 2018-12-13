module Semver.Version

import Lightyear
import Lightyear.Char
import Lightyear.Strings

%access public export

data Identifier = INum Nat | IStr String

Eq Identifier where
    (INum n) == (INum m) = n == m
    (IStr s) == (IStr t) = s == t
    _ == _ = False

Ord Identifier where
    compare (INum n) (INum m) = compare n m
    compare (IStr s) (IStr t) = compare s t
    compare (INum _) (IStr _) = LT
    compare (IStr _) (INum _) = GT

Show Identifier where
    show (INum n) = show n
    show (IStr s) = s

record Version where
    constructor MkVersion
    major, minor, patch : Nat
    release, metadata : List Identifier

Eq Version where
    (MkVersion ma1 mi1 pa1 rl1 mt1) == (MkVersion ma2 mi2 pa2 rl2 mt2) =
        ma1 == ma2 && mi1 == mi2 && pa1 == pa2 && rl1 == rl2 && mt1 == mt2

Ord Version where
    compare (MkVersion ma1 mi1 pa1 rl1 mt1) (MkVersion ma2 mi2 pa2 rl2 mt2) =
        compare (the (List Nat) [ma1, mi1, pa1]) [ma2, mi2, pa2] `thenCompare` compare (the (List $ List Identifier) [rl1, mt1]) [rl2, mt2]

Show Version where
    show (MkVersion ma mi pa rl mt) =
        show ma ++ "." ++ show mi ++ "." ++ show pa ++ rlText ++ mtText
        where
            dots : Show a => List a -> String
            dots = concat . intersperse "." . map show

            rlText = if isNil rl then "" else "-" ++ dots rl
            mtText = if isNil mt then "" else "+" ++ dots mt

isPre : Version -> Bool
isPre (MkVersion _ _ _ rl _) = not $ isNil rl

incMajor : Version -> Version
incMajor (MkVersion ma _ _ _ _) = MkVersion (ma + 1) 0 0 [] []

incMinor : Version -> Version
incMinor (MkVersion ma mi _ _ _) = MkVersion ma (mi + 1) 0 [] []

incPatch : Version -> Version
incPatch (MkVersion ma mi pa _ _) = MkVersion ma mi (pa + 1) [] []

namespace Parse
    nat : Parser Nat
    nat = integer

    identifier : Parser Identifier
    identifier = (INum <$> integer) <|> (IStr . pack <$> some letter)

    identList : Parser $ List Identifier
    identList = sepBy1 identifier $ char '.'

    bareVersionMajor : Parser (Version, Bool, Bool)
    bareVersionMajor = do
        major <- nat
        pure ((MkVersion major 0 0 [] []), False, False)

    bareVersionMinor : Parser (Version, Bool, Bool)
    bareVersionMinor = do
        major <- nat
        char '.'
        minor <- nat
        pure ((MkVersion major minor 0 [] []), True, False)

    bareVersionFull : Parser (Version, Bool, Bool)
    bareVersionFull = do
        major <- nat
        char '.'
        minor <- nat
        char '.'
        patch <- nat
        release <- maybe List.Nil id <$> (opt $ do
            char '-'
            identList)
        metadata <- maybe List.Nil id <$> (opt $ do
            char '+'
            identList)

        pure ((MkVersion major minor patch release metadata), True, True)

    bareVersion : Parser (Version, Bool, Bool)
    bareVersion = bareVersionFull <|> bareVersionMinor <|> bareVersionMajor