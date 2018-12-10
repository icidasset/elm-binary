module Binary exposing
    ( Bits
    , fromHex, toHex, fromIntegers, toIntegers, fromBooleans, toBooleans
    , and, or, xor, not
    , shiftLeftBy, shiftRightBy, shiftRightZfBy, rotateLeft, rotateRight
    , add
    , ensureBits, dropLeadingZeros, makeIsometric
    )

{-|

@docs Bits


# Convertors

@docs fromHex, toHex, fromIntegers, toIntegers, fromBooleans, toBooleans


# Bitwise Operators

@docs and, or, xor, not


# Bit Shifting

@docs shiftLeftBy, shiftRightBy, shiftRightZfBy, rotateLeft, rotateRight


# Mathematical Operators

@docs add


# Utilities

@docs ensureBits, dropLeadingZeros, makeIsometric

-}

import Dict exposing (Dict)
import List.Extra as List



-- 🌳


{-| **The binary sequence.**

Use convertors to make `Bits`.

    Binary.fromIntegers [ 0, 1, 0, 1 ]

-}
type Bits
    = Bits (List Bool)



-- CONVERTERS


{-| Convert a hex string to list of binary numbers.

    >>> fromHex "15" |> toIntegers
    [ 1, 0, 1, 0, 1 ]

    >>> fromHex "2C0" |> toIntegers
    [ 1, 0, 1, 1, 0, 0, 0, 0, 0, 0 ]

-}
fromHex : String -> Bits
fromHex hex =
    hex
        |> String.toUpper
        |> String.toList
        |> List.map
            (\h ->
                hexToBinaryTable
                    |> Dict.get h
                    |> Maybe.withDefault []
            )
        |> List.concat
        |> Bits
        |> dropLeadingZeros


{-| Convert a list of binary numbers to a hex string.

    >>> toHex <| fromIntegers [ 1, 0, 1, 0, 1 ]
    "15"

    >>> toHex <| fromIntegers [ 1, 0, 1, 1, 0, 0, 0, 0, 0, 0 ]
    "2C0"

-}
toHex : Bits -> String
toHex (Bits bits) =
    let
        missingLeadingZeros =
            4 - modBy 4 (List.length bits)

        bitsWithLeadingZeros =
            if missingLeadingZeros == 0 || missingLeadingZeros == 4 then
                bits

            else
                List.repeat missingLeadingZeros False ++ bits
    in
    bitsWithLeadingZeros
        |> List.greedyGroupsOf 4
        |> List.map
            (\a ->
                hexToBinaryTableList
                    |> List.find (Tuple.second >> (==) a)
                    |> Maybe.map Tuple.first
                    |> Maybe.withDefault '0'
            )
        |> List.dropWhile ((==) '0')
        |> String.fromList


{-| Convert a list of integers to `Bits`.

Everything below zero and zero itself becomes a `0` bit,
and everything above zero becomes a `1` bit.

    >>> fromIntegers [ 1, 0, 0, 0 ] |> toHex
    "8"

-}
fromIntegers : List Int -> Bits
fromIntegers =
    List.map (\i -> ifThenElse (i <= 0) False True) >> Bits


{-| Convert `Bits` to a list of integers.

    >>> toIntegers <| fromHex "8"
    [ 1, 0, 0, 0 ]

-}
toIntegers : Bits -> List Int
toIntegers (Bits bits) =
    List.map (\b -> ifThenElse (b == False) 0 1) bits


{-| Convert a list of booleans to `Bits`.

    >>> fromBooleans [ True, False, False, False ] |> toHex
    "8"

-}
fromBooleans : List Bool -> Bits
fromBooleans =
    Bits


{-| Convert `Bits` to a list of booleans.

    >>> toBooleans <| fromHex "8"
    [ True, False, False, False ]

-}
toBooleans : Bits -> List Bool
toBooleans (Bits bits) =
    bits



-- BITWISE OPERATORS


{-| AND operator.

    --     0101 (decimal 5)
    -- AND 0011 (decimal 3)
    --   = 0001 (decimal 1)

    >>> Binary.and
    ..>   (fromHex "5")
    ..>   (fromHex "3")
    ensureBits 3 (fromHex "1")

-}
and : Bits -> Bits -> Bits
and a b =
    condense
        (\( x, y ) -> x && y)
        (makeIsometric a b)


{-| OR operator.

    --    0101 (decimal 5)
    -- OR 0011 (decimal 3)
    --  = 0111 (decimal 7)

    >>> Binary.or
    ..>   (fromHex "5")
    ..>   (fromHex "3")
    fromHex "7"

-}
or : Bits -> Bits -> Bits
or a b =
    condense
        (\( x, y ) -> x || y)
        (makeIsometric a b)


{-| XOR operator.

    --     0101 (decimal 5)
    -- XOR 0011 (decimal 3)
    --   = 0110 (decimal 6)

    >>> Binary.xor
    ..>   (fromHex "5")
    ..>   (fromHex "3")
    fromHex "6"

-}
xor : Bits -> Bits -> Bits
xor a b =
    condense
        (\( x, y ) -> Basics.xor x y)
        (makeIsometric a b)


{-| NOT operator.

    -- NOT 0111 (decimal 7)
    --   = 1000 (decimal 8)

    >>> Binary.not
    ..>   (fromIntegers [ 0, 1, 1, 1 ])
    fromIntegers [ 1, 0, 0, 0 ]

-}
not : Bits -> Bits
not =
    map (List.map Basics.not)



-- BIT SHIFTING


{-| Arithmetic/Logical left shift.

    -- LEFTSHIFT 00010111 (decimal 23)
    --         = 00101110 (decimal 46)

    >>> [ 0, 0, 0, 1, 0, 1, 1, 1 ]
    ..>   |> fromIntegers
    ..>   |> shiftLeftBy 1
    ..>   |> toIntegers
    [ 0, 0, 1, 0, 1, 1, 1, 0 ]

-}
shiftLeftBy : Int -> Bits -> Bits
shiftLeftBy n =
    map (\bits -> List.drop n bits ++ List.repeat n False)


{-| Arithmetic right shift.

    -- ARI-RIGHTSHIFT 10010111 (decimal 151)
    --              = 11001011 (decimal 203)

    >>> [ 1, 0, 0, 1, 0, 1, 1, 1 ]
    ..>   |> fromIntegers
    ..>   |> shiftRightBy 1
    ..>   |> toIntegers
    [ 1, 1, 0, 0, 1, 0, 1, 1 ]

-}
shiftRightBy : Int -> Bits -> Bits
shiftRightBy n =
    map (shiftRightBy_ n)


shiftRightBy_ : Int -> List Bool -> List Bool
shiftRightBy_ n bits =
    if n > 0 then
        let
            firstBit =
                Maybe.withDefault False (List.head bits)
        in
        shiftRightBy_
            (n - 1)
            (firstBit :: List.take (List.length bits - 1) bits)

    else
        bits


{-| Logical right shift.

    -- LOG-RIGHTSHIFT 10010111 (decimal 151)
    --              = 00001011 (decimal 11)

    >>> [ 0, 0, 0, 1, 0, 1, 1, 1 ]
    ..>   |> fromIntegers
    ..>   |> shiftRightZfBy 1
    ..>   |> toIntegers
    [ 0, 0, 0, 0, 1, 0, 1, 1 ]

-}
shiftRightZfBy : Int -> Bits -> Bits
shiftRightZfBy n =
    map
        (\bits ->
            List.append
                (List.repeat n False)
                (List.take (max 0 (List.length bits - n)) bits)
        )


{-| Rotate a binary sequence to the left.

_NOTE: Make sure your binary sequence is of the correct size before rotating!
Rotating 8 bits is not always the same as, for example, 16 bits._

    >>> rotateLeft 1 (ensureBits 32 <| fromHex "17")
    ensureBits 32 (fromHex "2E")

    >>> rotateLeft 2 (ensureBits 32 <| fromHex "96")
    ensureBits 32 (fromHex "258")

-}
rotateLeft : Int -> Bits -> Bits
rotateLeft n =
    map (\bits -> List.drop n bits ++ List.take n bits)


{-| Rotate a binary sequence to the right.

_NOTE: Make sure your binary sequence is of the correct size before rotating!
Rotating 8 bits is not always the same as, for example, 16 bits._

    >>> rotateRight 1 (ensureBits 64 <| fromHex "17")
    ensureBits 64 (fromHex "800000000000000B")

    >>> rotateRight 1 (ensureBits 32 <| fromHex "96")
    ensureBits 32 (fromHex "4B")

    >>> rotateRight 5 (ensureBits 32 <| fromHex "96")
    ensureBits 32 (fromHex "B0000004")

-}
rotateRight : Int -> Bits -> Bits
rotateRight n =
    map (\bits -> List.drop (List.length bits - n) bits ++ List.take (List.length bits - n) bits)



-- MATHEMATICAL OPERATORS


{-| Add two sets of bits together.

    >>> add
    ..>   (fromIntegers [ 1, 0, 1, 1 ])
    ..>   (fromIntegers [ 1, 0, 1, 1 ])
    fromIntegers [ 1, 0, 1, 1, 0  ]

-}
add : Bits -> Bits -> Bits
add a b =
    makeIsometric a b
        |> (\( Bits c, Bits d ) -> List.zip c d)
        |> List.foldr add_ { bits = [], carryOver = False }
        |> add_ ( False, False )
        |> .bits
        |> Bits


add_ :
    ( Bool, Bool )
    -> { bits : List Bool, carryOver : Bool }
    -> { bits : List Bool, carryOver : Bool }
add_ ( x, y ) { bits, carryOver } =
    if carryOver && x && y then
        -- 1 + 1 + 1
        { bits = True :: bits, carryOver = True }

    else if x && y then
        -- 0 + 1 + 1
        { bits = False :: bits, carryOver = True }

    else if carryOver && (x || y) then
        -- 1 + 0 + 1
        -- 1 + 1 + 0
        { bits = False :: bits, carryOver = True }

    else if x || y then
        -- 0 + 0 + 1
        -- 0 + 1 + 0
        { bits = True :: bits, carryOver = False }

    else if carryOver then
        -- 1 + 0 + 0
        { bits = True :: bits, carryOver = False }

    else
        -- 0 + 0 + 0
        { bits = False :: bits, carryOver = False }



-- UTILITIES


{-| Ensure the binary sequence length is of certain size.

    >>> ensureBits 4 (fromIntegers [ 1, 0 ])
    fromIntegers [ 0, 0, 1, 0 ]

-}
ensureBits : Int -> Bits -> Bits
ensureBits size (Bits bits) =
    bits
        |> (++) (List.repeat (max 0 (size - List.length bits)) False)
        |> Bits


{-| Drops the leading zeros of a binary sequence.

    >>> dropLeadingZeros (fromIntegers [ 0, 0, 1, 0 ])
    fromIntegers [ 1, 0 ]

-}
dropLeadingZeros : Bits -> Bits
dropLeadingZeros =
    map (List.dropWhile ((==) False))


{-| Makes two sequences isometric (equal in size).

    >>> makeIsometric
    ..>   (fromIntegers [ 0, 1, 0 ])
    ..>   (fromIntegers [ 1, 0, 0, 0 ])
    ( fromIntegers [ 0, 0, 1, 0 ]
    , fromIntegers [ 1, 0, 0, 0 ]
    )

-}
makeIsometric : Bits -> Bits -> ( Bits, Bits )
makeIsometric (Bits a) (Bits b) =
    let
        size =
            max (List.length a) (List.length b)
    in
    ( ensureBits size (Bits a)
    , ensureBits size (Bits b)
    )



------------------------------------------------------------------------
-- PRIVATE
------------------------------------------------------------------------


condense : (( Bool, Bool ) -> Bool) -> ( Bits, Bits ) -> Bits
condense fn ( Bits a, Bits b ) =
    Bits (List.map fn (List.zip a b))


ifThenElse : Bool -> a -> a -> a
ifThenElse bool a b =
    if bool == True then
        a

    else
        b


map : (List Bool -> List Bool) -> Bits -> Bits
map fn (Bits list) =
    Bits (fn list)



-- CONSTANTS


hexToBinaryTableList : List ( Char, List Bool )
hexToBinaryTableList =
    [ ( '0', [ False, False, False, False ] )
    , ( '1', [ False, False, False, True ] )
    , ( '2', [ False, False, True, False ] )
    , ( '3', [ False, False, True, True ] )
    , ( '4', [ False, True, False, False ] )
    , ( '5', [ False, True, False, True ] )
    , ( '6', [ False, True, True, False ] )
    , ( '7', [ False, True, True, True ] )
    , ( '8', [ True, False, False, False ] )
    , ( '9', [ True, False, False, True ] )
    , ( 'A', [ True, False, True, False ] )
    , ( 'B', [ True, False, True, True ] )
    , ( 'C', [ True, True, False, False ] )
    , ( 'D', [ True, True, False, True ] )
    , ( 'E', [ True, True, True, False ] )
    , ( 'F', [ True, True, True, True ] )
    ]


hexToBinaryTable : Dict Char (List Bool)
hexToBinaryTable =
    Dict.fromList hexToBinaryTableList
