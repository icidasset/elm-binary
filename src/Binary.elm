module Binary exposing
    ( Bits, empty
    , fromHex, toHex, fromDecimal, toDecimal, fromIntegers, toIntegers, fromBooleans, toBooleans, fromString, toString
    , and, or, xor, not
    , shiftLeftBy, shiftRightBy, shiftRightZfBy, rotateLeftBy, rotateRightBy
    , add, subtract
    , append, chunksOf, concat, dropLeadingZeros, ensureSize, makeIsometric, width
    )

{-|

@docs Bits, empty


# Converters

@docs fromHex, toHex, fromDecimal, toDecimal, fromIntegers, toIntegers, fromBooleans, toBooleans, fromString, toString


# Bitwise Operators

@docs and, or, xor, not


# Bit Shifting

@docs shiftLeftBy, shiftRightBy, shiftRightZfBy, rotateLeftBy, rotateRightBy


# Mathematical Operators

@docs add, subtract


# Utilities

@docs append, chunksOf, concat, dropLeadingZeros, ensureSize, makeIsometric, width

-}

import Dict exposing (Dict)
import List.Extra as List



-- 🌳


{-| **The binary sequence.**

Use converters to make `Bits`.

    Binary.fromIntegers [ 0, 1, 0, 1 ]

-}
type Bits
    = Bits (List Bool)


{-| An empty binary sequence.
-}
empty : Bits
empty =
    Bits []



-- CONVERTERS


{-| Convert a hex string to list of binary numbers.

    >>> fromHex "8" |> toIntegers
    [ 1, 0, 0, 0 ]

-}
fromHex : String -> Bits
fromHex hex =
    hex
        |> String.toList
        |> concatMap hexCharToBinary
        |> Bits


{-| Convert a list of binary numbers to a hex string.

    >>> toHex <| fromIntegers [ 1, 0, 0, 0 ]
    "8"

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
        |> List.map (binaryToHexChar >> Maybe.withDefault '0')
        |> String.fromList


{-| Convert a decimal to `Bits`.

    >>> fromDecimal 8 |> toIntegers
    [ 1, 0, 0, 0 ]

-}
fromDecimal : Int -> Bits
fromDecimal =
    fromDecimal_ [] >> fromIntegers


fromDecimal_ : List Int -> Int -> List Int
fromDecimal_ acc n =
    let
        ( x, bit ) =
            ( n // 2, remainderBy 2 n )

        bits =
            modBy 2 bit :: acc
    in
    if x > 0 then
        fromDecimal_ bits x

    else
        bits


{-| Convert `Bits` to a decimal.

    >>> toDecimal <| fromIntegers [ 1, 0, 0, 0 ]
    8

-}
toDecimal : Bits -> Int
toDecimal (Bits bits) =
    bits
        |> List.foldl
            (\bit ( x, exponent ) ->
                ( (2 ^ exponent) * ifThenElse bit 1 0 + x
                , exponent - 1
                )
            )
            ( 0, List.length bits - 1 )
        |> Tuple.first


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
    List.map (\b -> ifThenElse b 1 0) bits


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


{-| Convert a string to `Bits`.

The resulting bits will represent the decimal value
(ie. code point) of each character (it uses `String.toList`).

The first argument determines how many bits
are used per decimal value.

    -- 1 character
    -- Code points: [ 0x1F936 ]

    >>> "🤶"
    ..>   |> fromString 32
    ..>   |> toHex
    "0001F936"

    -- 3 characters
    -- Code points: [ 0x61, 0x62, 0x63 ]
    -- These hexadecimal values are each 8 bits long.

    >>> "abc"
    ..>   |> fromString 8
    ..>   |> toHex
    "616263"

-}
fromString : Int -> String -> Bits
fromString amountOfBitsPerCharacter string =
    string
        |> String.toList
        |> concatMap (fromString_ amountOfBitsPerCharacter)
        |> Bits


fromString_ : Int -> Char -> List Bool
fromString_ amountOfBitsPerCharacter char =
    char
        |> Char.toCode
        |> fromDecimal
        |> ensureSize amountOfBitsPerCharacter
        |> unwrap


{-| Convert `Bits` to a string.

1.  Splits the bits in chunks of the given number
2.  Each chunk is converted to a decimal (code point)
3.  Each code point is translated to a character
4.  The list of characters is converted to a string

The first argument determines how many bits
are used per decimal value (ie. how large the chunks are).

    >>> "0001F936"
    ..>   |> fromHex
    ..>   |> toString 32
    "🤶"

    >>> "616263"
    ..>   |> fromHex
    ..>   |> toString 8
    "abc"

-}
toString : Int -> Bits -> String
toString amountOfBitsPerCharacter bits =
    bits
        |> chunksOf amountOfBitsPerCharacter
        |> List.map (toDecimal >> Char.fromCode)
        |> String.fromList



-- BITWISE OPERATORS


{-| AND operator.

    --     0101 (decimal 5)
    -- AND 0011 (decimal 3)
    --   = 0001 (decimal 1)

    >>> Binary.and
    ..>   (fromHex "5")
    ..>   (fromHex "3")
    ensureSize 4 (fromHex "1")

-}
and : Bits -> Bits -> Bits
and a b =
    condense (&&) (makeIsometric a b)


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
    condense (||) (makeIsometric a b)


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
    condense Basics.xor (makeIsometric a b)


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
shiftRightBy n (Bits bits) =
    case bits of
        firstBit :: _ ->
            Bits <| List.repeat n firstBit ++ List.take (List.length bits - n) bits

        [] ->
            Bits <| []


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
shiftRightZfBy n (Bits bits) =
    List.append
        (List.repeat n False)
        (List.take (List.length bits - n) bits)
        |> Bits


{-| Rotate a binary sequence to the left.

_NOTE: Make sure your binary sequence is of the correct size before rotating!
Rotating 8 bits is not always the same as, for example, 16 bits._

    >>> rotateLeftBy 1 (ensureSize 32 <| fromHex "17")
    ensureSize 32 (fromHex "2E")

    >>> rotateLeftBy 2 (ensureSize 32 <| fromHex "96")
    ensureSize 32 (fromHex "258")

-}
rotateLeftBy : Int -> Bits -> Bits
rotateLeftBy n =
    map (\bits -> List.drop n bits ++ List.take n bits)


{-| Rotate a binary sequence to the right.

_NOTE: Make sure your binary sequence is of the correct size before rotating!
Rotating 8 bits is not always the same as, for example, 16 bits._

    >>> rotateRightBy 1 (ensureSize 64 <| fromHex "17")
    ensureSize 64 (fromHex "800000000000000B")

    >>> rotateRightBy 1 (ensureSize 32 <| fromHex "96")
    ensureSize 32 (fromHex "4B")

    >>> rotateRightBy 5 (ensureSize 32 <| fromHex "96")
    ensureSize 32 (fromHex "B0000004")

-}
rotateRightBy : Int -> Bits -> Bits
rotateRightBy n =
    map (\bits -> List.splitAt (List.length bits - n) bits |> (\( a, b ) -> b ++ a))



-- MATHEMATICAL OPERATORS


{-| Add two sets of bits together.

    -- ADD 1011
    --     1011
    --  = 10110

    >>> add
    ..>   (fromIntegers [ 1, 0, 1, 1 ])
    ..>   (fromIntegers [ 1, 0, 1, 1 ])
    fromIntegers [ 1, 0, 1, 1, 0  ]

    >>> add
    ..>   (fromIntegers [ 1, 1, 1, 0, 1 ])
    ..>   (fromIntegers [ 1, 0, 1, 0 ])
    fromIntegers [ 1, 0, 0, 1, 1, 1  ]

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


{-| Subtract two sets of bits from each other.

    -- SUBTRACT 1011
    --          11
    --        = 1010

    >>> subtract
    ..>   (fromIntegers [ 1, 0, 1, 1 ])
    ..>   (fromIntegers [ 1, 1 ])
    fromIntegers [ 1, 0, 0, 0  ]

    >>> subtract
    ..>   (fromIntegers [ 1, 0, 0, 0, 1 ])
    ..>   (fromIntegers [ 0, 0, 1, 0, 0 ])
    fromIntegers [ 0, 1, 1, 0, 1  ]

-}
subtract : Bits -> Bits -> Bits
subtract a b =
    makeIsometric a b
        |> (\( Bits c, Bits d ) ->
                subtract_
                    { bits = []
                    , minuend = c
                    , subtrahend = d
                    }
           )
        |> .bits
        |> Bits


subtract_ :
    { bits : List Bool, minuend : List Bool, subtrahend : List Bool }
    -> { bits : List Bool, minuend : List Bool, subtrahend : List Bool }
subtract_ { bits, minuend, subtrahend } =
    case ( List.unconsLast minuend, List.unconsLast subtrahend ) of
        ( Just ( a, minuend_ ), Just ( b, subtrahend_ ) ) ->
            let
                maybeIdx =
                    List.findIndex identity minuend_

                ( bit, newSeqA ) =
                    if a == True && b == False then
                        ( True
                        , minuend_
                        )

                    else if a == True && b == True then
                        ( False
                        , minuend_
                        )

                    else if b == True && isJust maybeIdx then
                        let
                            idx =
                                Maybe.withDefault 0 maybeIdx
                        in
                        ( True
                        , List.drop idx minuend_
                            ++ [ False ]
                            ++ List.repeat (List.length minuend_ - idx - 1) True
                        )

                    else
                        ( False
                        , minuend_
                        )
            in
            subtract_
                { bits = bit :: bits
                , minuend = newSeqA
                , subtrahend = subtrahend_
                }

        _ ->
            { bits = bits
            , minuend = []
            , subtrahend = []
            }



-- UTILITIES


{-| Merge two binary sequences.

    >>> append
    ..>   (fromIntegers [ 1, 0, 0, 0 ])
    ..>   (fromIntegers [ 1, 0, 1, 0 ])
    fromIntegers [ 1, 0, 0, 0, 1, 0, 1, 0 ]

-}
append : Bits -> Bits -> Bits
append (Bits a) (Bits b) =
    Bits (List.append a b)


{-| Split the binary sequence in multiple chunks.

    >>> fromIntegers [ 1, 0, 0, 0, 1, 0, 1, 0 ]
    ..>   |> chunksOf 4
    ..>   |> List.map toIntegers
    [ [ 1, 0, 0, 0 ]
    , [ 1, 0, 1, 0 ]
    ]

-}
chunksOf : Int -> Bits -> List Bits
chunksOf n (Bits bits) =
    bits
        |> List.greedyGroupsOf n
        |> List.map Bits


{-| Concat multiple binary sequences.

    >>> [ fromIntegers [ 1, 0, 0, 0 ]
    ..> , fromIntegers [ 0, 1, 0, 1 ]
    ..> ]
    ..>   |> concat
    ..>   |> toDecimal
    133

-}
concat : List Bits -> Bits
concat =
    concatMap unwrap >> Bits


{-| Drops the leading zeros of a binary sequence.

    >>> dropLeadingZeros (fromIntegers [ 0, 0, 1, 0 ])
    fromIntegers [ 1, 0 ]

-}
dropLeadingZeros : Bits -> Bits
dropLeadingZeros =
    map (List.dropWhile ((==) False))


{-| Ensure the binary sequence length is of certain size.

    >>> ensureSize 4 (fromIntegers [ 1, 0 ])
    fromIntegers [ 0, 0, 1, 0 ]

-}
ensureSize : Int -> Bits -> Bits
ensureSize size (Bits bits) =
    let
        currentLength =
            List.length bits
    in
    if currentLength == size then
        Bits bits

    else if currentLength > size then
        Bits (List.drop (currentLength - size) bits)

    else
        Bits (List.repeat (size - currentLength) False ++ bits)


{-| Makes two sequences isometric (equal in size).

    >>> makeIsometric
    ..>   (fromIntegers [ 0, 1, 0 ])
    ..>   (fromIntegers [ 1, 0, 0, 0 ])
    ( fromIntegers [ 0, 0, 1, 0 ]
    , fromIntegers [ 1, 0, 0, 0 ]
    )

-}
makeIsometric : Bits -> Bits -> ( Bits, Bits )
makeIsometric a b =
    let
        ( widthA, widthB ) =
            ( width a
            , width b
            )
    in
    if widthA == widthB then
        ( a, b )

    else if widthA > widthB then
        ( a, ensureSize widthA b )

    else
        ( ensureSize widthB a, b )


{-| Get the amount of bits in a binary sequence.

    >>> fromIntegers [ 1, 0, 0, 0 ]
    ..>   |> width
    4

-}
width : Bits -> Int
width (Bits bits) =
    List.length bits



-----------------------------------------
-- PRIVATE
-----------------------------------------


concatMap : (a -> List b) -> List a -> List b
concatMap fn =
    -- A slightly faster alternative for List.concatMap
    List.foldr (\a -> List.append (fn a)) []


condense : (Bool -> Bool -> Bool) -> ( Bits, Bits ) -> Bits
condense fn ( Bits a, Bits b ) =
    Bits (List.map2 fn a b)


ifThenElse : Bool -> a -> a -> a
ifThenElse bool a b =
    if bool == True then
        a

    else
        b


isJust : Maybe a -> Bool
isJust m =
    case m of
        Just _ ->
            True

        Nothing ->
            False


map : (List Bool -> List Bool) -> Bits -> Bits
map fn (Bits list) =
    Bits (fn list)


unwrap : Bits -> List Bool
unwrap (Bits bits) =
    bits



-- CONSTANTS


hexCharToBinary : Char -> List Bool
hexCharToBinary char =
    case Char.toUpper char of
        '0' ->
            [ False, False, False, False ]

        '1' ->
            [ False, False, False, True ]

        '2' ->
            [ False, False, True, False ]

        '3' ->
            [ False, False, True, True ]

        '4' ->
            [ False, True, False, False ]

        '5' ->
            [ False, True, False, True ]

        '6' ->
            [ False, True, True, False ]

        '7' ->
            [ False, True, True, True ]

        '8' ->
            [ True, False, False, False ]

        '9' ->
            [ True, False, False, True ]

        'A' ->
            [ True, False, True, False ]

        'B' ->
            [ True, False, True, True ]

        'C' ->
            [ True, True, False, False ]

        'D' ->
            [ True, True, False, True ]

        'E' ->
            [ True, True, True, False ]

        'F' ->
            [ True, True, True, True ]

        _ ->
            []


binaryToHexChar : List Bool -> Maybe Char
binaryToHexChar binary =
    case binary of
        [ False, False, False, False ] ->
            Just '0'

        [ False, False, False, True ] ->
            Just '1'

        [ False, False, True, False ] ->
            Just '2'

        [ False, False, True, True ] ->
            Just '3'

        [ False, True, False, False ] ->
            Just '4'

        [ False, True, False, True ] ->
            Just '5'

        [ False, True, True, False ] ->
            Just '6'

        [ False, True, True, True ] ->
            Just '7'

        [ True, False, False, False ] ->
            Just '8'

        [ True, False, False, True ] ->
            Just '9'

        [ True, False, True, False ] ->
            Just 'A'

        [ True, False, True, True ] ->
            Just 'B'

        [ True, True, False, False ] ->
            Just 'C'

        [ True, True, False, True ] ->
            Just 'D'

        [ True, True, True, False ] ->
            Just 'E'

        [ True, True, True, True ] ->
            Just 'F'

        _ ->
            Nothing
