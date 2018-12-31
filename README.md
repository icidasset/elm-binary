# Binary

Work with sequences of [binary numbers](https://en.wikipedia.org/wiki/Binary_number).

```elm
>>> [ 1, 0, 0, 0 ]
..>   |> Binary.fromIntegers
..>   |> Binary.shiftRightBy 1
..>   |> Binary.toDecimal
4

>>> 4
..>   |> Binary.fromDecimal
..>   |> Binary.shiftLeftBy 1
..>   |> Binary.toIntegers
[ 1, 0, 0, 0 ]
```


### Convertors

- Hexadecimal
- Decimal
- Integers
- Booleans
- Strings

#### TODO

- Bytes


### Operations

- Basics: and, or, xor, not (complement)
- Shifting: left & right shift, left & right rotation
- Maths: add, subtract



### Utilities

- Collections: append, chunksOf, concat, dropLeadingZeros
- Sizing: ensureBits, makeIsomorphic, width
