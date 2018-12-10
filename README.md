# Binary

Work with sequences of [binary numbers](https://en.wikipedia.org/wiki/Binary_number).


```elm
>>> [ 1, 0, 0, 0 ]
..>   |> Binary.fromIntegers
..>   |> Binary.shiftRightBy 1
..>   |> Binary.toDecimal
4
```
