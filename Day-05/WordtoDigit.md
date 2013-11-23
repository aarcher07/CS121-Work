
```r
DigitToWord <- function(number, word) {
    paste(number, "-", word[number + 1])
}
```



```r
number <- c(0:9)
word <- c("cero", "uno", "dos", "tres", "cuatro", "cinco", "seis", "siete", 
    "ocho", "nueve")

DigitToWord(0:9, word)
```

```
##  [1] "0 - cero"   "1 - uno"    "2 - dos"    "3 - tres"   "4 - cuatro"
##  [6] "5 - cinco"  "6 - seis"   "7 - siete"  "8 - ocho"   "9 - nueve"
```


