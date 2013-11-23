BaseConversion
=============================================

```r
toBase <- function(Z, b) {
    base <- vector(length = 145)
    k <- 1
    while (Z != 0) {
        rem <- Z%%b
        base[k] <- rem
        Z <- (Z - rem)/b
        k <- k + 1
    }
    return(as.character(rev(base[1:k - 1])))
}
```




```r
toBase(Z = 10, b = 2)
```

```
## [1] "1" "0" "1" "0"
```



```r
toBase(Z = 10, b = 2)
```

```
## [1] "1" "0" "1" "0"
```



```r
basetoNumeric <- function(x, b) {
    n <- length(x)
    base <- b^((n - 1):0)
    numbers <- as.numeric(x) * base
    return(sum(numbers))
}
```



```r
basetoNumeric2 <- function(x, b) {
    numbers <- vector(length = (length(x)))
    n <- b^((length(x) - 1):0)
    for (k in 1:length(x)) {
        numbers[k] <- as.numeric(x[k]) * n[k]
    }
    return(sum(numbers))
}
```



```r
basetoNumeric(c("1", "0", "1", "0", "1", "0"), 2)
```

```
## [1] 42
```

```r
basetoNumeric2(c("1", "0", "1", "0", "1", "0"), 2)
```

```
## [1] 42
```



```r
ToBaseSixteen <- function(Z) {
    base16 <- vector(length = 456)
    k <- 1
    while (Z != 0) {
        base16[k] <- Z%%16
        Z <- floor(Z/16)
        k <- k + 1
    }
    base <- as.character(base16[1:k - 1])
    
    base[base == 10] <- "a"
    base[base == 11] <- "b"
    base[base == 12] <- "c"
    base[base == 13] <- "d"
    base[base == 14] <- "e"
    base[base == 15] <- "f"
    
    return(rev(base))
}
```



```r
ToBaseSixteen(34332)
```

```
## [1] "8" "6" "1" "c"
```




```r
ToBase256 <- function(Z) {
    base <- vector(length = 456)
    k <- 1
    while (Z != 0) {
        base[k] <- Z%%256
        Z <- floor(Z/256)
        k <- k + 1
    }
    base <- base[1:k - 1]
    base256 <- vector(length = (k - 1))
    
    base16 <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "a", "b", "c", "d", "e", "f")
    
    for (n in 1:k - 1) {
        firstdigit <- base16[(floor(base[n]/16)) + 1]
        seconddigit <- base16[(base[n]%%16) + 1]
        base256[n] <- paste(c(firstdigit, seconddigit), collapse = "")
    }
    
    return(rev(base256))
}
```



```r
ToBase256(256)
```

```
## [1] "01" "00"
```

