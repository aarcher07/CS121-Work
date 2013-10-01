# Oct 1, 2013
##Reverse

```r
reverser <- function(x) {
    rsplit <- strsplit(x, split = "")
    reversal <- rsplit[[1]][rev(1:nchar(x))]
    stick <- paste(reversal, collapse = "")
    return(stick)
}

```


Test statements:

```r
# Test statement here. Should return true or false.
reverser("vsgsfgs") == "sgfsgsv"
```

```
## [1] TRUE
```


##Scrambler

```r
scrambler <- function(x) {
    rscram <- strsplit(x, split = "")
    scram <- rscram[[1]][sample(1:nchar(x))]
    stick <- paste(scram, collapse = "")
    return(stick)
}

```


Test statements:

```r
# Test statement here. Should return true or false.
scrambler("egegsegr")
```

```
## [1] "reeggesg"
```


#VowelBleeper

```r
VowelBleeper <- function(x) {
    asterisks <- gsub("[aeiou]", "*", x)
    return(asterisks)
}
```

Test statements:

```r
# Test statement here. Should return true or false.
VowelBleeper("sgethaerhe") == "sg*th**rh*"
```

```
## [1] TRUE
```


#l33t

```r
l33t <- function(x) {
    Ereplace <- gsub("[e]", "3", x)
    Oreplace <- gsub("[o]", "0", Ereplace)
    Sreplace <- gsub("[s]", "5", Oreplace)
    Greplace <- gsub("[g]", "9", Sreplace)
    return(Greplace)
}
```

Test statements:

```r
# Test statement here. Should return true or false.
l33t("sgethaerhe")
```

```
## [1] "593tha3rh3"
```

