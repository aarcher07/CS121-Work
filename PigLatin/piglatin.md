
```r
latinizer <- function(word) {
    
    vowels <- c("a", "e", "i", "o", "u")
    
    positions <- vector(length = 5)
    
    
    for (k in 1:5) {
        vowel <- vowels[k]
        empty <- regexpr(vowel, word)
        positions[k] <- empty[[1]]
    }
    
    positions[positions == -1] <- NA
    
    if (any(positions == 1, na.rm = T)) {
        piglatin <- paste(word, "yay", collapse = "", sep = "")
    } else {
        if (!all(is.na(positions))) {
            firstvowel <- min(positions, na.rm = T)
            fronthalf <- substring(word, 1, (firstvowel - 1))
            
            backhalf <- substring(word, firstvowel, nchar(word))
            
            
            piglatin <- paste(backhalf, fronthalf, "ay", collapse = "", sep = "")
        } else {
            piglatin <- paste(word, "yay", sep = "")
        }
    }
    return(piglatin)
}
```



```r
latinizer("fddbfduator")
```

```
## [1] "uatorfddbfday"
```

