
```r
pigLatinRule <- function(word) {
    if (grepl("^[^aeiou]+[aeiou]", word)) {
        res <- "consonant"
    } else {
        if (grepl("^[aeiou].+$", word)) {
            res <- "vowel"
        } else {
            ifelse(grepl("[:;,.?!']", word), res <- "punc", res <- "novowels")
        }
    }
    return(res)
}
```


```r
pigLatinRule("wdgsrigr")
```

```
## [1] "consonant"
```


```r
rule1 <- function(word) {
    if (grepl("^[^aeiou]+[aeiou]", word)) {
        firstletter <- min(as.numeric(regexpr("[aeiou]", word)))
        fronthalf <- substr(word, 1, (firstletter - 1))
        backhalf <- substr(word, firstletter, nchar(word))
        piglatin <- paste(backhalf, fronthalf, "ay", sep = "", collapse = "")
        return(piglatin)
    }
}
```


```r
rule2 <- function(word) {
    if (grepl("^[aeiou].+$", word)) {
        piglatin <- paste(word, "yay", sep = "", collapse = "")
        return(piglatin)
    }
}
```


```r
rule3 <- function(word) {
    if (grepl("[^aeiou]", word)) {
        piglatin <- paste(word, "yay", sep = "", collapse = "")
    }
    return(piglatin)
}
```


```r
punctuation <- function(word) {
    return(word)
}
```



```r
piglatinizer <- function(word) {
    res <- switch(pigLatinRule(word), consonant = rule1(word), vowel = rule2(word), 
        novowels = rule3(word), punc = punctuation(word))
    return(res)
}
```


```r
piglatinizer("pig")
```

```
## [1] "igpay"
```



```r
toPigLatin <- function(string) {
    
    number <- unlist(gregexpr(" ", string))
    spaces <- vector(length = (length(number) + 2))
    spaces[2:(length(spaces) - 1)] <- number
    spaces[1] <- 0
    spaces[length(spaces)] <- nchar(string) + 1
    
    Word <- vector(length = (length(number) + 1))
    k <- 1
    
    numberofPunctuations <- sum(grepl("[:;,.?!']", string))
    punc <- vector(length = numberofPunctuations + length(Word))
    
    while (k != length(spaces)) {
        first <- spaces[k]
        second <- spaces[k + 1]
        Word[k] <- substr(string, (first + 1), (second - 1))
        
        k <- k + 1
    }
    
    i <- 1
    for (w in Word) {
        if (grepl("[:;,.?!']", w)) {
            pos <- unlist(gregexpr("[:;,.?!']", w))
            if (pos == 1) {
                fronthalf <- substr(w, pos, pos)
                punc[i] <- fronthalf
                backhalf <- substr(w, pos + 1, nchar(w))
                punc[i + 1] <- backhalf
                i <- i + 2
            } else {
                fronthalf <- substr(w, 1, pos - 1)
                punc[i] <- fronthalf
                backhalf <- substr(w, pos, pos)
                punc[i + 1] <- backhalf
                i <- i + 2
            }
        } else {
            punc[i] <- w
            i <- i + 1
        }
    }
    
    sentence <- sapply(punc, piglatinizer)
    CompleteSentence <- paste(sentence, collapse = " ")
    return(CompleteSentence)
}

toPigLatin(c("when in the course of human events, it becomes necessary, for one people to dissolve the political bonds."))
```

```
## [1] "enwhay inyay ethay oursecay ofyay umanhay eventsyay , ityay ecomesbay ecessarynay , orfay oneyay eoplepay otay issolveday ethay oliticalpay ondsbay ."
```

##There are actually two rule to pig latin. 1) If the word contains a vowel, but not at the start, break the word vowel and reverse the segments , adding 'ay'. 2) If the word does not contain a vowel or starts with a vowel, just add 'yay'.


```r
pigLatinRule2 <- function(word) {
    ifelse(grepl("^[^aeiou]+[aeiou]", word), res <- "consonant", ifelse(grepl("^[aeiou].+$", 
        word), res <- "vowel", res <- "novowels"))
    return(res)
}
# This provides more truncated and simplier format for an if else statement
```



```r
pigLatinRule2("JKDVFGHSKHeMb")
```

```
## [1] "consonant"
```



```r
pigLatinRuleFixed <- function(word) {
    ifelse(grepl("^[^aeiouAEIOU]+[aeiouAEIOU]", word), res <- "consonant", ifelse(grepl("^[aeiouAEIOU].+$", 
        word), res <- "vowel", res <- "novowels"))
    return(res)
}
```



```r
pigLatinRuleFixed("JKDVFGHSKHeMb")
```

```
## [1] "consonant"
```

