
```r
words <- readLines(url("http://dtkaplan.github.io/ScientificComputing/Syllabus/Daily/Day-07/word_list_moby_crossword-flat/word_list_moby_crossword.flat.txt"))

Summary <- function(words) {
    # 100 Longest Words
    numberofcharacters <- nchar(words)
    DescendingCharacters <- order(-numberofcharacters)
    longest100 <- words[DescendingCharacters[1:100]]
    LongestWords <- words[which(numberofcharacters %in% longest100)]
    
    # Word Length
    wordLength <- table(numberofcharacters)
    
    
    NoUwords <- words[-(grep("[u]", words))]
    qNoUwords <- NoUwords[(grep("[q]", NoUwords))]
    
    # Words starting with a given letter of the alphabet
    letters <- c("^a", "^b", "^c", "^d", "^e", "^f", "^g", "^h", "^i", "^j", 
        "^k", "^l", "^m", "^n", "^o", "^p", "^q", "^r", "^s", "^t", "^u", "^v", 
        "^w", "^x", "^y", "^z")
    
    Sums <- vector(length = 26)
    for (k in 1:26) {
        lettersnew <- letters[k]
        Sums[k] <- sum(grepl(lettersnew, words))
    }
    return(c(qNoUwords, LongestWords, wordLength, Sums))
}


Summary(words)
```

```
## Warning: closing unused connection 5
## (http://dtkaplan.github.io/ScientificComputing/Syllabus/Daily/Day-07/word_list_moby_crossword-flat/word_list_moby_crossword.flat.txt)
```

```
##                                                                       
##   "faqir"  "faqirs"    "qaid"   "qaids"  "qindar" "qindars"  "qintar" 
##                                       2         3         4         5 
## "qintars"    "qoph"   "qophs"      "85"     "908"    "3686"    "8258" 
##         6         7         8         9        10        11        12 
##   "14374"   "21727"   "26447"   "16658"    "9199"    "5296"    "3166" 
##        13        14        15        16        17        18        19 
##    "1960"    "1023"     "557"     "261"     "132"      "48"      "16" 
##        20        21                                                   
##       "5"       "3"    "6557"    "6848"   "10385"    "6436"    "4364" 
##                                                                       
##    "4937"    "3950"    "4080"    "4013"    "1106"    "1312"    "3710" 
##                                                                       
##    "6270"    "2208"    "3978"    "8693"     "568"    "7141"   "12591" 
##                                                                       
##    "5951"    "2934"    "1932"    "2927"      "82"     "438"     "398"
```


##Crossword


```r
crossword <- function(pattern) {
    words <- readLines(url("http://dtkaplan.github.io/ScientificComputing/Syllabus/Daily/Day-07/word_list_moby_crossword-flat/word_list_moby_crossword.flat.txt"))
    return(words[grep(pattern, words)])
}
```



```r
crosswordPattern <- function(letters) {
    word <- rep(".", letters["length"] + 2)  #Assigns the word vector. 2 is added to compensate for the '^' and '$'.
    
    
    ListNames <- names(letters)  #This takes the name of the vectors in the 'Letters'' argument
    
    SomeLetters <- ListNames[-(which(ListNames == "length"))]  #This removes the length vector
    
    word[letters[SomeLetters] + 1] <- SomeLetters  #Places the letters in their respective positions
    
    word[length(word)] <- "$"  #Places '$' in front
    word[1] <- "^"  #Places '^' at the end of the pattern 
    
    CompleteWord <- paste(word, collapse = "")  #Makes a complete pattern
    
    return(CompleteWord)  #Returns the pattern
}
```



```r
crosswordPattern(c(b = 4, s = 7, length = 7))
```

```
## [1] "^...b..s$"
```


```![just some text](http://www.slushcreekwalkers.com/images/Crossword.gif)


```r
crossword(crosswordPattern(c(e = 2, r = 3, n = 5, length = 5)))
```

```
## [1] "heron" "reran" "rerun" "serin"
```

```r
crossword(crosswordPattern(c(v = 1, r = 3, length = 5)))
```

```
##  [1] "varas" "varia" "varix" "varna" "varus" "varve" "verbs" "verge"
##  [9] "verse" "verso" "verst" "verts" "vertu" "verve" "viral" "vireo"
## [17] "vires" "virga" "virid" "virls" "virtu" "virus"
```

```r
crossword(crosswordPattern(c(b = 1, l = 3, s = 4, a = 5, length = 5)))
```

```
## Warning: closing unused connection 6
## (http://dtkaplan.github.io/ScientificComputing/Syllabus/Daily/Day-07/word_list_moby_crossword-flat/word_list_moby_crossword.flat.txt)
## Warning: closing unused connection 5
## (http://dtkaplan.github.io/ScientificComputing/Syllabus/Daily/Day-07/word_list_moby_crossword-flat/word_list_moby_crossword.flat.txt)
```

```
## [1] "balsa"
```



```r
Scrabble <- function(letters) {
    words <- readLines(url("http://dtkaplan.github.io/ScientificComputing/Syllabus/Daily/Day-07/word_list_moby_crossword-flat/word_list_moby_crossword.flat.txt"))
    
    letter1 <- letters[1]
    letter2 <- letters[2]
    letter3 <- letters[3]
    letter4 <- letters[4]
    letter5 <- letters[5]
    letter6 <- letters[6]
    letter7 <- letters[7]
    letter8 <- letters[8]
    letter9 <- letters[9]
    
    Numofchar <- nchar(words)
    limitedWords <- words[which(Numofchar >= 7 & Numofchar <= 9)]
    scrabblewords1 <- limitedWords[grep(letter1, limitedWords)]
    scrabblewords2 <- scrabblewords1[grep(letter2, scrabblewords1)]
    scrabblewords3 <- scrabblewords2[grep(letter3, scrabblewords2)]
    scrabblewords4 <- scrabblewords3[grep(letter4, scrabblewords3)]
    scrabblewords5 <- scrabblewords4[grep(letter5, scrabblewords4)]
    scrabblewords6 <- scrabblewords5[grep(letter6, scrabblewords5)]
    scrabblewords7 <- scrabblewords6[grep(letter7, scrabblewords6)]
    
    NoTieIn <- vector(length = 10)
    NoTieIn <- scrabblewords7
    cat("Highest scoring words ", NoTieIn)
    
    # Tie-in Words
    scrabblewords8 <- scrabblewords7[grep(letter8, scrabblewords7)]
    scrabblewords9 <- scrabblewords8[grep(letter9, scrabblewords8)]
    Words <- vector(length = 10)
    Words <- c(scrabblewords9, scrabblewords7, scrabblewords8)
    cat("\n", "These are the possible words including tie-in words: ", Words)
}
Scrabble(c("r", "l", "a", "m", "e", "o", "n", "g", "d"))
```

```
## Warning: closing unused connection 5
## (http://dtkaplan.github.io/ScientificComputing/Syllabus/Daily/Day-07/word_list_moby_crossword-flat/word_list_moby_crossword.flat.txt)
```

```
## Highest scoring words  almoner almoners almonries amelcorn amelcorns angleworm cornmeal cornmeals normalize patrolmen womanlier
##  These are the possible words including tie-in words:  almoner almoners almonries amelcorn amelcorns angleworm cornmeal cornmeals normalize patrolmen womanlier angleworm
```

