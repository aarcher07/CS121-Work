
```r
outlier <- function(x) {
    quartile2 <- median(x)
    quartile1 <- median(x[x <= quartile2])
    quartile3 <- median(x[x >= quartile2])
    
    
    low <- quartile1 - 1.5 * (quartile3 - quartile1)
    high <- quartile3 + 1.5 * (quartile3 - quartile1)
    
    return(c(x[x < low], x[x > high]))
    
}
```



```r
outlier(c(6, 8, 8, 4, 5, 6, 7, 8, 7, 3, 2, 1, 45632, 1334, 5662, 3))
```

```
## [1] 45632  1334  5662
```



```r
lettersMatch <- function(pattern, words) {
    return(words[grep(pattern, words)])
}
```



```r
small <- c("first", "second", "errand", "arrest", "are")
lettersMatch(c("[frs]"), small)
```

```
## [1] "first"  "second" "errand" "arrest" "are"
```



```r
crosswordpuzzle <- c("aardvark", "apple", "arc", "ark", "awkward", "balast", 
    "beta", "chill", "drive", "empyrean", "fanciful", "giant", "happy", "intoxicated", 
    "jackalope", "anger", "magic", "missile", "picnic", "pizza", "pokeman", 
    "scarlet", "sober", "stone", "study", "ventral", "yeezus", "zoo", "zebra", 
    "zulu", "zigzag", "advance", "add", "adverse", "assualt", "bro", "big", 
    "category", "derive", "enough", "fat", "aggravated", "furious", "frustrated", 
    "hilarious", "hippo", "judge", "jury", "love", "monetary", "never", "umbrage", 
    "zamboni", "Totodile", "Pidgey", "Togepi", "Sceptile", "Arcanine", "Growlithe", 
    "dratini", "Blaziken", "Dragonite", "Altaria", "argue")
pattern <- c("^ag.r")

lettersMatch(pattern, crosswordpuzzle)
```

```
## [1] "aggravated"
```



```r
DigitToWord <- function(number, word) {
    paste(number, "-", word[number + 1])
}
```



```r
word <- c("cero", "uno", "dos", "tres", "cuatro", "cinco", "seis", "siete", 
    "ocho", "nueve")

DigitToWord(0:9, word)
```

```
##  [1] "0 - cero"   "1 - uno"    "2 - dos"    "3 - tres"   "4 - cuatro"
##  [6] "5 - cinco"  "6 - seis"   "7 - siete"  "8 - ocho"   "9 - nueve"
```



```r
piseries <- function(N) {
    b <- 0:(N - 1)  # 0 represents the first element in the series and N-1 represents the last. There are then N elements in the series between 0 and N-1.
    
    fraction <- vector(length = N)  #Creates an empty vector of length N  
    
    fraction <- (4 * (-1)^b)/(2 * b + 1)  #The vector that contains each element in the series 
    
    pisum <- sum(fraction)  #Sum of the series
    
    return(pisum)
    
}
```



```r
piseries(4566)
```

```
## [1] 3.141
```



```r
Canvas <- function(xmin, xmax, ymin, ymax) {
    plot(1:2, type = "n", xlim = c(xmin, xmax), ylim = c(ymin, ymax))
}
howCloseToPi <- function(N) {
    b <- 0:(N - 1)
    fraction <- vector(length = N)
    fraction <- (4 * (-1)^b)/(2 * b + 1)
    partialsums <- cumsum(fraction)
    points(b, partialsums, pch = 20)
    pirep <- rep(pi, N)
    polygon(b, pirep)
    roundup3 <- round(partialsums, digits = 3)
    roundup15 <- round(partialsums, digits = 15)
    Digits3 <- which(roundup3 == 3.141)
    Digits15 <- which(roundup15 == 3.14159265358979)
    return(c(Digits3[1], Digits15[1]))
}
```



```r
Canvas(0, 500, 2.5, 3.9)
howCloseToPi(1000)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 

```
## [1] 916  NA
```




```r
ApproximationtoPi <- function(N) {
    x <- runif(N)
    y <- runif(N)
    distance <- x^2 + y^2
    approximation <- mean(distance < 1) * 4
    return(approximation)
}
```



```r
x <- round(10^runif(1000, min = 2, max = 6))
piPlot <- sapply(x, ApproximationtoPi)
plot(x, piPlot, log = "x", pch = 20, col = rgb(0, 0, 0, 0.4))
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 

