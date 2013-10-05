Functions and Graphics
========================================
## Function that counts the number of odd numbers

```r
CountsOdd <- function(x) {
    Remainder <- vector(length = length(x))
    Remainder <- x%%2
    count <- sum(Remainder)
    return(count)
}
```


Now we'll test it.  The answer should be 3:

```r
CountsOdd(c(3, 5, 7))
```

```
## [1] 3
```

```r
CountsOdd(c(3, 5, 7, 6, 2, 0))
```

```
## [1] 3
```


## Function that counts the number of even numbers

```r
CountEvens1 <- function(x) {
    TotalCount <- length(x)  #This finds the amount numbers in the vector x.
    Remainder <- vector(length = length(x))  #Generates a vector of the equal length as x.
    Remainder <- x%%2
    Oddcount <- sum(Remainder)
    Evencount <- TotalCount - Oddcount  # After counting the total odd numbers in the vector, the function will subtract that amount from the total. If a number is not odd, it must be even. Therefore, the numbers not counted must be even.
    return(Evencount)
}
```


The answer should be 2:

```r
CountEvens1(c(3, 5, 7, 6, 2, 0))
```

```
## [1] 3
```


## This is another function that will count the number of even numbers.

```r
CountEvens2 <- function(x) {
    x <- x + 1  #This changes every odd number to even and every even number to odd.
    # We can then use the odd count function to count the amount of 'odd'
    # numbers.
    Remainder <- vector(length = length(x))
    Remainder <- x%%2
    Evencount <- sum(Remainder)
    return(Evencount)
}
```


The answer should be 2:

```r
CountEvens2(c(3, 5, 7, 6, 2, 0))
```

```
## [1] 3
```


## This will calculate hypotenuse of a right angled triangle.

```r
hypotenuseLength <- function(a, b) {
    asquared <- a^2
    bsquared <- b^2
    hypotenusesquared <- asquared + bsquared
    hypotenuse <- (hypotenusesquared)^(1/2)
    return(hypotenuse)
}
```


Test for the hypotenuse function.

* Answer should be 5.

```r
hypotenuseLength(3, 4)
```

```
## [1] 5
```


* Answer should be 85.

```r
hypotenuseLength(13, 84)
```

```
## [1] 85
```

 
## This is a function that calculates the missing side of triangle using the Law of Cosine. 

```r
LawOfCosines <- function(a, b, theta) {
    asquared <- a^2
    bsquared <- b^2
    sum <- asquared + bsquared
    Cosine <- cos(theta)
    abCosine <- a %*% b %*% Cosine
    TwoCosine <- 2 %*% abCosine
    csquared <- sum - TwoCosine
    c <- (csquared)^(1/2)
    return(c)
}
```


Test statements:    
* Equalateral triangle.  Answer should be 10.


```r
LawOfCosines(5, 5, pi/3)
```

```
##      [,1]
## [1,]    5
```


* A collapsed triangle. Answer should be 71.

```r
LawOfCosines(13, 84, 0)
```

```
##      [,1]
## [1,]   71
```


*Right angled triangle. Answer should be 85.

```r
LawOfCosines(13, 84, pi/2)
```

```
##      [,1]
## [1,]   85
```


## This will calculate the angle of a triangle using the sides of the triangle 

```r
ThetaFromLengths <- function(a, b, c) {
    asqred <- a^2
    bsqred <- b^2
    csqred <- c^2
    Sum <- asqred + bsqred - csqred
    Divisor <- 2 %*% a %*% b
    quotient <- Sum/Divisor
    angles <- acos(quotient)
    return(angles)
}
```


Test again on an equilateral and right angled triangle: 

```r
ThetaFromLengths(10, 10, 10)
```

```
##       [,1]
## [1,] 1.047
```

```r
pi/3
```

```
## [1] 1.047
```

```r
ThetaFromLengths(3, 4, 5)
```

```
##       [,1]
## [1,] 1.571
```

```r
pi/2
```

```
## [1] 1.571
```

 
## This function tests the accuracy of ThetaFromLengths

```r
testThetaFromLengths <- function(a, b, theta) {
    Difference <- abs(ThetaFromLengths(a, b, LawOfCosines(a, b, theta)) - theta)
    return(Difference)
}
```


Here are the test cases:

```r
testThetaFromLengths(a = 100, b = 34, theta = 2.412)
```

```
##      [,1]
## [1,]    0
```




