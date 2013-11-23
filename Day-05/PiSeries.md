

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

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```
## [1] 916  NA
```




```r
ApproximationtoPi2 <- function(N) {
    x <- runif(N)
    y <- runif(N)
    distance <- x^2 + y^2
    approximation <- mean(distance < 1) * 4
    return(approximation)
}
```



```r
x <- round(10^runif(1000, min = 2, max = 6))
piPlot <- sapply(x, ApproximationtoPi2)
plot(x, piPlot, log = "x", pch = 20, col = rgb(0, 0, 0, 0.4))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-61.png) 

```r
plot(x, rep(pi, length(x)), type = "l", add = T)
```

```
## Warning: "add" is not a graphical parameter Warning: "add" is not a
## graphical parameter Warning: "add" is not a graphical parameter Warning:
## "add" is not a graphical parameter Warning: "add" is not a graphical
## parameter Warning: "add" is not a graphical parameter
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-62.png) 


