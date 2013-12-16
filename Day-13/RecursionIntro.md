
```r
fibLoop <- function(n) {
    fibSeq <- vector(length = (n + 1))
    fibSeq[1] <- 0
    fibSeq[2] <- 1
    for (k in 3:(n + 1)) {
        fibSeq[k] <- fibSeq[k - 1] + fibSeq[k - 2]
    }
    return(fibSeq)
}
```



```r
fibLoop(31)
```

```
##  [1]       0       1       1       2       3       5       8      13
##  [9]      21      34      55      89     144     233     377     610
## [17]     987    1597    2584    4181    6765   10946   17711   28657
## [25]   46368   75025  121393  196418  317811  514229  832040 1346269
```



```r
fibRecurse <- function(n) {
    if (n == 1) {
        return(1)
    } else {
        if (n == 0) 
            return(0)
    }
    before <- Sys.time()
    thisF <- fibRecurse(n - 1) + fibRecurse(n - 2)
    return(c(as.numeric(Sys.time() - before)))
}
```



```r
fibRecurse(2)
```

```
## [1] 0
```



```r
addNSeq <- function(n) {
    if (n == 0) 
        return(0)
    return(n + addNSeq(n - 1))
}
```



```r
addNSeq(5)
```

```
## [1] 15
```



```r
addRecursively <- function(n) {
    if (length(n) == 1) 
        return(n)
    return(n[1] + addRecursively(n[-1]))
}
```



```r
integrateRiemann <- function(f, a = 0, b = 1) {
    nbins <- 5
    biggerBins <- simpleRiemann(f, a = a, b = b, n = nbins)  #Create the state
    for (k in 1:5) {
        nbins <- nbins * 10  #the loop utlizies nbins as the amount of the bins need to calculate a more accurate sum. The larger nbins becomes the more accurate the approximation. Because nbins will become very large number even after 5 repetition, the value of the sum should approximate the area under the function. In the worst case, there will be 500,000 bins.
        
        smallerBins <- simpleRiemann(f, a = a, b = b, n = nbins)
        
        if (abs(smallerBins - biggerBins) < 1e-05) 
            break
        biggerBins <- smallerBins  #The if statement above recognizes if the current approximatation is better than the previous to 5 decimal places. If it is, repeat the process creating an new approximation. To compare the current to previous approximation, the loop initializes the previous approximation to biggerBins and recalculates smallerBins with a large nbins.
    }
    return(smallerBins)
}
```



```r
simpleRiemann <- function(f, a = 0, b = 1, n = 3) {
    rectangleWidth <- (b - a)/n
    midpoints <- seq(a + rectangleWidth/2, b - rectangleWidth/2, length = n)
    rectangleAreas <- sapply(midpoints, f) * rectangleWidth
    return(sum(rectangleAreas))
}
```



```r
integrateRecursive <- function(f, a, b, bLIM = (b - a) * 1e-04) {
    
    
    bigBins <- simpleRiemann(f, a = a, b = b, n = 5)
    smallBins <- simpleRiemann(f, a = a, b = b, n = 10)
    if ((abs(bigBins - smallBins) < 1e-04)) {
        # | ( (b-a)/10 < bLIM)){
        return(smallBins)
    } else {
        if ((b - a)/10 < bLIM) {
            warning("bLim exceeded")  #If blim is exceeded a warning is returned
            return(smallBins)
        } else {
            mid <- (a + b)/2
            total <- integrateRecursive(f, a = a, b = mid, bLIM = bLIM) + integrateRecursive(f, 
                a = mid, b = b, bLIM = bLIM)
            return(total)
        }
    }
}
```


Testing with $x^{6}$
Integral of  $x^{6}$ is $\frac{x^{7}}{7}$

```r
integral <- (10)^7/7 - 1^7/7  #Integral x^6
integrateRecursive(function(x) {
    x^6
}, 1, 10, )
```

```
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
## Warning: bLim exceeded Warning: bLim exceeded Warning: bLim exceeded
```

```
## [1] 1428571
```

Testing with $x^{5}$
Integral of  $x^{5}$ is $\frac{x^{6}}{6}$

```r
integral2 <- (10)^6/6 - 1^6/6
integrateRecursive(function(x) {
    x^5
}, 1, 10, )
```

```
## [1] 166666
```

```r
integrateRecursive(function(x) {
    x^5
}, 1, 10, ) == integral2
```

```
## [1] FALSE
```


Testing with $1$
Integral of  $1$ is $x$

```r
integral3 <- 10 - 1
integrateRecursive(function(x) {
    1
}, 1, 10, )
```

```
## [1] 9
```

```r
integrateRecursive(function(x) {
    1
}, 1, 10, ) == integral3
```

```
## [1] TRUE
```





```r
guassQuadrature <- function(f, a = 0, b = 1) {
    z <- c(-0.339981043584856, 0.339981043584856, -0.86113631159053, 0.86113631159053)
    
    w <- c(0.652145154862546, 0.652145154862546, 0.347854845137454, 0.347854845137454)
    
    x <- ((b - a)/2) * z + (a + b)/2
    
    return(((b - a)/2) * sum(w * sapply(x, f)))
}
```



```r
guassQuadrature(function(x) 3 * x^317, 0, 10)  #Above this power, the value of the integral cannot be found
```

```
## [1] 6.466e+307
```



```r
guassQuadrature(function(x) 3 * x^318, 0, 10)
```

```
## [1] Inf
```


```r
guassQuadrature(function(x) sin(x), 0, 0.1)
```

```
## [1] 0.004996
```

```r
cos(0) - cos(0.1)  #Integral of sin
```

```
## [1] 0.004996
```


```r
guassQuadrature(function(x) sin(x), 0, 0.5)
```

```
## [1] 0.1224
```

```r
cos(0) - cos(0.5)  #Integral of sin
```

```
## [1] 0.1224
```


```r
guassQuadrature(function(x) sin(x), 0, 1)
```

```
## [1] 0.4597
```

```r
cos(0) - cos(1)
```

```
## [1] 0.4597
```


```r
guassQuadrature(function(x) sin(x), 0, 1.5)
```

```
## [1] 0.9293
```

```r
cos(0) - cos(1.5)
```

```
## [1] 0.9293
```


```r
guassQuadrature(function(x) sin(x), 0, 2)
```

```
## [1] 1.416
```

```r
cos(0) - cos(2)
```

```
## [1] 1.416
```


```r
guassQuadrature(function(x) sin(x), 0, 2.5)
```

```
## [1] 1.801
```

```r
cos(0) - cos(2.5)  #Integral of sin
```

```
## [1] 1.801
```


```r
guassQuadrature(function(x) sin(x), 0, 3)
```

```
## [1] 1.99
```

```r
cos(0) - cos(3)  #Integral of sin
```

```
## [1] 1.99
```


```r
guassQuadrature(function(x) sin(x), 0, 4)
```

```
## [1] 1.654
```

```r
cos(0) - cos(4)  #Integral of sin
```

```
## [1] 1.654
```


```r
guassQuadrature(function(x) sin(x), 0, 11)
```

```
## [1] 1.427
```

```r
cos(0) - cos(11)  #Integral of sin
```

```
## [1] 0.9956
```


```r
guassQuadrature(function(x) sin(x), 0, 13)
```

```
## [1] -0.3349
```

```r
cos(0) - cos(13)  #Integral of sin
```

```
## [1] 0.09255
```


```r
guassQuadrature(function(x) sin(x), 0, 18)
```

```
## [1] -4.554
```

```r
cos(0) - cos(18)  #Integral of sin
```

```
## [1] 0.3397
```



```r
integrateRecursive2 <- function(f, a = 0, b = 1) {
    
    whole <- guassQuadrature(f, a, b)
    mid <- (a + b)/2
    left <- guassQuadrature(f, a, mid)
    right <- guassQuadrature(f, mid, b)
    if (abs(whole - (left + right)) > 1e-04) {
        left <- integrateRecursive2(f, a, mid)
        right <- integrateRecursive2(f, mid, b)
    }
    return(left + right)
}
```

#Fix down here

```r
plotF <- function(f, a = 0, b = 1) {
    x <- getX(f, a, b)
    plot(x, f(x), pch = 20, cex = 0.2)
    
}
```



```r
simpleRiemann <- function(f, a = 0, b = 1, n = 3) {
    rectangleWidth <- (b - a)/n
    midpoints <- seq(a + rectangleWidth/2, b - rectangleWidth/2, length = n)
    rectangleAreas <- sapply(midpoints, f) * rectangleWidth
    return(sum(rectangleAreas))
}

getX <- function(f, a = 0, b = 1) {
    
    bigBins <- simpleRiemann(f, a = a, b = b, n = 5)
    smallBins <- simpleRiemann(f, a = a, b = b, n = 10)
    
    if (abs(bigBins - smallBins) < 1e-05) {
        return(seq(a, b, length = 10))
    } else {
        mid <- (a + b)/2
        return(c(getX(f, a, b = mid), getX(f, a = mid, b)))
    }
}
```



```r
fun <- function(x) sin(x)
plotF(fun, 0, 10)
```

![plot of chunk unnamed-chunk-31](figure/unnamed-chunk-31.png) 

