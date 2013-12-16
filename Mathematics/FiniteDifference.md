
Finding the derivative of $x^3 + 2x^2 +3$

```r
evalD <- function(f, x) {
    h <- 0.001
    return((f(x + h) - f(x - h))/(2 * h))
}
```



```r
fprime <- evalD(function(x) {
    x^3 + 2 * x^2 + 3
}, 0:10)
```



```r
Df <- function(x) {
    return(3 * x^2 + 4 * x)
}
```


```r
plot(1:2, ylim = c(0, 350), xlim = c(0, 10), xlab = "x", ylab = "y'", type = "n")
lines(0:10, fprime, col = "blue", lwd = 3)
lines(0:10, Df(0:10), col = "red", lwd = 2)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 



```r
Difference <- Df(0:10) - fprime
return(Difference)
```

```
##  [1] -1.000e-06 -1.000e-06 -1.000e-06 -1.000e-06 -1.000e-06 -1.000e-06
##  [7] -1.000e-06 -1.000e-06 -1.000e-06 -9.998e-07 -9.998e-07
```

```r

plot(y = Difference, x = 0:10, type = "l")  #The difference between the symbolic and finite-difference derviative is neglible, so it is a good approximation.
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 



```r
evalDh <- function(f, x, h) {
    # Adjusting EvalD to take h has an argument
    return((f(x + h) - f(x - h))/(2 * h))
}
```




```r
PlotDev <- function() {
    plot(1:2, ylim = c(0, 350), xlim = c(0, 10), xlab = "x", ylab = "y'", type = "n")
    lines(0:10, evalDh(function(x) {
        x^3 + 2 * x^2 + 3
    }, 0:10, 1), col = "blue")
    lines(0:10, evalDh(function(x) {
        x^3 + 2 * x^2 + 3
    }, 0:10, 0.1), col = "green")
    lines(0:10, evalDh(function(x) {
        x^3 + 2 * x^2 + 3
    }, 0:10, 1e-05), col = "orange")
    lines(0:10, evalDh(function(x) {
        x^3 + 2 * x^2 + 3
    }, 0:10, 1e-10), col = "red")
    lines(0:10, evalDh(function(x) {
        x^3 + 2 * x^2 + 3
    }, 0:10, 1e-15), col = "purple")
    lines(0:10, evalDh(function(x) {
        x^3 + 2 * x^2 + 3
    }, 0:10, 1e-20), col = "tomato")
}
```



```r
PlotDev()
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 



```r
Difference2 <- function() {
    diff1 <- Df(0:10) - evalDh(function(x) {
        x^3 + 2 * x^2 + 3
    }, 0:10, 1)
    diff2 <- Df(0:10) - evalDh(function(x) {
        x^3 + 2 * x^2 + 3
    }, 0:10, 0.1)
    diff3 <- Df(0:10) - evalDh(function(x) {
        x^3 + 2 * x^2 + 3
    }, 0:10, 1e-05)
    diff4 <- Df(0:10) - evalDh(function(x) {
        x^3 + 2 * x^2 + 3
    }, 0:10, 1e-10)
    diff5 <- Df(0:10) - evalDh(function(x) {
        x^3 + 2 * x^2 + 3
    }, 0:10, 1e-15)
    diff6 <- Df(0:10) - evalDh(function(x) {
        x^3 + 2 * x^2 + 3
    }, 0:10, 1 - 20)
    return(list(hEquals1 = c(diff1), hEquals0.1 = c(diff2), hEquals5 = c(diff3), 
        hEquals10 = c(diff4), hEquals10 = c(diff5), hequals20 = c(diff6)))  #1e-5 is the best
}
```

 

 Task 3 #This is incomplete
When $x=0$, 1e-8(abs(x)) will be 0.If $2*h$ is defined as 1e-8*abs(x) and $x = 0$, the numerator will be then divided by 0. As result, the derivative will be undefined.

When h is defined by 1e-8*pmax(1,abs(x)) and $x=0$, h will become 1. The derivative will then no longer be a undefined when x=0.

 Task 4

```r
myD <- function(f) {
    return(function(x) evalD(f, x))
}
```


#Example

```r
FirstDev <- myD(sin)
FirstDev(3.14/2)
```

```
## [1] 0.0007963
```

```r

# Since this returns a value(which is equal to sin(3.14/2)), myD(sin)
# returns a function.

# myD 'remembers' f only when the function has been assigned to a variable
# and then that variable will act as a function.

# In the myD function we told the computer to return a function of x. The
# function, evalD(f,x), will then only accept a single numerical argument,x,
# and return the derivative at the point in the function,f.
```



```r
SecDev <- myD(myD(sin))
SecDev(3.14/2)  #This should be approximately equal to -1.
```

```
## [1] -1
```

```r

ThirdDev <- myD(myD(myD(sin)))
ThirdDev(3.14/2)  #This should be approximately 0, but approaches 0 from a negative value.
```

```
## [1] -0.0007963
```

```r

plot(1:2, xlim = c(0, 10), ylim = c(-2, 2), type = "n")
lines(1:10, SecDev(1:10))
lines(1:10, ThirdDev(1:10))
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 



```r
dddf <- D(sin(x^2) ~ x & x & x)
```

```
## Error: argument "name" is missing, with no default
```

```r
plot(1:2, xlim = c(20, 20), ylim = c(-10, 10), type = "n")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 

```r
lines(1:10, dddf(1:10))
```

```
## Error: could not find function "dddf"
```



```r
myD2 <- function(f) {
    h <- 1e-05
    return(function(x) {
        ((f(x + h) - 2 * f(x) + f(x - h))/h^2)
    })
}
```



```r
SinD2 <- myD2(sin)
SinD2(1.57)
```

```
## [1] -1
```

