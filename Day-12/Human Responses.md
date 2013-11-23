

```r

testlatency <- function(N = 20) {
    
    result <- vector(length = N)
    time <- rexp(N, rate = 1/2)
    for (k in 1:N) {
        
        before <- Sys.time()
        readline("Press return")
        cat(rep("\n", 3))
        after <- Sys.time()
        diff <- after - before
        result[k] <- diff
        Sys.sleep(time[k])
    }  #runif is a random number generator
    
    table <- data.frame(`Time Interval` = time, Latency = result)
    return(table)
}
```



```r
load("DorisData.RData")
plot(DorisData$Time.Interval, DorisData$Latency, xlim = c(0, 2), ylim = c(0, 
    5))
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

