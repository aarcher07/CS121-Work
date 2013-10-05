Graphics
==================

## Black Canvas

```r
canvas <- function(min, max) {
    plot(1:2, type = "n", xlim = c(min, max), ylim = c(min, max))
}
canvas(min = 0, max = 100)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 



```r
DrawCircle <- function(x, y, r, ...) {
    angles <- seq(from = 0, to = 2 * pi, length = 1000)
    xpts <- x + r * cos(angles)
    ypts <- y + r * sin(angles)
    polygon(xpts, ypts, asp = 1, ...)
}
```


## Overlapping Circles

```r
canvas(min = 0, max = 100)
DrawCircle(17, 47, 10, border = "blue")
DrawCircle(30, 38, 10, border = "yellow")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


## Olympic Symbol

```r
canvas(min = 0, max = 100)
DrawCircle(17, 47, 10, border = "blue", lwd = 7)
DrawCircle(30, 38, 10, border = "yellow", lwd = 7)
DrawCircle(44, 47, 10, border = "black", lwd = 7)
DrawCircle(58, 38, 10, border = "green", lwd = 7)
DrawCircle(72, 47, 10, border = "red", lwd = 7)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

