```r
circle <- function(x,y,r){
 angles<- seq(0,(2*pi), length=1000)
 xpts <- x + r*cos(angles)
  ypts <- y + r*sin(angles)
  plot(xpts, ypts, xlim=c(0,40), ylim=c(0,40), type="l", col= "#ee82ee",asp=1)}
 circle(26,20, 10)
 polygon(c(14,25,25,14), c(20,20,31,31))
 polygon(c(13,26,26,13), c(19,19,32,32))
 ```
