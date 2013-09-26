plot(1:20, xlim=c(0,100), ylim=c(0,100), type="n")
DrawCircle<- function(x,y,r,...){
  angles<- seq(from=0, to=2*pi, length= 1000)
  xpts <- x + r*cos(angles)
  ypts <- y + r*sin(angles)
  polygon(xpts, ypts, asp=1, ...)
}

DrawCircle(17,47,10, border="blue") 
DrawCircle(30,38,10, border="yellow") 

plot(1:20, xlim=c(0,100), ylim=c(0,100), type="n")

DrawCircle(17,47,10, border="blue" lwd=7) 
DrawCircle(30,38,10, border="yellow", lwd=7)   
DrawCircle(44,47,10, border="black", lwd=7)
DrawCircle(58,38,10, border="green", lwd=7) 
DrawCircle(72,47,10, border="red", lwd=7)