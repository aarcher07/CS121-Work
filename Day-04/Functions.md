CountsOdd<-function(x){
Remainder<- vector(length=length(x))
Remainder <- x%%2
count <- sum(Remainder)
return(count)}
Counts(c(3,4,2,4))

 CountEvens<- function(x){
 TotalCount<- length(x)
 Remainder<- vector(length=length(x))
 Remainder <- x%%2
 Oddcount <- sum(Remainder)
 Evencount<- TotalCount-Oddcount
 return(Evencount)}
 
 CountEvens<- function(x){
  x <- x+1
  Remainder<- vector(length=length(x))
Remainder <- x%%2
Evencount <- sum(Remainder)
return(Evencount)}

hypotenuseLength<- function(a,b) {
asquared <- a^2
bsquared <- b^2
hypotenusesquared <- asquared + bsquared
hypotenuse<- (hypotenusesquared)^(1/2)
return(hypotenuse)
}

LawofCosines<- function(a,b,theta){
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

ThetaFromLengths<-function(a,b,c){
asqred <- a^2
bsqred <- b^2
csqred <- c^2
Sum <- asqred + bsqred - csqred
Divisor<- 2%*%a%*%b
quotient <- Sum/Divisor
angles <- acos(quotient)
return(angles)}

