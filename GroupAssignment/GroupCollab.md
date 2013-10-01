mydiff<- function(x){
i <- 2:length(x)   
Difference<- x[i] - x[i-1]  
return(Difference)} 

mydiff<- function(x){   
Difference<- x[-1] - x[-length(x)]
return(Difference)}

threeNum <- function(x){
     Stats <- vector(length=3)
     Stats[1] <-c(minimum = min(x))
     Stats[2] <-c(median = median(x))
     Stats[3] <-c(maximum = max(x))
     return(Stats)}

proportionalchange<- function(x,y){
  Difference <- y - x
   PercentageChange <- (Difference/x)*100%
  return(PercentageChange)}

proportionchangetest<-function(x,y)
  Difference <- y - x
   PercentageChange <- (Difference/x)*100%
  return(PercentageChange)
 return(all(PercentageChange > y & Percentage change < x) )
  }
  
 digit2word <- function(v){
   word<-c("zero","one","two","three", "four", "five", "six", "seven","eight","nine")
 return(word[v+1])}

JustConstants<- function(s){
constants<- gsub("[aeiou]", "" ,s)
return(constants)}

piSeries<- function(n){
m<-0:length(n)
Numerator <- (-1)^m
Denominator <- 2*m + 1
fractions <- Numerator/Denominator
sum <- sum(fractions)
return(sum*4)}
