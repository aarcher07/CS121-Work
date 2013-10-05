outlier<- function(x){
  quartile2<- median(x)
  quartile1 <- median(x[x<=quartile2])
  quartile3 <- median(x[x>=quartile2])
  
  
#   for ( i in 1:length(x)) {
#     if (x[i] < quartile2) quartilerange1<- quartilerange1+ x[i]
#     if (x[i] > quartile2) quartilerangle3<- quartilerange3 + x[i]
#   }
#   quartile1<- median(quartilerange1)
#   quartile3<- median(quartilerange3)
  
  low <- quartile1 - 1.5*(quartile3 - quartile1)
  high<- quartile3 + 1.5*(quartile3 - quartile1)
  
  return(x[x < low | x > high])
  
  for (i in 1:length(x)){
    if ((x < quartile1)|(x > quartile3)) 
      datatable<- data.frame(list(Numbers= x[i], Outliers=c("YES")))
    if ((x > quartile1) & (x < quartile3))
      datatable<- data.frame(list(Numbers=c(x[i]), Outliers=c("NO")))
  }
  return(datatable)
}