outlier<- function(x){
  quartile2<- median(x)
  quartilerange1<- NULL
  quartilerange3<- NULL 
  for ( i in 1:length(x)) {
    if (x[i] < quartile2) quartilerange1<- quartilerange1+ x[i]
    if (x[i] > quartile2) quartilerangle3<- quartilerange3 + x[i]
  }
  quartile1<- median(quartilerange1)
  quartile3<- median(quartilerange3)
  for (i in 1:length(x)){
    if ((x < quartile1)|(x > quartile3)) 
      datatable<- data.frame(list(Numbers= x[i], Outliers=c("YES")))
    if ((x > quartile1) & (x < quartile3))
      datatable<- data.frame(list(Numbers=c(x[i]), Outliers=c("NO")))
  }
  return(datatable)
}