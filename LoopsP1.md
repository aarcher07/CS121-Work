findScrabble<- function(letters){
for (k in 1:length(letters)){
words<-words[grep(letters[k],words)]
if (length(words)=]<10) break 
#If is a conditional statement
#the break stops the loop 
}
return(words)
}
#Way 1 for Fibacci Numbers
FibacciSeq<- function(n){
 current <- 1
 beforeIt<- 0
 for (k in 1:n){
 tmp <- current+beforeIt
 beforeIt<- current
 current <- tmp
 }
 return(current)
 }



