counter.balanced.median<- function(x){
  len<-length(x)
  x<-sort(x)
  x<-data.frame(x)
  x$row<-row.names(x)
  med<-median(x$x)
#test if the calculated median is part of the set, and if not
#return the calculated median.
  
  if(identical(as.numeric(x[x$x==med,2]),numeric(0))) return(med)
  
#find the range of the median
  
  Min<-min(as.numeric(x[x$x==med,2]))
  Max<-max(as.numeric(x[x$x==med,2]))
#Where is the center of the medial range?
  if(abs(Min-(len+1)/2)>abs(Max-(len+1)/2)){
#What is the number next to the median?
    temp1<-x[Max+1,1]
#How far does that number go?
    temp2<-max(as.numeric(x[x$x==temp1,2]))
#What is the counterbalence to the mean?
    temp3<-len+1-Min
#Which is smaller?
    cb2<-min(c(temp2,temp3))
#What is the counter balence to that number(cb2)?
    cb1<-len+1-cb2
  }else{
    temp1<-x[Min-1,1]
    temp2<-min(as.numeric(x[x$x==temp1,2]))
    temp3<-len+1-Max
    cb1<-max(c(temp2,temp3))
    cb2<-len+1-cb1
     }
  cb<-mean(x[cb1:cb2,1])
  cb
}