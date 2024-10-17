data=data.frame(xi=c(5,6,9,11),yi=c(12,13,14,16))


px<-function(x,xi,yi){
  result=0
  if(length(xi)!=length(yi)) return("argument of unequal length")
  
  for(i in 1:length(xi)){
    temp=1
    for(j in 1:length(xi)){
      if(j==i) next
      temp=temp*(x-xi[j])/(xi[i]-xi[j])
      
    }
    temp=temp*yi[i]
    result=result+temp
    
  }
  return(result)
}

px(10,data$xi,data$yi)

#plotting the polynomial we got

plot(data$xi,data$yi,col="red",pch=19,,cex=1.3,xlim=c(-5,20),ylim=c(-20,20),xlab="xi",ylab="yi from px")
lines(-20:20,px(-20:20,data$xi,data$yi))

#data of temperature vs pressure
data2=data.frame(temp=c(361,367,378,387,399),pressure=c(154.9,167,191,212.5,399))
data2

px(371.2,data2$temp,data2$pressure)

#plotting
plot(data2$temp,data2$pressure,col="red",pch=19,cex=1.3,ylab="pressure",xlab="temperature")
lines(300:400,px(300:400,data2$temp,data2$pressure),col="blue",lwd=2)
