screeArchetyp <- function(k,rss_lass_def,rss_step,rss_step_which,ylim,main,xlab,ylab,col=c("red","blue"),
                          axis2,seq,leg){
 plot(1:k,rss_lass_def,xaxt="n",yaxt="n",ylim=ylim,main=main,xlab=xlab,ylab=ylab,type="b") 
 points(1:k,rss_step,type="b",col=col[1]) 
 points(1:k,rss_step_which,type="b",col=col[2],lty=2)
 axis(1,at=1:k,labels=1:k)
 if(axis2){                          
  axis(2,at=seq,labels=seq)
 }
 if(leg){
  legend("topright",c("Archetypes","Archetypoids from nearest","Archetypoids from which"),lty = c(1,1,2),
         col=c("black",col[1],col[2]),text.col=c("black",col[1],col[2]))
 }
}