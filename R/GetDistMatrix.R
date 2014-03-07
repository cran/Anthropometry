GetDistMatrix <- function(x,np,nv,w,bl,bh,al,ah,progress){

 if ( (!is.numeric(np)) || (np!=as.integer(np)) )
 {
  cat("GetDistMatrix Error: np is not an integer.\n");
  break;
 }
 if ( (!is.numeric(nv)) || (nv!=as.integer(nv)) )
 {
  cat("GetDistMatrix Error: nv is not an integer.\n");
  break;
 }
 if (length(x)!=(np*nv))
 {
  cat("GetDistMatrix Error: x is not a vector of",np*nv,"components.\n");
  break;
 }
 if ((length(bh)!=nv) || (length(bl)!=nv) || (length(ah)!=nv) || (length(al)!=nv))
 {
  cat("GetDistMatrix Error: any of the vectors bh,bl,ah or al has not the correct length (it must be ",nv,")\n");
  break;
 }
 if (sum(bl<0)!=nv)
 {
  cat("GetDistMatrix Error: any of the elements of bl is not strictly negative.\n");
  break;
 }
 if (sum(bh>0)!=nv)
 {
  cat("GetDistMatrix Error: any of the elements of bh is not strictly positive.\n");
  break;
 }
 if (sum(al>0)!=nv)
 {
  cat("GetDistMatrix Error: any of the elements of al is not strictly positive (remember: the sign changes inside).\n");
  break;
 }
 if (sum(ah>0)!=nv)
 {
  cat("GetDistMatrix Error: any of the elements of ah is not strictly positive.\n");
  break;
 }
 if (progress)
 {
  t1=Sys.time();
 }
 errorfill=0;
 errorfill=.Call("FillAllDistOwa",x,w,nv,np,al,ah,bl,bh,progress);
 if (errorfill==1)
 {
  cat("Error: the dissimilarity symmetric matrix was reserved but not released.\n");
  break;
 }
 if (errorfill==2)
 {
  cat("Error: it was not possible to reserve memory not even for the pointers to the rows of the dissimilarity matrix.\n");
  break;
 }
 if (errorfill==3)
 {
  cat("Error: it was not possible to reserve memory for all or part of the dissimilarity matrix.\n");
  break;
 }
 if (progress)
 {
  t2=Sys.time();
  cat("Time spent:",difftime(t2,t1,units="secs"),"seconds.\n");
 }

 if (progress)
 {
  cat("Reserving space for d...\n");
 }

 d=rep(0,np*np);
 dim(d)<-c(np,np);

 if (progress)
 {
  cat("Done\n");
  cat("Returning the distance matrix...\n");
  cat("Row:\n0 ");
  t1=Sys.time();
 }

 for ( row in (1:np) )
 {
  retval=.Call("GetRowAndFree",row-1);
  if (is.null(retval))
  {
   cat("Error calling GetRowAndFree: trying to access a non-existent matrix row.\n");
   break;
  }
  d[row,row:np]=retval;
  if ((progress) && (row==100*as.integer(row/100)))
  {
   cat(row," ");
  }
 }

 if (progress)
 {
  t2=Sys.time();
  cat("\nTime spent::",difftime(t2,t1,units="secs"),"seconds.\n");
 }

 .Call("DeleteDistOwa");

 if (progress)
 {
  cat("Making the matrix symmetrical ...\n");
 }
 for ( row in (2:np) )
 {
  d[row,1:(row-1)]=d[1:(row-1),row];
 }
 if (progress)
 {
  cat("Done.\n");
 }
 d
}

