#include <R.h>
#include <Rdefines.h>
#include <stdlib.h>
#include <math.h>

int compareDouble(const void* a, const void* b)
{
    double aa = *(double *)a; 
    double bb = *(double *)b; 
    if (aa < bb) return 1;
    if (aa > bb) return -1;
    return 0;
}

static int filled=0;

struct simmat
{
 double **dv;
 int msize;
} d;

SEXP Dist(SEXP person_i,SEXP person_j)
{
 SEXP ret;
 int pi,pj;
 double *r;

 pi = INTEGER_VALUE(person_i);
 pj = INTEGER_VALUE(person_j);
 
 PROTECT(ret = NEW_NUMERIC(1));
 r = NUMERIC_POINTER(ret);
 *r=(pi<=pj) ? d.dv[pi][pj-pi] : d.dv[pj][pi-pj];
 UNPROTECT(1);
 return(ret);
}

SEXP GetRowAndFree(SEXP erow)
{
 SEXP ret;
 double *r;
 int j,row;
 
 row = INTEGER_VALUE(erow);

 if ((row<0) || (row>=d.msize))
 {
  return(R_NilValue);
 }

 PROTECT(ret = NEW_NUMERIC(d.msize-row));
 r = NUMERIC_POINTER(ret);
 for (j=0;j<d.msize-row;j++)
  *(r+j)=d.dv[row][j];
 UNPROTECT(1);
 free(d.dv[row]);
 d.dv[row]=NULL;
 return(ret);
}
   
SEXP FillAllDistOwa(SEXP ex,SEXP ew,SEXP envars,SEXP enpers,SEXP eal,SEXP eah,SEXP ebl,SEXP ebh,SEXP verbose)
{
 int nvars,npers;
 double *xt,*w,*al,*ah,*bl,*bh,**x,dist,*adist,l;
 int i,j,k,j1,total_size,pr,step,*r;
 SEXP ret;

 PROTECT( envars = AS_INTEGER(envars) );
 PROTECT( enpers  = AS_INTEGER(enpers) );
 PROTECT(  ex = AS_NUMERIC(ex) );
 PROTECT(  ew = AS_NUMERIC(ew) );
 PROTECT( eal = AS_NUMERIC(eal) );
 PROTECT( eah = AS_NUMERIC(eah) );
 PROTECT( ebl = AS_NUMERIC(ebl) );
 PROTECT( ebh = AS_NUMERIC(ebh) );
 PROTECT( verbose = AS_INTEGER(verbose) );

 nvars = INTEGER_POINTER(envars)[0];
 npers = INTEGER_POINTER(enpers)[0]; 
 xt = NUMERIC_POINTER(ex);
 w  = NUMERIC_POINTER(ew);
 al = NUMERIC_POINTER(eal);
 ah = NUMERIC_POINTER(eah);
 bl = NUMERIC_POINTER(ebl);
 bh = NUMERIC_POINTER(ebh);
 pr = INTEGER_POINTER(verbose)[0];

 x=calloc(npers,sizeof(double *));
 for (i=0;i<npers;i++)
  x[i]=xt+(i*nvars);

 adist=calloc(nvars,sizeof(double));
 
 if (filled)
 {
  PROTECT(ret = NEW_INTEGER(1));
  r = INTEGER_POINTER(ret);
  *r=1;
  UNPROTECT(1);
  return(ret);
 }

 d.msize=npers;

 d.dv=calloc(npers,sizeof(double *));
 if (d.dv==NULL)
 {
  PROTECT(ret = NEW_INTEGER(1));
  r = INTEGER_POINTER(ret);
  *r=2;
  UNPROTECT(1);
  return(ret);
 }

 total_size=0;
 for (i=0;i<npers;i++)
 {
  d.dv[i]=calloc(npers-i,sizeof(double));
  total_size+=(npers-i);
  if (d.dv[i]==NULL)
  {
   PROTECT(ret = NEW_INTEGER(1));
   r = INTEGER_POINTER(ret);
   *r=3;
   UNPROTECT(1);
   return(ret);
  }
 }
 filled=1;
 if (pr!=0)
 {
  step=npers/100;
  if (step==0)
   step=1;
 }
 for (i=0;i<npers;i++)
 {
  j1=0;
  d.dv[i][j1]=0.0;
  j1++;
  
  for (j=i+1;j<npers;j++)
  {
   for (k=0;k<nvars;k++)
   {
    l=log(x[i][k]/x[j][k]);

    adist[k]=0.0;
   
    if (l<bl[k])
     adist[k]=-(l-bl[k])*al[k];

    if (l>bh[k])
     adist[k]=(l-bh[k])*ah[k];
   }

   qsort(adist, nvars-1, sizeof(double), compareDouble);

   dist=0.0;
   for (k=0;k<nvars && adist[k]!=0.0;k++)
    dist+=adist[k]*w[k];

   d.dv[i][j1]=dist;
   j1++;
  }
 }
 
 if (x!=NULL)
  free(x);
 if (adist!=NULL)
  free(adist);
 
 PROTECT(ret = NEW_INTEGER(1));
 r = INTEGER_POINTER(ret);
 *r=0;
 
 unprotect(10);

 return(ret);
}

SEXP DeleteDistOwa(void)
{
 int i;

 if (filled)
 {
  for (i=0;i<d.msize;i++)
   if (d.dv[i])
    free(d.dv[i]);
  free(d.dv);
  d.dv=NULL;
  d.msize=0;
  filled=0;
 }

 return(R_NilValue);
}
