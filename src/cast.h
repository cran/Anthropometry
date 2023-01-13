#ifndef _CAST_H
#define _CAST_H

#include <R.h>
#include <Rdefines.h>

int compareDouble(const void* a, const void* b);

SEXP Dist(SEXP person_i,SEXP person_j);

SEXP GetRowAndFree(SEXP erow);

SEXP FillAllDistOwa(SEXP ex,SEXP ew,SEXP envars,SEXP enpers,SEXP eal,SEXP eah,SEXP ebl,SEXP ebh,SEXP verbose);

SEXP DeleteDistOwa(void);

#endif

