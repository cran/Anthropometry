#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

#include "cast.h"

static const R_CallMethodDef CallEntries[] = {
    {"DeleteDistOwa",  (DL_FUNC) &DeleteDistOwa,  0},
    {"FillAllDistOwa", (DL_FUNC) &FillAllDistOwa, 9},
    {"GetRowAndFree",  (DL_FUNC) &GetRowAndFree,  1},
    {NULL, NULL, 0}
};

void R_init_Anthropometry(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

