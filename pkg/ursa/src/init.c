#include <R.h>
#include <R_ext/Rdynload.h>
#include "ursa.h"

static const R_CMethodDef CEntries[] = {
   {"focalGaussian", (DL_FUNC) &focalGaussian, 11},
   {"makemap4", (DL_FUNC) &makemap4, 7},
   {"makeField", (DL_FUNC) &makeField, 3},
   {"focalCommon", (DL_FUNC) &focalCommon, 10},
   {"rasterize", (DL_FUNC) &rasterize, 9},
   {"aggregate", (DL_FUNC) &aggregate, 6},
   {"internalMargin", (DL_FUNC) &internalMargin, 4},
   {"focalHires", (DL_FUNC) &focalHires, 11},
   {"readBilBandDouble", (DL_FUNC) &readBilBandDouble, 7},
   {"focalLoG", (DL_FUNC) &focalLoG, 11},
   {"readBsqLineInteger", (DL_FUNC) &readBsqLineInteger, 7},
   {"variability4", (DL_FUNC) &variability4, 5},
   {"focalMedian", (DL_FUNC) &focalMedian, 9},
   {"focalCorrel", (DL_FUNC) &focalCorrel, 11},
   {"focalLaplacian", (DL_FUNC) &focalLaplacian, 11},
   {"writeBilBandInteger", (DL_FUNC) &writeBilBandInteger, 7},
   {"interp4", (DL_FUNC) &interp4, 5},
   {"readBilLineDouble2", (DL_FUNC) &readBilLineDouble2, 7},
   {"areaIncrement", (DL_FUNC) &areaIncrement, 4},
   {"readBsqBandDouble", (DL_FUNC) &readBsqBandDouble, 7},
   {"conTest", (DL_FUNC) &conTest, 2},
   {"optimalDatatypeDouble", (DL_FUNC) &optimalDatatypeDouble, 3},
   {"reclassify", (DL_FUNC) &reclassify, 6},
   {"focalMean", (DL_FUNC) &focalMean, 9},
   {"readBsqBandInteger", (DL_FUNC) &readBsqBandInteger, 7},
   {"readBsqLineDouble", (DL_FUNC) &readBsqLineDouble, 7},
   {"ffocal4", (DL_FUNC) &ffocal4, 9},
   {"groupSummary", (DL_FUNC) &groupSummary, 6},
   {"isNear", (DL_FUNC) &isNear, 5},
   {"progressBar", (DL_FUNC) &progressBar, 3},
   {"focalSobel", (DL_FUNC) &focalSobel, 11},
   {"focalExtrem", (DL_FUNC) &focalExtrem, 10},
   {"focalMeanWithNA", (DL_FUNC) &focalMeanWithNA, 7},
   {"readBilLineInteger2", (DL_FUNC) &readBilLineInteger2, 7},
   {"bilinear", (DL_FUNC) &bilinear, 5},
   {"writeBilBandDouble", (DL_FUNC) &writeBilBandDouble, 7},
   {"dist2dist", (DL_FUNC) &dist2dist, 10},
   {"expand", (DL_FUNC) &expand, 5},
   {"readBilLineInteger", (DL_FUNC) &readBilLineInteger, 7},
   {"readBilLineDouble", (DL_FUNC) &readBilLineDouble, 7},
   {"timefilt4", (DL_FUNC) &timefilt4, 5},
   {"readBilBandInteger", (DL_FUNC) &readBilBandInteger, 7},
   {"focalSobelG", (DL_FUNC) &focalSobelG, 11},
   {"focalOsisaf", (DL_FUNC) &focalOsisaf, 11},
   {"table_log", (DL_FUNC) &table_log, 3},
   {"resampl4", (DL_FUNC) &resampl4, 10},
   {"optimalDatatypeInt", (DL_FUNC) &optimalDatatypeInt, 3},
   {"focal4", (DL_FUNC) &focal4, 9},
   {NULL, NULL, 0}
};

void R_init_ursa(DllInfo *dll)
{
   R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
   R_useDynamicSymbols(dll, FALSE);
   R_forceSymbols(dll, TRUE);
}
