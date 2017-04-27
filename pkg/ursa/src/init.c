#include <R.h>
#include <R_ext/Rdynload.h>
#include "ursa.h"

static const R_CMethodDef CEntries[] = {
   {"groupSummary", (DL_FUNC) &groupSummary, 6},
   {"focalExtrem", (DL_FUNC) &focalExtrem, 10},
   {"focalLoG", (DL_FUNC) &focalLoG, 11},
   {"timefilt4", (DL_FUNC) &timefilt4, 5},
   {"table_log", (DL_FUNC) &table_log, 3},
   {"focalSobelG", (DL_FUNC) &focalSobelG, 11},
   {"variability4", (DL_FUNC) &variability4, 5},
   {"focalCommon", (DL_FUNC) &focalCommon, 10},
   {"progressBar", (DL_FUNC) &progressBar, 3},
   {"optimalDatatypeInt", (DL_FUNC) &optimalDatatypeInt, 3},
   {"readBilLineDouble2", (DL_FUNC) &readBilLineDouble2, 7},
   {"areaIncrement", (DL_FUNC) &areaIncrement, 4},
   {"ffocal4", (DL_FUNC) &ffocal4, 9},
   {"dist2dist", (DL_FUNC) &dist2dist, 10},
   {"readBsqBandInteger", (DL_FUNC) &readBsqBandInteger, 7},
   {"readBsqLineDouble", (DL_FUNC) &readBsqLineDouble, 7},
   {"aggregate", (DL_FUNC) &aggregate, 6},
   {"focalGaussian", (DL_FUNC) &focalGaussian, 11},
   {"readBilBandInteger", (DL_FUNC) &readBilBandInteger, 7},
   {"readBilLineDouble", (DL_FUNC) &readBilLineDouble, 7},
   {"reclassify", (DL_FUNC) &reclassify, 6},
   {"optimalDatatypeDouble", (DL_FUNC) &optimalDatatypeDouble, 3},
   {"focalMeanWithNA", (DL_FUNC) &focalMeanWithNA, 7},
   {"readBilLineInteger2", (DL_FUNC) &readBilLineInteger2, 7},
   {"interp4", (DL_FUNC) &interp4, 5},
   {"focal4", (DL_FUNC) &focal4, 9},
   {"focalMean", (DL_FUNC) &focalMean, 9},
   {"internalMargin", (DL_FUNC) &internalMargin, 4},
   {"expand", (DL_FUNC) &expand, 5},
   {"readBilBandDouble", (DL_FUNC) &readBilBandDouble, 7},
   {"focalLaplacian", (DL_FUNC) &focalLaplacian, 11},
   {"makeField", (DL_FUNC) &makeField, 3},
   {"resampl4", (DL_FUNC) &resampl4, 10},
   {"rasterize", (DL_FUNC) &rasterize, 9},
   {"makemap4", (DL_FUNC) &makemap4, 7},
   {"readBsqBandDouble", (DL_FUNC) &readBsqBandDouble, 7},
   {"focalCorrel", (DL_FUNC) &focalCorrel, 11},
   {"readBsqLineInteger", (DL_FUNC) &readBsqLineInteger, 7},
   {"isNear", (DL_FUNC) &isNear, 5},
   {"focalMedian", (DL_FUNC) &focalMedian, 9},
   {"focalOsisaf", (DL_FUNC) &focalOsisaf, 11},
   {"focalSobel", (DL_FUNC) &focalSobel, 11},
   {"focalHires", (DL_FUNC) &focalHires, 11},
   {"writeBilBandInteger", (DL_FUNC) &writeBilBandInteger, 7},
   {"writeBilBandDouble", (DL_FUNC) &writeBilBandDouble, 7},
   {"bilinear", (DL_FUNC) &bilinear, 5},
   {"readBilLineInteger", (DL_FUNC) &readBilLineInteger, 7},
   {"conTest", (DL_FUNC) &conTest, 2},
   {NULL, NULL, 0}
};

void R_init_ursa(DllInfo *dll)
{
   R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
   R_useDynamicSymbols(dll, FALSE);
   R_forceSymbols(dll, TRUE);
}
