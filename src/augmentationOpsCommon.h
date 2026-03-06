#ifndef RF_AUGMENTATION_OPS_COMMON_H
#define RF_AUGMENTATION_OPS_COMMON_H
#include "augmentation.h"
AugmentationObjCommon *getAugmentationObjCommon(uint n, uint p, double **xArrayIn, char *permIn, double **yArrayIn);
void getAugmentationObjCommonTest(AugmentationObjCommon *obj, uint fn, double **fxArrayIn, double **fyArrayIn);
void freeAugmentationObjCommon(AugmentationObjCommon *obj);
#endif
