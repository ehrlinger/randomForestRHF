#ifndef RF_AUGMENTATION_H
#define RF_AUGMENTATION_H
typedef struct augmentationObjCommon AugmentationObjCommon;
struct augmentationObjCommon {
  uint pSize;
  uint nSize;
  char *perm;
  double **xArray;
  double **yArray;
  uint     fnSize;
  double **fxArray;
  double **fyArray;
};
typedef struct augmentationObj AugmentationObj;
struct augmentationObj {
  char *perm;
  struct augmentationObjCommon *common;
};
#endif 
