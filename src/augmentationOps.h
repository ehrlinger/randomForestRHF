#ifndef RF_AUGMENTATION_OPS_H
#define RF_AUGMENTATION_OPS_H
#include "augmentation.h"
AugmentationObj *getAugmentationObj(AugmentationObjCommon *objCommon, uint treeID, struct node *parent);
void freeAugmentationObj(AugmentationObj *obj);
#endif
