
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "shared/globalCore.h"
#include           "shared/externalCore.h"
#include           "global.h"
#include           "external.h"

// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "augmentationOpsSimple.h"
#include "node.h"
#include "shared/nrutil.h"
AugmentationObj *getAugmentationObjSimple(AugmentationObjCommon *objCommon, uint treeID, Node *parent) {
  AugmentationObj *obj;
  obj = (AugmentationObj*) gblock((size_t) sizeof(AugmentationObj));
  obj -> perm = NULL;
  obj -> common = objCommon;
  return obj;
}
