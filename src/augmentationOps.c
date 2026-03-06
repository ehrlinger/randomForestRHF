
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "shared/globalCore.h"
#include           "shared/externalCore.h"
#include           "global.h"
#include           "external.h"

// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "augmentationOps.h"
#include "splitUtil.h"
#include "node.h"
#include "shared/nrutil.h"
AugmentationObj *getAugmentationObj(AugmentationObjCommon *objCommon, uint treeID, Node *parent) {
  AugmentationObj *obj;
  char *perm;
  NodeBase *parentBase;
  double **xArray;
  uint j;
  uint p;
  obj = (AugmentationObj*) gblock((size_t) sizeof(AugmentationObj));
  obj -> common = objCommon;
  p  = objCommon -> pSize;
  xArray = objCommon -> xArray;
  parentBase = (NodeBase*) parent;
  perm = parentBase -> permissible;
  for (j = 1; j <= p; j++) {
    if (perm[j] == TRUE) {
      perm[j] = getVarianceSinglePass(parentBase -> repMembrSize,
                                      parentBase -> repMembrIndx,
                                      xArray[j],
                                      NULL,
                                      NULL);
    }
  }
  return obj;
}
void freeAugmentationObj(AugmentationObj *obj) {
  free_gblock(obj, (size_t) sizeof(AugmentationObj));
}
