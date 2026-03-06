
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "shared/globalCore.h"
#include           "shared/externalCore.h"
#include           "global.h"
#include           "external.h"

// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "augmentationOpsCommon.h"
#include "splitUtil.h"
#include "node.h"
#include "shared/nrutil.h"
#include "shared/factorOps.h"
AugmentationObjCommon *getAugmentationObjCommon(uint n, uint p, double **xArrayIn, char *permIn, double **yArrayIn) {
  AugmentationObjCommon *obj;
  char  *perm;
  uint j;
  obj = (AugmentationObjCommon*) gblock((size_t) sizeof(AugmentationObjCommon));
  obj -> nSize = n;
  obj -> pSize = p;
  obj -> xArray = xArrayIn;
  obj -> yArray = yArrayIn;
  obj -> fnSize = 0;
  obj -> fxArray = NULL;
  obj -> fyArray = NULL;
  perm = obj -> perm = cvector(1, p);
  for (j = 1; j <= p; j++) {
    perm[j] = permIn[j];    
    if (perm[j]) {
      perm[j] = getVarianceSinglePass(RF_identityMembershipIndexSize,  
                                      RF_identityMembershipIndex,      
                                      xArrayIn[j],
                                      NULL,
                                      NULL);
    }
  }
  return obj;
}
void getAugmentationObjCommonTest(AugmentationObjCommon *obj,
                                  uint     fn,
                                  double **fxArrayIn,
                                  double **fyArrayIn) {
  obj -> fxArray = fxArrayIn;
  obj -> fyArray = fyArrayIn;
  obj -> fnSize  = fn;
}
void freeAugmentationObjCommon(AugmentationObjCommon *obj) {
  free_cvector(obj -> perm, 1, obj -> pSize);
  free_gblock(obj, (size_t) sizeof(AugmentationObjCommon));
}
