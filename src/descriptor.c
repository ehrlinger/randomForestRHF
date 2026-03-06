
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "shared/globalCore.h"
#include           "shared/externalCore.h"
#include           "global.h"
#include           "external.h"

// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "descriptor.h"
#include "shared/nrutil.h"
#include "shared/error.h"
DescriptorObj *makeDescriptorObj(uint xSize, uint pSize, int descID, char socketState) {
  DescriptorObj *obj = (DescriptorObj *) gblock((size_t) sizeof(DescriptorObj));
  obj -> fwdLink = obj -> bakLink = NULL;
  obj -> record = cvector(0, SG_TCP_BUFFER_SIZE - 1);
  obj -> len = 0;
  obj -> nSizeAlloc = 0;
  obj -> nSize = 0;
  obj -> nIndx = NULL;
  obj -> nSizeRead = 0;
  obj -> mSize = 0;
  obj -> mIndx = NULL;
  obj -> nSent = 0;
  obj -> xSize = xSize;
  obj -> pSize = pSize;  
  obj -> augmSize = pSize - xSize;
  obj -> yHat = NULL;
  obj -> fxArrayIn = NULL;
  obj -> fxArrayInAugm = NULL;
  obj -> fyArrayIn = NULL;
  obj -> userState = SG_DESC_OPENED;
  obj -> descID = descID;
  obj -> socketState = socketState;
  obj -> yHatPacket = (PredictorPacket *) gblock((size_t) sizeof(PredictorPacket));  
  return obj;
}
void setDescriptorObj(DescriptorObj *obj, uint nSizeAlloc, char respInFlag) {
  uint i;
  obj -> nSizeAlloc = nSizeAlloc;
  obj -> fxArrayIn = new_vvector(1, obj -> pSize, NRUTIL_DPTR);
  for (i = 1; i <= obj -> pSize; i++) {
    obj -> fxArrayIn[i] = dvector(1, obj -> nSizeAlloc);
  }
  if (obj -> augmSize > 0) {
    obj -> fxArrayInAugm = new_vvector(1, obj -> augmSize, NRUTIL_DPTR);
    for (i = 1; i <= obj -> augmSize; i++) {
      obj -> fxArrayInAugm[i] = dvector(1, obj -> nSizeAlloc);
    }
  }
  if (respInFlag) {
    obj -> fyArrayIn = dvector(1, obj -> nSizeAlloc);
  }
  obj -> yHat = dvector(1, obj -> nSizeAlloc);
  obj -> nIndx = uivector(1, obj -> nSizeAlloc);
  obj -> mIndx = uivector(1, obj -> nSizeAlloc);
  for (i = 1; i <= obj -> nSizeAlloc; i++) {
    obj -> yHat[i] = RF_nativeNaN;
    obj -> nIndx[i] = 0;
    obj -> mIndx[i] = 0;
  }
}
void resetDescriptorObj(DescriptorObj *obj, char socketState) {
  uint i;
  if (obj -> fxArrayIn != NULL) {
    for (i = 1; i <= obj -> xSize; i++) {
      free_dvector((obj -> fxArrayIn)[i], 1, obj -> nSizeAlloc);
    }
    free_new_vvector(obj -> fxArrayIn, 1, obj -> pSize, NRUTIL_DPTR);
    obj -> fxArrayIn = NULL;
  }
  if (obj -> fxArrayInAugm != NULL) {
    for (i = 1; i <= obj -> augmSize; i++) {
      free_dvector((obj -> fxArrayInAugm)[i], 1, obj -> nSizeAlloc);
    }
    free_new_vvector(obj -> fxArrayInAugm, 1, obj -> augmSize, NRUTIL_DPTR);
    obj -> fxArrayInAugm = NULL;
  }
  if (obj -> fyArrayIn != NULL) {
    free_dvector(obj -> fyArrayIn, 1, obj -> nSizeAlloc);
    obj -> fyArrayIn = NULL;
  }
  if (obj -> yHat != NULL) {
    free_dvector(obj -> yHat, 1, obj -> nSizeAlloc);
    obj -> yHat = NULL;
  }
  if (obj -> nIndx != NULL) {
    free_uivector(obj -> nIndx, 1, obj -> nSizeAlloc);
    obj -> nIndx = NULL;
  }
  if (obj -> mIndx != NULL) {
    free_uivector(obj -> mIndx, 1, obj -> nSizeAlloc);
    obj -> mIndx = NULL;
  }
  obj -> len = 0;
  obj -> nSizeAlloc = 0;
  obj -> nSize = 0;
  obj -> nSizeRead = 0;
  obj -> mSize = 0;
  obj -> userState = SG_DESC_OPENED;
  obj -> socketState = socketState;
}
void freeDescriptorObj(DescriptorObj *obj) {
  uint i;
  free_cvector(obj -> record, 0, SG_TCP_BUFFER_SIZE - 1);
  if (obj -> fxArrayIn != NULL) {
    for (i = 1; i <= obj -> xSize; i++) {
      free_dvector((obj -> fxArrayIn)[i], 1, obj -> nSizeAlloc);
    }
    free_new_vvector(obj -> fxArrayIn, 1, obj -> pSize, NRUTIL_DPTR);
  }
  if (obj -> fxArrayInAugm != NULL) {
    for (i = 1; i <= obj -> augmSize; i++) {
      free_dvector((obj -> fxArrayInAugm)[i], 1, obj -> nSizeAlloc);
    }
    free_new_vvector(obj -> fxArrayInAugm, 1, obj -> augmSize, NRUTIL_DPTR);
  }
  if (obj -> fyArrayIn != NULL) {
    free_dvector(obj -> fyArrayIn, 1, obj -> nSizeAlloc);
  }
  if (obj -> yHat != NULL) {
    free_dvector(obj -> yHat, 1, obj -> nSizeAlloc);
  }
  if (obj -> nIndx != NULL) {
    free_uivector(obj -> nIndx, 1, obj -> nSizeAlloc);
  }
  if (obj -> mIndx != NULL) {
    free_uivector(obj -> mIndx, 1, obj -> nSizeAlloc);
  }
  free_gblock(obj -> yHatPacket, sizeof(PredictorPacket));
  free_gblock(obj, sizeof(DescriptorObj));
}
uint unlinkUnheldDescriptors(DescriptorObj *headDO, DescriptorObj **tailDO, char force, uint *live) {
  DescriptorObj *currDO, *oldDO;
  uint count;
  count = 0;
  *live = 0;
  currDO = headDO;
  while (currDO != *tailDO) {
    currDO = currDO -> fwdLink;
    (*live) ++;
    if (force == TRUE) {
      currDO -> descID = 0;
    }
    if (currDO -> descID == 0) {
      if ((currDO -> userState == SG_DESC_OPENED) ||
          (currDO -> userState == SG_DESC_PREREAD) ||
          (currDO -> userState == SG_DESC_READING) ||
          (currDO -> userState == SG_DESC_POSTREAD) ||
          (currDO -> userState == SG_DESC_CLOSED)) {
        oldDO = currDO;
        (currDO -> bakLink) -> fwdLink = currDO -> fwdLink;
        if (currDO -> fwdLink != NULL) {
          (currDO -> fwdLink) -> bakLink = currDO -> bakLink;
        }
        else {
          *tailDO = currDO -> bakLink;
        }
        currDO = currDO -> bakLink;
        freeDescriptorObj(oldDO);
        count ++;
        (*live) --;
      }
    }
  }
  return count;
}
