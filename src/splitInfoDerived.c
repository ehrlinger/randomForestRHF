
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "shared/globalCore.h"
#include           "shared/externalCore.h"
#include           "global.h"
#include           "external.h"

// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "splitInfoDerived.h"
#include "shared/nrutil.h"
SplitInfoDerived *makeSplitInfoDerived(AugmentationObjCommon *augmComm) {
  SplitInfoDerived *info = (SplitInfoDerived*) gblock((size_t) sizeof(SplitInfoDerived));
  initSplitInfoMax((SplitInfoMax*) info, augmComm -> nSize);
  info -> yBar = RF_nativeNaN;
  info -> bsf = 0;
  return info;
}
void freeSplitInfoDerived(SplitInfoDerived *info, AugmentationObjCommon *augmComm) {
  deinitSplitInfoMax((SplitInfoMax*) info);
  free_gblock(info, (size_t) sizeof(SplitInfoDerived));
}
