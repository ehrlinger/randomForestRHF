
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "shared/globalCore.h"
#include           "shared/externalCore.h"
#include           "global.h"
#include           "external.h"

// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "nodeOps.h"
#include "node.h"
#include "shared/nrutil.h"
#include "shared/nodeBaseOps.h"
#include "augmentationOpsCommon.h"
#include "augmentationOpsSimple.h"
#include "augmentationOps.h"
#include "splitInfoDerived.h"
void *makeNodeDerived(uint xSize) {
  Node *parent = (Node*) gblock((size_t) sizeof(Node));
  initNodeBase((NodeBase*) parent, xSize);
  parent -> nSize = 0;
  parent -> augm = NULL;
  parent -> eRiskCart  = RF_nativeNaN;
  parent -> mean = RF_nativeNaN;
  parent -> v0 = parent -> u0 = parent -> c0 = parent -> rn0 = 0.0;
  parent -> phi = RF_nativeNaN;
  parent -> outcome = 0;
  parent -> splitInfoDerived = NULL;
  parent -> tstMembrSizeAlloc = 0;
  parent -> tstMembrSize = 0;
  parent -> tstMembrIndx = NULL;
  return parent;
}
void freeNodeDerived(void *parent) {
  if (((Node*) parent) -> splitInfoDerived != NULL) {
    freeSplitInfoDerived(((Node*) parent) -> splitInfoDerived, ((Node*) parent) -> augm -> common);
  }
  if (((Node*) parent) -> augm != NULL) {
    freeAugmentationObj(((Node*) parent) -> augm);
  }
  if (((Node*) parent) -> tstMembrSizeAlloc > 0) {
    if (((Node*) parent) -> tstMembrIndx != NULL) {
      free_uivector(((Node*) parent) -> tstMembrIndx, 1, ((Node*) parent) -> tstMembrSizeAlloc);
      ((Node*) parent) -> tstMembrIndx = NULL;
    }
  }
  deinitNodeBase((NodeBase*) parent);
  free_gblock(parent, (size_t) sizeof(Node));
}
