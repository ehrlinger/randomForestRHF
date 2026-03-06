
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "shared/globalCore.h"
#include           "shared/externalCore.h"
#include           "global.h"
#include           "external.h"

// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "splitGreedy.h"
#include "greedyOps.h"
#include "greedyInfo.h"
#include "splitInfoDerived.h"
#include "augmentationOpsCommon.h"
#include "augmentationOpsSimple.h"
#include "augmentationOps.h"
#include "splitUtil.h"
#include "node.h"
#include "splitTDC.h"
#include "splitNLS.h"
#include "shared/nrutil.h"
#include "shared/error.h"
char getBestSplit(uint treeID, LatOptTreeObj *lotObj, GreedyObj *greedyMembr) {
  Node     *parent;
  NodeBase *parentBase;
  char *localSplitIndicator;
  uint  localSplitIndx;
  double localSplitValue;
  uint leftSize, rightSize;
  char result, resultVirtual;
  double sumCart;
  double sumCartRaw;
  double eRiskLossCart;
  double eRiskLossRaw;
  double **yArray;
  uint i;
  eRiskLossCart = 0.0;
  eRiskLossRaw  = 0.;
  parent = greedyMembr -> parent;
  parentBase = (NodeBase *) parent;
  yArray = parent -> augm -> common -> yArray;
  result = getPreSplitResult(treeID,
                             parentBase,
                             RF_nodeSize,
                             yArray);
 resultVirtual = FALSE;
 if (result) {
   uint  allMembrSize = parentBase -> allMembrSize;
   uint *allMembrIndx = parentBase -> allMembrIndx;
   greedyMembr -> splitInfoDerivedCart = makeSplitInfoDerived(parent -> augm -> common);
   localSplitIndicator = ((SplitInfoMax *) (greedyMembr -> splitInfoDerivedCart)) -> indicator;
   for (i = 1; i <= allMembrSize; i++) {
     localSplitIndicator[allMembrIndx[i]] = NEITHER;
   }
   if (SG_splitRule == SG_SURV_TDC) {
     resultVirtual = virtuallySplitNodeTDC(treeID,
                                           greedyMembr,
                                           localSplitIndicator,
                                           & localSplitIndx,  
                                           & localSplitValue, 
                                           & leftSize);
   }
   else if (SG_splitRule == SG_SURV_NLS) {
     resultVirtual = virtuallySplitNodeNLS(treeID,
                                           greedyMembr,
                                           localSplitIndicator,
                                           & localSplitIndx,  
                                           & localSplitValue, 
                                           & leftSize);
   }
   if (resultVirtual) {
     rightSize = parentBase -> repMembrSize - leftSize;
     if ((leftSize != 0) && (rightSize != 0)) {
     }
     else {
       RF_nativeError("\nRF-SRC:  *** ERROR *** ");
       RF_nativeError("\nRF-SRC:  left or right size is zero:  (%10d or %10d)", leftSize, rightSize);
       RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
       RF_nativeExit();
     }
   }
   else {
     freeSplitInfoDerived(greedyMembr -> splitInfoDerivedCart, parent -> augm -> common);
     greedyMembr -> splitInfoDerivedCart = NULL;
     greedyMembr -> leftCart  = NULL;
     greedyMembr -> rightCart = NULL;
   }
   if (greedyMembr -> splitInfoDerivedCart != NULL) {
     sumCart = greedyMembr -> leftCart -> eRiskCart + greedyMembr -> rightCart -> eRiskCart;
     sumCartRaw = greedyMembr -> leftCart -> eRiskRaw + greedyMembr -> rightCart -> eRiskRaw;      
     eRiskLossCart = sumCart - parent -> eRiskCart;
     eRiskLossRaw  = sumCartRaw - parent -> eRiskRaw;
     greedyMembr -> eRiskLossCart = eRiskLossCart;
     greedyMembr -> eRiskLossRaw  = eRiskLossRaw;
     if (RF_xtType[((SplitInfoMax*) (greedyMembr -> splitInfoDerivedCart)) -> splitParameter] > 0) {
       greedyMembr -> bestSplitType = SG_SPLIT_ON_TDC;
     }
     else {
       greedyMembr -> bestSplitType = SG_SPLIT_NO_TDC;
     }
   }
 }  
  return result && resultVirtual;
}
