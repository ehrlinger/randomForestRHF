
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "shared/globalCore.h"
#include           "shared/externalCore.h"
#include           "global.h"
#include           "external.h"

// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "greedyOps.h"
#include "greedyInfo.h"
#include "shared/nrutil.h"
#include "shared/error.h"
GreedyObj *makeGreedyObj(Node *parent, GreedyObj *head) {
  GreedyObj *greedyObj = (GreedyObj*) gblock((size_t) sizeof(GreedyObj));
  greedyObj -> parent = parent;
  greedyObj -> leftCart = NULL;
  greedyObj -> rightCart = NULL;
  greedyObj -> fwdLink = NULL;
  greedyObj -> bakLink = NULL;
  greedyObj -> head = head;
  greedyObj -> splitInfoDerivedCart = NULL;
  greedyObj -> leafFlag = FALSE;
  greedyObj -> eRiskLossCart = RF_nativeNaN;
  greedyObj -> eRiskLossRaw  = RF_nativeNaN;
  greedyObj -> bestSplitType = 0;
  return greedyObj;
}
void freeGreedyObj(GreedyObj *gObj) {
  if (gObj -> splitInfoDerivedCart != NULL) {
    freeSplitInfoDerived(gObj -> splitInfoDerivedCart, gObj -> parent -> augm -> common);
  }
  if (gObj -> leftCart != NULL) {
    freeNode(gObj -> leftCart);
  }
  if (gObj -> rightCart != NULL) {
    freeNode(gObj -> rightCart);
  }
  free_gblock(gObj, (size_t) sizeof(GreedyObj));
}
void freeGreedyObjList(GreedyObj *gObj) {
  if (gObj -> fwdLink != NULL) {
    freeGreedyObjList(gObj -> fwdLink);
  }
  freeGreedyObj(gObj);
}
LatOptTreeObj *makeLatOptTreeObj(uint lag) {
  LatOptTreeObj *lotObj = (LatOptTreeObj*) gblock((size_t) sizeof(LatOptTreeObj));
  lotObj -> lag = lag;
  lotObj -> risk  = dvector(1, lag + 1);
  for (uint i = 1; i <= lag; i++) {
    lotObj -> risk[i]  = 0.0;
  }
  lotObj -> firstIn = 0;
  lotObj -> lastIn  = 0;
  lotObj -> size = 0;
  lotObj -> strikeout = 0;
  lotObj -> firstOD = 0.0;
  lotObj -> treeSize = 1;
  return lotObj;
}
void freeLatOptTreeObj(LatOptTreeObj *lotObj) {
  free_dvector(lotObj -> risk,  1, lotObj -> lag + 1);
  free_gblock(lotObj, (size_t) sizeof(LatOptTreeObj));
}
void insertRisk(uint treeID, LatOptTreeObj *obj, double value) {
  double normalizedValue;
  if (obj -> lag > 0) {
    normalizedValue = value / (obj -> lag);
    if (obj -> size < (obj -> lag)) {
      if (obj -> size == 0) {
        (obj -> firstIn) ++;
      }
      else {
      }
      obj -> firstOD = obj -> firstOD + normalizedValue;
      (obj -> size) ++;
      (obj -> lastIn) ++;
    }
    else {
      double saveFirstOD = obj -> firstOD; 
      obj -> firstOD = obj -> firstOD - obj -> risk[obj -> firstIn] + normalizedValue;
      obj -> strikeout = (obj -> strikeout) + (((obj -> firstOD - saveFirstOD)) > 0 ? 1:0);
      if ((++ (obj -> lastIn))  > (obj -> lag)) obj -> lastIn  = 1;
      if ((++ (obj -> firstIn)) > (obj -> lag)) obj -> firstIn = 1;
    }
    obj -> risk[obj -> lastIn] = normalizedValue;
  }
}
