
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "shared/globalCore.h"
#include           "shared/externalCore.h"
#include           "global.h"
#include           "external.h"

// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "splitNLS.h"
#include "splitTDC.h"
#include "greedyInfo.h"
#include "splitUtil.h"
#include "augmentationOpsCommon.h"
#include "augmentationOpsSimple.h"
#include "augmentationOps.h"
#include "shared/error.h"
#include "shared/sampling.h"
#include "shared/factorOps.h"
#include "shared/splitInfo.h"
#include "shared/nrutil.h"
char virtuallySplitNodeNLS (uint       treeID,
                            GreedyObj *greedyMembr,
                            char   *localSplitIndicator,
                            uint   *localSplitIndx,
                            double *localSplitValue,
                            uint   *leftSize) {
  Node     *parent;
  NodeBase *baseParent;
  uint  repMembrSize;
  uint *repMembrIndx;
  uint     xvarID;
  uint     xvarCount;
  double  *splitVector;
  uint     splitVectorSize;
  uint   *indxx;
  double **xArray;
  uint priorMembrIter, currentMembrIter;
  uint rghtSize;
  char daughterFlag;
  uint splitLength;
  void *splitVectorPtr;
  double *observation;
  char factorFlag;
  uint mwcpSizeAbsolute;
  char deterministicSplitFlag;
  char result;
  double delta;
  uint i, j, k;
  uint m;
  parent = greedyMembr -> parent;
  baseParent = (NodeBase *) parent;
  AugmentationObjCommon *augmObjCommon = parent -> augm -> common;  
  repMembrSize = baseParent -> repMembrSize;
  repMembrIndx = baseParent -> repMembrIndx;
  xArray = augmObjCommon -> xArray;
  splitVector = dvector(1, repMembrSize);
  DistributionObj *distObj = stackRandomCovariates(treeID, baseParent, RF_mtry);
  double phiLeft, phiRight;
  double max_phiLeft, max_phiRight;
  delta = RF_nativeNaN;     
  max_phiLeft = max_phiRight = RF_nativeNaN;  
  SplitInfoMax *infoMax = (SplitInfoMax *) (greedyMembr -> splitInfoDerivedCart);
  double deltaMax;
  uint   indexMax;
  deltaMax = RF_nativeNaN;  
  xvarCount = 0;
  result = FALSE;
  uint *localEventTimeCount, *localEventTimeIndex;
  uint  localEventTimeSize;
  uint *nodeParentEvent,  *nodeLeftEvent,  *nodeRightEvent;
  uint *nodeParentAtRisk, *nodeLeftAtRisk, *nodeRightAtRisk;
  uint tIndx;
  stackAndGetSplitSurv(treeID,
                       baseParent,
                       & localEventTimeCount,
                       & localEventTimeIndex,
                       & localEventTimeSize,
                       & nodeParentEvent,
                       & nodeParentAtRisk,
                       & nodeLeftEvent,
                       & nodeLeftAtRisk,
                       & nodeRightEvent,
                       & nodeRightAtRisk);
  while (selectRandomCovariates(treeID,
                                baseParent,
                                distObj,
                                RF_mtry,
                                & factorFlag,
                                & xvarID,
                                & xvarCount)) {
    observation = xArray[xvarID];
    splitVectorSize = stackAndConstructSplitVectorGenericPhase1(treeID,
                                                                baseParent,
                                                                xvarID,
                                                                observation,
                                                                splitVector,
                                                                & indxx);
    if (splitVectorSize >= 2) {
      splitLength = stackAndConstructSplitVectorGenericPhase2(treeID,
                                                              baseParent,
                                                              xvarID,
                                                              splitVector,
                                                              splitVectorSize,
                                                              factorFlag,
                                                              & deterministicSplitFlag,
                                                              & mwcpSizeAbsolute,
                                                              & splitVectorPtr);
      *leftSize = 0;
      priorMembrIter = 0;
      if (factorFlag == FALSE) {
        for (m = 1; m <= localEventTimeSize; m++) {
          nodeLeftEvent[m] = nodeLeftAtRisk[m] = 0;
          nodeRightEvent[m] = nodeRightAtRisk[m] = 0;
        }
        for (j = 1; j <= repMembrSize; j++) {
          localSplitIndicator[repMembrIndx[j]] = RIGHT;
        }
      }
      indexMax =  0;
      for (j = 1; j < splitLength; j++) {
        if (factorFlag == TRUE) {
          priorMembrIter = 0;
          *leftSize = 0;
        }
        virtuallySplitNode(treeID,
                           baseParent,
                           factorFlag,
                           mwcpSizeAbsolute,
                           observation,
                           indxx,
                           priorMembrIter,
                           splitVectorPtr,
                           j,
                           localSplitIndicator,                                 
                           leftSize,
                           & currentMembrIter);
        rghtSize = repMembrSize - (*leftSize);
        if (((*leftSize) != 0) && (rghtSize != 0)) {
          if (factorFlag == TRUE) {
            for (m = 1; m <= localEventTimeSize; m++) {
              nodeLeftEvent[m] = nodeLeftAtRisk[m] = 0;
              nodeRightEvent[m] = nodeRightAtRisk[m] = 0;
            }
            for (k = 1; k <= repMembrSize; k++) {
              if (localSplitIndicator[ repMembrIndx[k] ] == LEFT) {
                tIndx = 0;  
                for (m = 1; m <= localEventTimeSize; m++) {
                  if (localEventTimeIndex[m] <= RF_masterTimeIndex[treeID][ repMembrIndx[k] ]) {
                    tIndx = m;
                    nodeLeftAtRisk[tIndx] ++;
                  }
                  else {
                    m = localEventTimeSize;
                  }
                }
                if (RF_status[treeID][ repMembrIndx[k] ] > 0) {
                  nodeLeftEvent[tIndx] ++;
                }
              }
              else {
              }
            } 
          }
          else {
            for (k = priorMembrIter + 1; k < currentMembrIter; k++) {
              tIndx = 0;  
              for (m = 1; m <= localEventTimeSize; m++) {
                if (localEventTimeIndex[m] <= RF_masterTimeIndex[treeID][ repMembrIndx[indxx[k]] ]) {
                  tIndx = m;
                  nodeLeftAtRisk[tIndx] ++;
                }
                else {
                  m = localEventTimeSize;
                }
              }
              if (RF_status[treeID][ repMembrIndx[indxx[k]] ] > 0) {
                nodeLeftEvent[tIndx] ++;
              }
            }
          }
          for (m = 1; m <= localEventTimeSize; m++) {
            nodeRightEvent[m] = nodeParentEvent[m] - nodeLeftEvent[m];
            nodeRightAtRisk[m] = nodeParentAtRisk[m] - nodeLeftAtRisk[m];
          }
          phiLeft = getLogLikelihood(localEventTimeSize,
                                     nodeLeftEvent,
                                     nodeLeftAtRisk);
          phiRight = getLogLikelihood(localEventTimeSize,
                                      nodeRightEvent,
                                      nodeRightAtRisk);
                    delta = phiLeft + phiRight;
        }
        else {
          delta = RF_nativeNaN;
        }
        if (!RF_nativeIsNaN(delta)) {
          if(RF_nativeIsNaN(deltaMax)) {
            deltaMax = delta;
            indexMax = j;
            max_phiLeft = phiLeft;
            max_phiRight = phiRight;
          }
          else {
            if ((delta - deltaMax) > EPSILON2) {
              deltaMax = delta;
              indexMax = j;
              max_phiLeft = phiLeft;
              max_phiRight = phiRight;
            }
            else {
            }
          }
        }
        else {
        }
        if (factorFlag == FALSE) {
          priorMembrIter = currentMembrIter - 1;
        }
      }  
      updateMaximumSplitCart(treeID,
                             parent,
                             deltaMax,
                             xvarID,
                             indexMax,
                             factorFlag,
                             mwcpSizeAbsolute,
                             splitVectorPtr,
                             infoMax);
      unstackSplitVectorGeneric(treeID,
                                baseParent,
                                splitLength,
                                factorFlag,            
                                splitVectorSize,
                                mwcpSizeAbsolute,
                                deterministicSplitFlag,
                                splitVectorPtr,
                                indxx);
    }
  }  
  unstackSplitSurv(treeID,
                   baseParent,
                   localEventTimeCount,
                   localEventTimeIndex,
                   localEventTimeSize,
                   nodeParentEvent,
                   nodeParentAtRisk,
                   nodeLeftEvent,
                   nodeLeftAtRisk,
                   nodeRightEvent,
                   nodeRightAtRisk);
  unstackRandomCovariates(treeID, distObj);
  free_dvector(splitVector, 1, repMembrSize);
  if (!RF_nativeIsNaN(deltaMax)) {
    result = TRUE;
    *localSplitIndx = 0;
    *localSplitValue = RF_nativeNaN;
    Node *left  = makeNode(augmObjCommon -> pSize);
    Node *right = makeNode(augmObjCommon -> pSize);
    left  -> nSize = augmObjCommon -> nSize;
    right -> nSize = augmObjCommon -> nSize;
    greedyMembr -> leftCart  = left;
    greedyMembr -> rightCart = right;
    NodeBase *baseLeft  = (NodeBase *) left;
    NodeBase *baseRight = (NodeBase *) right;
    baseLeft ->  repMembrIndx  = uivector(1, repMembrSize);
    baseRight -> repMembrIndx  = uivector(1, repMembrSize);
    baseLeft  -> repMembrSizeAlloc = repMembrSize;
    baseRight -> repMembrSizeAlloc = repMembrSize;  
    *leftSize = 0;
    rghtSize = 0;
    observation = xArray[infoMax -> splitParameter];
    if (infoMax -> splitValueFactSize > 0) {
      for (i = 1; i <= repMembrSize; i++) {
        daughterFlag = splitOnFactor((uint)  observation[repMembrIndx[i]], infoMax -> splitValueFactPtr);
        if (daughterFlag == LEFT) {
          localSplitIndicator[repMembrIndx[i]] = LEFT;
          (*leftSize) ++;
          if (TRUE) {
            if ((*leftSize < 1) || (*leftSize > repMembrSize)) {
              RF_nativeError("\nBAD leftSize: tree=%u leaf=%u leftSize=%u repMembrSize=%u",
                             treeID,
                             baseLeft -> nodeID,
                             *leftSize,
                             repMembrSize);
              RF_nativeExit();
            }
          }
          baseLeft -> repMembrIndx[*leftSize] = repMembrIndx[i];
        }
        else {
          localSplitIndicator[repMembrIndx[i]] = RIGHT;
          rghtSize ++;
          if (TRUE) {
            if ((rghtSize < 1) || (rghtSize > repMembrSize)) {
              RF_nativeError("\nBAD rghtSize: tree=%u leaf=%u rghtSize=%u repMembrSize=%u",
                             treeID,
                             baseRight -> nodeID,
                             rghtSize,
                             repMembrSize);
              RF_nativeExit();
            }
          }
          baseRight -> repMembrIndx[rghtSize] = repMembrIndx[i];
        }
      }
    }
    else {
      for (i = 1; i <= repMembrSize; i++) {
        if ( (infoMax -> splitValueCont - observation[repMembrIndx[i]]) >= 0.0) {
          daughterFlag = LEFT;
        }
        else {
          daughterFlag = RIGHT;
        }
        if (daughterFlag == LEFT) {
          localSplitIndicator[repMembrIndx[i]] = LEFT;
          (*leftSize) ++;
          if (TRUE) {
            if ((*leftSize < 1) || (*leftSize > repMembrSize)) {
              RF_nativeError("\nBAD leftSize: tree=%u leaf=%u leftSize=%u repMembrSize=%u",
                             treeID,
                             baseLeft -> nodeID,
                             *leftSize,
                             repMembrSize);
              RF_nativeExit();
            }
          }
          baseLeft -> repMembrIndx[*leftSize] = repMembrIndx[i];
        }
        else {
          localSplitIndicator[repMembrIndx[i]] = RIGHT;
          rghtSize ++;
          if (TRUE) {
            if ((rghtSize < 1) || (rghtSize > repMembrSize)) {
              RF_nativeError("\nBAD rghtSize: tree=%u leaf=%u rghtSize=%u repMembrSize=%u",
                             treeID,
                             baseRight -> nodeID,
                             rghtSize,
                             repMembrSize);
              RF_nativeExit();
            }
          }
          baseRight -> repMembrIndx[rghtSize] = repMembrIndx[i];
        }
      }
    }
    baseLeft  -> repMembrSize = *leftSize;
    baseRight -> repMembrSize =  rghtSize;
    for (k = 1; k <= baseParent -> xSize; k++) {
      baseLeft -> permissible[k] = baseRight -> permissible[k] = baseParent -> permissible[k];
    }
    left  -> augm =  getAugmentationObjGeneric(augmObjCommon, treeID, left);
    right -> augm =  getAugmentationObjGeneric(augmObjCommon, treeID, right);
    saveRiskRaw(treeID, left,  max_phiLeft);
    saveRiskRaw(treeID, right, max_phiRight);
    initializeRisk(treeID, left);
    initializeRisk(treeID, right);
  }
  return result;
}
void stackAndGetSplitSurv(uint      treeID,
                          NodeBase *parent,
                          uint  **eventTimeCount,
                          uint  **eventTimeIndex,
                          uint   *eventTimeSize,
                          uint  **parentEvent,
                          uint  **parentAtRisk,
                          uint  **leftEvent,
                          uint  **leftAtRisk,
                          uint  **rightEvent,
                          uint  **rightAtRisk) {
  uint *repMembrIndx = parent -> repMembrIndx;
  uint  repMembrSize = parent -> repMembrSize;
  *eventTimeCount = uivector(1, RF_masterTimeSize);
  *eventTimeIndex = uivector(1, RF_masterTimeSize);
  *eventTimeSize = getEventTime(treeID,
                                parent,
                                repMembrIndx,
                                repMembrSize,
                                *eventTimeCount,
                                *eventTimeIndex);
  stackSplitEventAndRisk(treeID,
                         parent,
                         *eventTimeSize,
                          parentEvent,
                          parentAtRisk,
                          leftEvent,
                          leftAtRisk,
                          rightEvent,
                          rightAtRisk);
  getSplitEventAndRisk( treeID,
                        parent,
                        repMembrIndx,
                        repMembrSize,
                        *eventTimeCount,
                        *eventTimeIndex,
                        *eventTimeSize,
                        *parentEvent,
                        *parentAtRisk);
}
void unstackSplitSurv(uint      treeID,
                      NodeBase *parent,
                      uint *eventTimeCount,
                      uint *eventTimeIndex,
                      uint  eventTimeSize,
                      uint *parentEvent,
                      uint *parentAtRisk,
                      uint *leftEvent,
                      uint *leftAtRisk,
                      uint *rightEvent,
                      uint *rightAtRisk) {
  free_uivector(eventTimeCount, 1, RF_masterTimeSize);
  free_uivector(eventTimeIndex, 1, RF_masterTimeSize);
  unstackSplitEventAndRisk(treeID,
                           parent,
                           eventTimeSize,
                           parentEvent,
                           parentAtRisk,
                           leftEvent,
                           leftAtRisk,
                           rightEvent,
                           rightAtRisk);
}
uint getEventTime(uint      treeID,
                  NodeBase *parent,
                  uint   *repMembrIndx,
                  uint    repMembrSize,
                  uint   *eventTimeCount,
                  uint   *eventTimeIndex) {
  uint i;
  uint eventTimeSize;
  eventTimeSize = 0;
  for (i=1; i <= RF_masterTimeSize; i++) {
    eventTimeCount[i] = 0;
  }
  for (i = 1; i <= repMembrSize; i++) {
    if (RF_status[treeID][ repMembrIndx[i] ] > 0) {
      eventTimeCount[RF_masterTimeIndex[treeID][ repMembrIndx[i] ]] ++;
    }
  }
  for (i=1; i <= RF_masterTimeSize; i++) {
    if (eventTimeCount[i] > 0) {
      eventTimeIndex[++eventTimeSize] = i;
    }
  }
  return (eventTimeSize);
}
void stackSplitEventAndRisk(uint      treeID,
                            NodeBase *parent,
                            uint    genEventTimeSize,
                            uint  **genParentEvent,
                            uint  **genParentAtRisk,
                            uint  **genLeftEvent,
                            uint  **genLeftAtRisk,
                            uint  **genRightEvent,
                            uint  **genRightAtRisk) {
  if (genEventTimeSize > 0) {
    *genParentEvent  = uivector(1, genEventTimeSize);
    *genParentAtRisk = uivector(1, genEventTimeSize);
    *genLeftEvent    = uivector(1, genEventTimeSize);
    *genLeftAtRisk   = uivector(1, genEventTimeSize);
    *genRightEvent   = uivector(1, genEventTimeSize);
    *genRightAtRisk  = uivector(1, genEventTimeSize);
  }
  else {
    *genParentEvent  = *genParentAtRisk = *genLeftEvent  = *genLeftAtRisk = *genRightEvent  = *genRightAtRisk = NULL;
  }
}
void unstackSplitEventAndRisk(uint      treeID,
                              NodeBase *parent,
                              uint    genEventTimeSize,
                              uint   *genParentEvent,
                              uint   *genParentAtRisk,
                              uint   *genLeftEvent,
                              uint   *genLeftAtRisk,
                              uint   *genRightEvent,
                              uint   *genRightAtRisk) {
  if (genEventTimeSize > 0) {
    free_uivector(genParentEvent, 1, genEventTimeSize);
    free_uivector(genParentAtRisk, 1, genEventTimeSize);
    free_uivector(genLeftEvent, 1, genEventTimeSize);
    free_uivector(genLeftAtRisk, 1, genEventTimeSize);
    free_uivector(genRightEvent, 1, genEventTimeSize);
    free_uivector(genRightAtRisk, 1, genEventTimeSize);
  }
}
void getSplitEventAndRisk(uint      treeID,
                          NodeBase *parent,
                          uint   *repMembrIndx,
                          uint    repMembrSize,
                          uint   *eventTimeCount,
                          uint   *eventTimeIndex,
                          uint    eventTimeSize,
                          uint   *parentEvent,
                          uint   *parentAtRisk) {
  uint i, j;
  for (i = 1; i <= eventTimeSize; i++) {
    parentAtRisk[i] = 0;
    parentEvent[i]  = eventTimeCount[eventTimeIndex[i]];
    for (j = 1; j <= repMembrSize; j++) {
      if (eventTimeIndex[i] <= RF_masterTimeIndex[treeID][ repMembrIndx[j] ]) {
        parentAtRisk[i] ++;
      }
    }
  }
}
double getLogLikelihood(uint size, uint *event, uint *atRisk) {
  double phi, logLH, delta;
  uint i;
  phi  = 0.0;
  for (i = 1; i <= size; i++) {
    logLH = 0.0;
    if (event[i] > 0) {
      logLH += (event[i] * log((double) event[i]));
    }
    delta = atRisk[i] - event[i];    
    if (delta > 0) {
      logLH += (delta * log((double) delta));
    }
    if (atRisk[i] > 0) {
      logLH -= (atRisk[i] * log((double) atRisk[i]));
    }
    phi += logLH;
  }
  return phi;
}
void initializeRiskRaw(uint treeID, Node *parent) {
  uint *repMembrIndx;
  uint  repMembrSize;
  uint *eventTimeCount;
  uint *eventTimeIndex;
  uint  eventTimeSize;
  uint *parentEvent;
  uint *parentAtRisk;  
  repMembrIndx = ((NodeBase *) parent) -> repMembrIndx;
  repMembrSize = ((NodeBase *) parent) -> repMembrSize;
  eventTimeCount = uivector(1, RF_masterTimeSize);
  eventTimeIndex = uivector(1, RF_masterTimeSize);
  eventTimeSize = getEventTime(treeID,
                               (NodeBase *) parent,
                               repMembrIndx,
                               repMembrSize,
                               eventTimeCount,
                               eventTimeIndex);
  if (eventTimeSize > 0) {
    parentEvent  = uivector(1, eventTimeSize);
    parentAtRisk = uivector(1, eventTimeSize);
    getSplitEventAndRisk( treeID,
                          (NodeBase *) parent,
                          repMembrIndx,
                          repMembrSize,
                          eventTimeCount,
                          eventTimeIndex,
                          eventTimeSize,
                          parentEvent,
                          parentAtRisk);
    parent -> phi = getLogLikelihood(eventTimeSize, parentEvent, parentAtRisk);
    parent -> eRiskRaw = - parent -> phi;
    free_uivector(parentEvent, 1, eventTimeSize);
    free_uivector(parentAtRisk, 1, eventTimeSize);
  }
  free_uivector(eventTimeCount, 1, RF_masterTimeSize);
  free_uivector(eventTimeIndex, 1, RF_masterTimeSize);
}
void saveRiskRaw(uint treeID, Node *parent, double phi) {
  parent -> phi= phi;
  parent -> eRiskRaw = phi;
}
