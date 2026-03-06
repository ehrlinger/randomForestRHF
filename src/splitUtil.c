
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "shared/globalCore.h"
#include           "shared/externalCore.h"
#include           "global.h"
#include           "external.h"

// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "splitUtil.h"
#include "shared/sampling.h"
#include "shared/factorOps.h"
#include "shared/nrutil.h"
#include "shared/error.h"
char getPreSplitResult (uint treeID, NodeBase *parent, uint nodeSize, double **response) {
  char result;
  result = TRUE;
  if (result) {
    if (parent -> repMembrSize < (2 * nodeSize)) {
      result = FALSE;
    }
  }
  if (result) {
        uint q,k,m;
        uint *evntProp = uivector(1, RF_eventTypeSize + 1);
        for (q = 1; q <= RF_eventTypeSize + 1; q++) {
          evntProp[q] = 0;
        }
        for (uint i = 1; i <= parent -> repMembrSize; i++) {
          m = (uint) response[RF_statusIndex][parent -> repMembrIndx[i]];
          if (m > 0) {
            evntProp[RF_eventTypeIndex[m]] ++;
          }
          else {
            evntProp[RF_eventTypeSize + 1] ++;
          }
        }
        k = 0;
        for (q = 1; q <= RF_eventTypeSize + 1; q++) {
          if(evntProp[q] > 0) {
            k ++;
          }
        }
        if (k == 1) {
          if (evntProp[RF_eventTypeSize + 1] > 0) {
            result = FALSE;
          }
          else {
            result = getVariance(parent -> repMembrSize,
                                 parent -> repMembrIndx,
                                 response[RF_timeIndex],
                                 & (parent -> mean),
                                 NULL);
          }
        }
        free_uivector(evntProp, 1, RF_eventTypeSize + 1);
  }
  return result;
}
char getVarianceSinglePass(uint    repMembrSize,
                           uint   *repMembrIndx,
                           double *response,
                           double *mean,
                           double *variance) {
  uint i;
  double meanResultOld, meanResult, varResultOld, varResult;
  char result;
  result = TRUE;
  meanResultOld = response[repMembrIndx[1]];
  varResultOld  = 0.0;
  for (i = 2; i <= repMembrSize; i++) {
    meanResult = meanResultOld + ((response[repMembrIndx[i]] - meanResultOld) / i);
    varResult  = varResultOld + ((response[repMembrIndx[i]] - meanResultOld) * (response[repMembrIndx[i]] - meanResult)); 
    meanResultOld = meanResult;
    varResultOld = varResult;
  }
  if (repMembrSize > 1) {
    varResultOld = varResultOld / (double) (repMembrSize - 1);
    result = ((varResultOld <= EPSILON2) ? FALSE : TRUE);
  }
  if (mean     != NULL) *mean     = meanResultOld;
  if (variance != NULL) *variance = varResultOld;
  return result;
}
void getRandomPair (uint treeID, uint relativeFactorSize, uint absoluteFactorSize, double *absoluteLevel, uint *result) {
  uint randomGroupIndex;
  double randomValue;
  uint k;
  if(RF_factorList[treeID][relativeFactorSize] == NULL) {
    RF_nativeError("\nRF-SRC:  *** ERROR *** ");
    RF_nativeError("\nRF-SRC:  Factor not allocated for size:  %10d", relativeFactorSize);
    RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
    RF_nativeExit();
  }
  double *cdf = dvector(1, RF_factorList[treeID][relativeFactorSize] -> cardinalGroupCount);
  if (relativeFactorSize <= MAX_EXACT_LEVEL) {
    for (k=1; k <= RF_factorList[treeID][relativeFactorSize] -> cardinalGroupCount; k++) {
      cdf[k] = (double) ((uint*) RF_factorList[treeID][relativeFactorSize] -> cardinalGroupSize)[k];
    }
  }
  else {
    for (k=1; k <= RF_factorList[treeID][relativeFactorSize] -> cardinalGroupCount; k++) {
      cdf[k] = ((double*) RF_factorList[treeID][relativeFactorSize] -> cardinalGroupSize)[k];
    }
  }
  for (k=2; k <= RF_factorList[treeID][relativeFactorSize] -> cardinalGroupCount; k++) {
    cdf[k] += cdf[k-1];
  }
  randomValue = ceil((ran1B(treeID) * cdf[RF_factorList[treeID][relativeFactorSize] -> cardinalGroupCount]));
  randomGroupIndex = 1;
  while (randomValue > cdf[randomGroupIndex]) {
    randomGroupIndex ++;
  }
  free_dvector(cdf, 1, RF_factorList[treeID][relativeFactorSize] -> cardinalGroupCount);
  createRandomBinaryPair(treeID, relativeFactorSize, absoluteFactorSize, randomGroupIndex, absoluteLevel, result);
}
void createRandomBinaryPair(uint    treeID,
                            uint    relativeFactorSize,
                            uint    absoluteFactorSize,
                            uint    groupIndex,
                            double *absoluteLevel,
                            uint   *pair) {
  uint mwcpLevelIdentifier;
  uint mwcpSizeAbsolute;
  uint offset, levelSize, levelIndex;
  uint k;
  levelIndex = 0;  
  mwcpSizeAbsolute = RF_factorList[treeID][absoluteFactorSize] -> mwcpSize;
  uint *levelVector = uivector(1, relativeFactorSize);
  uint *randomLevel = uivector(1, groupIndex);
  for (k = 1; k <= relativeFactorSize; k++) {
    levelVector[k] = k;
  }
  levelSize = relativeFactorSize;
  for (k = 1; k <= groupIndex; k++) {
    randomLevel[k] = sampleUniformlyFromVector(treeID,
                                               ran1B,
                                               levelVector,
                                               levelSize,
                                               &levelIndex);
    levelVector[levelIndex] = levelVector[levelSize];
    levelSize --;
  }
  for (k = 1; k <= groupIndex; k++) {
    randomLevel[k] = (uint) absoluteLevel[randomLevel[k]];
  }
  for (offset = 1; offset <= mwcpSizeAbsolute; offset++) {
    pair[offset] = 0;
  }
  for (k = 1; k <= groupIndex; k++) {
    mwcpLevelIdentifier = (randomLevel[k] >> (3 + ulog2(sizeof(uint)))) + ((randomLevel[k] & (MAX_EXACT_LEVEL - 1)) ? 1 : 0);
    pair[mwcpLevelIdentifier] += upower(2, randomLevel[k] - ((mwcpLevelIdentifier - 1) * MAX_EXACT_LEVEL) - 1 );
  }
  free_uivector(levelVector, 1, relativeFactorSize);
  free_uivector(randomLevel, 1, groupIndex);
}
void convertRelToAbsBinaryPair(uint    treeID,
                               uint    relativeFactorSize,
                               uint    absoluteFactorSize,
                               uint    relativePair,
                               double *absoluteLevel,
                               uint   *pair) {
  uint mwcpLevelIdentifier;
  uint mwcpSizeAbsolute;
  uint coercedAbsoluteLevel;
  uint k, offset;
  mwcpSizeAbsolute = RF_factorList[treeID][absoluteFactorSize] -> mwcpSize;
  for (offset = 1; offset <= mwcpSizeAbsolute; offset++) {
    pair[offset] = 0;
  }
  for (k = 1; k <= relativeFactorSize; k++) {
    if (relativePair & ((uint) 0x01)) {
      coercedAbsoluteLevel = (uint) absoluteLevel[k];
      mwcpLevelIdentifier = (coercedAbsoluteLevel >> (3 + ulog2(sizeof(uint)))) + ((coercedAbsoluteLevel & (MAX_EXACT_LEVEL - 1)) ? 1 : 0);
      pair[mwcpLevelIdentifier] += upower(2, coercedAbsoluteLevel - ((mwcpLevelIdentifier - 1) * MAX_EXACT_LEVEL) - 1 );
    }
    relativePair = relativePair >> 1;
  }
}
DistributionObj *stackRandomCovariatesDefault(uint treeID, NodeBase *parent, uint mtry) {
  char *permissible;
  DistributionObj *obj = makeDistributionObjRaw();
  permissible = parent -> permissible;
  obj -> permissibleIndex    = NULL;
  obj -> permissible         = permissible;
  obj -> permissibleSize     = parent -> xSize;
  obj -> augmentationSize    = NULL;
  obj -> weightType          = RF_xWeightType;
  obj -> weight              = RF_xWeight;
  obj -> weightSorted        = RF_xWeightSorted;
  obj -> densityAllocSize    = RF_xWeightDensitySize;
  initializeCDFNew(treeID, obj);
  return obj;
}
void unstackRandomCovariatesDefault(uint treeID, DistributionObj *obj) {
  discardCDFNew(treeID, obj);
  freeDistributionObjRaw(obj);
}
DistributionObj *stackRandomCovariatesSimple(uint treeID, NodeBase *parent, uint mtry) {
  char *permissible;
  uint  xSize;
  uint indexSize;
  DistributionObj *obj = makeDistributionObjRaw();
  xSize = parent -> xSize;
  permissible = parent -> permissible;
  obj -> indexSizeAlloc = xSize;
  obj -> index = uivector(1, xSize);
  indexSize = 0;
  for (uint p = 1; p <= xSize; p++) {
    if (permissible[p]) {
      (obj -> index)[++indexSize] = p;
    }
  }
  obj -> indexSize = indexSize;
  if (indexSize <= mtry) {
    obj -> deterministicFlag = TRUE;
  }
  else {
    obj -> deterministicFlag = FALSE;
  }
  return obj;
}
void unstackRandomCovariatesSimple(uint treeID, DistributionObj *obj) {
  if (obj -> indexSizeAlloc > 0) {
    if (obj -> index != NULL) {
      free_uivector(obj -> index, 1, obj -> indexSizeAlloc);
      obj -> index = NULL;
    }
    obj -> indexSizeAlloc = 0;
  }
  freeDistributionObjRaw(obj);
}
char selectRandomCovariatesDefault(uint      treeID,
                                   NodeBase *parent,
                                   DistributionObj *obj,
                                   uint      mtry,
                                   char     *factorFlag,
                                   uint     *xvarID,
                                   uint     *xvarCount) {
  char found;
  (*xvarID) = UINT_MAX;
  found = FALSE;
  *factorFlag = FALSE;
  while ( ((*xvarCount) < mtry) && ((*xvarID) != 0) && (found == FALSE)) {
    (*xvarCount) ++;
    *xvarID = sampleFromCDFNew(ran1B, treeID, obj);
    if (*xvarID != 0) {
      updateCDFNew(treeID, obj);
      found = TRUE;
      if (RF_xType[*xvarID] == 'C') {
        *factorFlag = TRUE;
      }
    }  
  }  
  return found;
}
char selectRandomCovariatesSimple(uint      treeID,
                                  NodeBase *parent,
                                  DistributionObj *obj,
                                  uint      mtry,
                                  char     *factorFlag,
                                  uint     *xvarID,
                                  uint     *xvarCount) {
  char found;
  (*xvarID) =  UINT_MAX;
  found = FALSE;
  *factorFlag = FALSE;
  while ( ((*xvarCount) < mtry) && ((*xvarID) != 0) && (found == FALSE)) {
    if (obj -> indexSize > 0) {
      (*xvarCount) ++;
      if (mtry == 1) {
        obj -> slot = (uint) ceil(ran1B(treeID) * ((obj -> indexSize) * 1.0));
        *xvarID = obj -> index[obj -> slot];
        found = TRUE;
      }
      else {
        if (obj -> deterministicFlag) {
          *xvarID = (obj -> index)[obj -> indexSize];
          (obj -> indexSize) --;
          found = TRUE;
        }
        else {
          obj -> slot = (uint) ceil(ran1B(treeID) * ((obj -> indexSize) * 1.0));
          *xvarID = (obj -> index)[obj -> slot];
          (obj -> index)[obj -> slot] = (obj -> index)[obj -> indexSize];
          (obj -> indexSize) --;
          found = TRUE;
        }
      }
      if (RF_xType[*xvarID] == 'C') {
        *factorFlag = TRUE;
      }
    }
    else {
      *xvarID = 0;
    }
  }  
  return found;
}
uint stackAndConstructSplitVectorGenericPhase1 (uint       treeID,
                                                NodeBase  *parent,
                                                uint       covariate,
                                                double    *observation, ...) {
  double *splitVectorTemp;
  uint vectorSize;
  uint i;
  uint  *repMembrIndx = parent -> repMembrIndx;
  uint   repMembrSize = parent -> repMembrSize;
  va_list list;
  va_start(list, observation);
  double *splitVector = va_arg(list, double*);
  uint **indxx = (uint**) va_arg(list, uint**);
  splitVectorTemp = dvector(1, repMembrSize);
  vectorSize = 0;
  for (i = 1; i <= repMembrSize; i++) {
    splitVectorTemp[i] = observation[repMembrIndx[i]];
  }
  if ((repMembrSize) >= 2) {
    (*indxx) = uivector(1, repMembrSize);
    indexx((repMembrSize),
           splitVectorTemp,
           (*indxx));
    splitVector[1] = splitVectorTemp[(*indxx)[1]];
    vectorSize = 1;
    for (i = 2; i <= (repMembrSize); i++) {
      if (splitVectorTemp[(*indxx)[i]] > splitVector[vectorSize]) {
        vectorSize ++;
        splitVector[vectorSize] = splitVectorTemp[(*indxx)[i]];
      }
    }
    if(vectorSize >= 2) {
    }
    else {
      vectorSize = 0;
      if (covariate <= RF_xSize) {
        (parent -> permissible)[covariate] = FALSE;
      }
      free_uivector(*indxx, 1, repMembrSize);
    }
  }
  else {
    vectorSize = 0;
    (parent -> permissible)[covariate] = FALSE;
  }
  free_dvector(splitVectorTemp, 1, repMembrSize);
  return vectorSize;
}
uint stackAndConstructSplitVectorGenericPhase2 (uint         treeID,
                                                NodeBase    *parent,
                                                uint         covariate,
                                                double      *splitVector,
                                                uint         vectorSize,
                                                char         factorFlag,
                                                char        *deterministicSplitFlag,
                                                uint        *mwcpSizeAbsolute,
                                                void       **splitVectorPtr) {
  uint repMembrSize;
  uint  sworIndex;
  uint *sworVector;
  uint  sworVectorSize;
  uint j, j2, k2;
  uint factorSizeAbsolute;
  uint offset;
  uint splitLength;
  uint relativePair;
  repMembrSize = parent -> repMembrSize;
  splitLength = 0;  
  (*splitVectorPtr) = NULL;  
  if (vectorSize < 2) {
    RF_nativeError("\nRF-SRC:  *** ERROR *** ");
    RF_nativeError("\nRF-SRC:  Split vector is of insufficient size in Stack Phase II allocation:  %10d", vectorSize);
    RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
    RF_nativeExit();
  }
  if (factorFlag) {
    if(RF_factorList[treeID][vectorSize] == NULL) {
      RF_factorList[treeID][vectorSize] = makeFactor(vectorSize, FALSE);
    }
    factorSizeAbsolute = RF_xFactorSize[RF_xFactorMap[covariate]];
    *mwcpSizeAbsolute = RF_factorList[treeID][factorSizeAbsolute] -> mwcpSize;
    if (SG_splitRule == RAND_CRT) {
      splitLength = 1 + 1;
      *deterministicSplitFlag = FALSE;
    }
    else {
      if (RF_nsplit == 0) {
        *deterministicSplitFlag = TRUE;
        if ((RF_factorList[treeID][vectorSize] -> r) > MAX_EXACT_LEVEL) {
          *deterministicSplitFlag = FALSE;
        }
        else {
          if ( *((uint *) RF_factorList[treeID][vectorSize] -> complementaryPairCount) >= repMembrSize ) {
            *deterministicSplitFlag = FALSE;
          }
        }
        if (*deterministicSplitFlag == FALSE) {
          splitLength = repMembrSize + 1;
        }
        else {
          splitLength = *((uint*) RF_factorList[treeID][vectorSize] -> complementaryPairCount) + 1;
        }
      }
      else {
        *deterministicSplitFlag = FALSE;
        if ((RF_factorList[treeID][vectorSize] -> r) <= MAX_EXACT_LEVEL) {
          if (*((uint*) RF_factorList[treeID][vectorSize] -> complementaryPairCount) <= ((RF_nsplit <= repMembrSize) ? RF_nsplit : repMembrSize)) {
            splitLength = *((uint*) RF_factorList[treeID][vectorSize] -> complementaryPairCount) + 1;
            *deterministicSplitFlag = TRUE;
          }
        }
        if (*deterministicSplitFlag == FALSE) {
          splitLength = 1 + ((RF_nsplit <= repMembrSize) ? RF_nsplit : repMembrSize);
        }
      }  
    }  
    (*splitVectorPtr) = uivector(1, splitLength * (*mwcpSizeAbsolute));
    for (offset = 1; offset <= *mwcpSizeAbsolute; offset++) {
      ((uint*) (*splitVectorPtr) + ((splitLength - 1) * (*mwcpSizeAbsolute)))[offset] = 0;
    }
    if (*deterministicSplitFlag) {
      bookFactor(RF_factorList[treeID][vectorSize]);
      j2 = 0;
      for (j = 1; j <= RF_factorList[treeID][vectorSize] -> cardinalGroupCount; j++) {
        for (k2 = 1; k2 <= ((uint*) RF_factorList[treeID][vectorSize] -> cardinalGroupSize)[j]; k2++) {
          ++j2;
          relativePair = (RF_factorList[treeID][vectorSize] -> cardinalGroupBinary)[j][k2];
          convertRelToAbsBinaryPair(treeID,
                                    vectorSize,
                                    factorSizeAbsolute,
                                    relativePair,
                                    splitVector,
                                    (uint*) (*splitVectorPtr) + ((j2 - 1) * (*mwcpSizeAbsolute)));
        }
      }
    }  
    else {
      for (j = 1; j < splitLength; j++) {
        getRandomPair(treeID, vectorSize, factorSizeAbsolute, splitVector, (uint*) (*splitVectorPtr) + ((j - 1) * (*mwcpSizeAbsolute)));
      }
    }
  }  
  else {
    if (SG_splitRule == RAND_CRT) {
      splitLength = 1 + 1;
      *deterministicSplitFlag = FALSE;
    }
    else {
      if(RF_nsplit == 0) {
        splitLength = vectorSize;
        (*splitVectorPtr) = splitVector;
        *deterministicSplitFlag = TRUE;
      }
      else {
        if (vectorSize <= RF_nsplit + 1) {
          splitLength = vectorSize;
          (*splitVectorPtr) = splitVector;
          *deterministicSplitFlag = TRUE;
        }
        else {
          splitLength = RF_nsplit + 1;
          *deterministicSplitFlag = FALSE;
        }
      }  
    }  
    if (*deterministicSplitFlag == FALSE) {
      (*splitVectorPtr) = dvector(1, splitLength);
      ((double*) (*splitVectorPtr))[splitLength] = 0;
      if (SG_splitRule == RAND_CRT) {
        ((double*) (*splitVectorPtr))[1]  = splitVector[(uint) ceil(ran1B(treeID) * ((vectorSize - 1) * 1.0))];
      }
      else {
        sworVector = uivector(1, vectorSize);
        sworVectorSize = vectorSize - 1;
        for (j = 1; j <= sworVectorSize; j++) {
          sworVector[j] = j;
        }
        for (j = 1; j < splitLength; j++) {
          sworIndex = (uint) ceil(ran1B(treeID) * (sworVectorSize * 1.0));
          ((double*) (*splitVectorPtr))[j]  = splitVector[sworVector[sworIndex]];
          sworVector[sworIndex] = sworVector[sworVectorSize];
          sworVectorSize --;
        }
        free_uivector (sworVector, 1, vectorSize);
        qksort(((double*) (*splitVectorPtr)), splitLength-1);
      }
    }
  }  
  return splitLength;
}
void unstackSplitVectorGeneric(uint       treeID,
                               NodeBase  *parent,
                               uint       splitLength,
                               char       factorFlag,
                               uint       splitVectorSize,
                               uint       mwcpSizeAbsolute,
                               char       deterministicSplitFlag,
                               void      *splitVectorPtr,
                               uint      *indxx) {
  if (splitLength > 0) {
    if (factorFlag == TRUE) {
      free_uivector(splitVectorPtr, 1, splitLength * mwcpSizeAbsolute);
      if (deterministicSplitFlag == FALSE) {
        if (splitVectorSize > SAFE_FACTOR_SIZE) {
          unbookFactor(RF_factorList[treeID][splitVectorSize]);
        }
      }
    }
    else {
      if (deterministicSplitFlag == FALSE) {
        free_dvector(splitVectorPtr, 1, splitLength);
      }
    }
    if (indxx != NULL) {
      free_uivector((indxx), 1, parent -> repMembrSize);
    }
  }
}
uint virtuallySplitNode(uint      treeID,
                        NodeBase *parent,
                        char  factorFlag,
                        uint  mwcpSizeAbsolute,
                        double *observation,
                        uint *indxx,
                        uint  priorMembrIter,
                        void *splitVectorPtr,
                        uint  offset,
                        char *localSplitIndicator,
                        uint *leftSize,
                        uint *currentMembrIter) {
  uint *repMembrIndx;
  uint  repMembrSize;
  char daughterFlag;
  char iterFlag;
  iterFlag = TRUE;
  repMembrIndx = parent -> repMembrIndx;
  repMembrSize = parent -> repMembrSize;
  *currentMembrIter = priorMembrIter;
  while (iterFlag) {
    (*currentMembrIter) ++;
    if (factorFlag == TRUE) {
      daughterFlag = splitOnFactor((uint)  observation[    repMembrIndx[*currentMembrIter]     ],
                                   (uint*) splitVectorPtr + ((offset - 1) * mwcpSizeAbsolute));
      localSplitIndicator[     repMembrIndx[*currentMembrIter]   ] = daughterFlag;
      if ((*currentMembrIter) == repMembrSize) {
        iterFlag = FALSE;
      }
    }
    else {
      if ((((double*) splitVectorPtr)[offset] - observation[   repMembrIndx[indxx[*currentMembrIter]]    ]) >= 0.0) {
        daughterFlag = LEFT;
      }
      else {
        daughterFlag = RIGHT;
        iterFlag = FALSE;
      }
      localSplitIndicator[     repMembrIndx[indxx[*currentMembrIter]]   ] = daughterFlag;
    }
    if (daughterFlag == LEFT) {
      (*leftSize) ++;
    }
  }  
  if ((*leftSize == 0) || (*leftSize == repMembrSize)) {
    RF_nativeError("\nRF-SRC:  *** ERROR *** ");
    RF_nativeError("\nRF-SRC:  Left or Right Daughter of size zero:  (%10d, %10d)", *leftSize, repMembrSize);
    RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
    RF_nativeExit();
  }
  return (*leftSize);
}
char updateMaximumSplitCart(uint    treeID,
                            Node   *parent,
                            double  delta,
                            uint    covariate,
                            uint    index,
                            char    factorFlag,
                            uint    mwcpSizeAbsolute,
                            void   *splitVectorPtr,
                            SplitInfoMax *infoMax) {
  char flag;
  uint k;
  if(RF_nativeIsNaN(delta)) {
    flag = FALSE;
  }
  else {
    if(RF_nativeIsNaN(infoMax -> delta)) {
      flag = TRUE;
    }
    else {
      if ((delta - (infoMax -> delta)) > EPSILON2) {
        flag = TRUE;
      }
      else {
        flag = FALSE;
      }
    }
  }
  if (flag) {
    infoMax -> delta = delta;
    infoMax -> splitParameter = covariate;
    if (factorFlag == TRUE) {
      if (infoMax -> splitValueFactSize > 0) {
        if (infoMax -> splitValueFactSize != mwcpSizeAbsolute) {
          free_uivector(infoMax -> splitValueFactPtr, 1, infoMax -> splitValueFactSize);
          infoMax -> splitValueFactSize = mwcpSizeAbsolute;
          infoMax -> splitValueFactPtr = uivector(1, infoMax -> splitValueFactSize);
        }
      }
      else {
        infoMax -> splitValueFactSize = mwcpSizeAbsolute;
        infoMax -> splitValueFactPtr = uivector(1, infoMax -> splitValueFactSize);
      }
      infoMax -> splitValueCont = RF_nativeNaN;
      for (k=1; k <= infoMax -> splitValueFactSize; k++) {
        (infoMax -> splitValueFactPtr)[k] =
          ((uint*) splitVectorPtr + ((index - 1) * (infoMax -> splitValueFactSize)))[k];
      }
    }
    else {
      if (infoMax -> splitValueFactSize > 0) {
        free_uivector(infoMax -> splitValueFactPtr, 1, infoMax -> splitValueFactSize);
        infoMax -> splitValueFactSize = 0;
        infoMax -> splitValueFactPtr = NULL;
      }
      else {
      }
      infoMax -> splitValueCont = ((double*) splitVectorPtr)[index];
    }
  }
  else {
  }
  return flag;
}
