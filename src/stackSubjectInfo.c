
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "shared/globalCore.h"
#include           "shared/externalCore.h"
#include           "global.h"
#include           "external.h"

// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "stackSubjectInfo.h"
#include "shared/nrutil.h"
#include "shared/error.h"
char stackSubjectArraysOnly(char     mode,
                            uint     timeIndex,
                            uint     statusIndex,
                            uint     startTimeIndex,
                            uint     observationSize,
                            uint    *subjIn,
                            uint    *subjSize,
                            uint   **subjSlot,
                            uint   **subjSlotCount,
                            uint  ***subjList,
                            uint   **caseMap,
                            uint   **subjMap,
                            uint    *subjCount) {
  uint i, j;
  char result;
  result = TRUE;
  *subjList = NULL;
  if ((timeIndex > 0) && (statusIndex > 0)) {
    if (startTimeIndex > 0) {
      RF_opt                  = RF_opt & (~OPT_PERF);
      RF_opt                  = RF_opt & (~OPT_VIMP);
      *subjSlot = uivector(1, observationSize);
      *subjSlotCount = uivector(1, observationSize);
      for (i = 1; i <= observationSize; i++) {
        (*subjSlotCount)[i] = 0;
      }
      *caseMap = uivector(1, observationSize);
      uint   *sortedIdx = uivector(1, observationSize);
      indexxui(observationSize, subjIn, sortedIdx);
      *subjCount = 1;
      (*subjSlotCount)[1] = 1;      
      (*subjSlot)[1] = subjIn[sortedIdx[1]]; 
      (*caseMap)[sortedIdx[1]] = 1;
      for (i = 2; i <= observationSize; i++) {
        if (subjIn[sortedIdx[i]] > (*subjSlot)[*subjCount]) {
          (*subjCount) ++;
          (*subjSlot)[*subjCount] = subjIn[sortedIdx[i]];
        }
        (*subjSlotCount)[*subjCount] ++;
        (*caseMap)[sortedIdx[i]] = *subjCount;
      }
      for (i = (*subjCount) + 1; i <= observationSize; i++) {
        (*subjSlot)[i] = 0;
      }
      *subjMap = uivector(1, (*subjSlot)[*subjCount]);
      for (i = 1; i <= (*subjSlot)[*subjCount]; i++) {
        (*subjMap)[i] = 0;
      }
      for (i = 1; i <= *subjCount; i++) {
        (*subjMap)[(*subjSlot)[i]] = i;
      }
      if (mode != RF_PRED) {
        if (*subjSize == 0) {
          *subjSize = *subjCount;
        }
      }
      else {
        if (*subjSize == 0) {
          *subjSize = *subjCount;        
        }
      }
      if (*subjCount != *subjSize) {
        RF_nativeError("\nRF-SRC: *** ERROR *** ");
        RF_nativeError("\nRF-SRC: Subject count found in cases inconsistent with incoming subject size:  %10d vs %10d", *subjCount, *subjSize);
        result = FALSE;
      }
      else {
        *subjList = (uint **) new_vvector(1, *subjCount, NRUTIL_UPTR);
        uint *tempSubjIter = uivector(1, *subjCount);
        for (i = 1; i <= *subjCount; i++) {
          (*subjList)[i] = uivector(1, (*subjSlotCount)[i]);
          tempSubjIter[i] = 0;
        }
        for (i = 1; i <= observationSize; i++) {
          (*subjList)[(*caseMap)[i]][++tempSubjIter[(*caseMap)[i]]] = i;
        }
        free_uivector(tempSubjIter, 1, *subjCount);
      }
      free_uivector(sortedIdx, 1, observationSize);
    }
  }
  return result;
}
void unstackSubjectArraysOnly(char     mode,
                              uint     timeIndex,
                              uint     statusIndex,
                              uint     startTimeIndex,
                              uint     observationSize,
                              uint     subjCount,
                              uint    *subjSlot,
                              uint    *subjSlotCount,
                              uint   **subjList,
                              uint    *caseMap,
                              uint    *subjMap) {
  uint i;
  if ((RF_timeIndex > 0) && (RF_statusIndex > 0)) {
    if (startTimeIndex > 0) {
      free_uivector(subjMap, 1, subjSlot[subjCount]);
      free_uivector(subjSlot, 1, observationSize);
      free_uivector(caseMap, 1, observationSize);
      if (subjList != NULL) {
        for (i = 1; i <= subjCount; i++) {
          free_uivector(subjList[i], 1, subjSlotCount[i]);
        }
        free_uivector(subjSlotCount, 1, observationSize);
        free_new_vvector(subjList, 1, subjCount, NRUTIL_UPTR);
      }
    }
  }
}
void stackSubjectTimeInterestIndex(char mode,
                                   uint timeIndex,
                                   uint statusIndex,
                                   uint startTimeIndex,
                                   uint observationSize,
                                   double **responseIn,
                                   uint  subjCount,
                                   uint *subjIn,
                                   uint *subjMap,
                                   uint *subjSlot,
                                   uint    sortedTimeInterestSize,
                                   double *timeInterest,
                                   uint  **timeInterestIntervalCount,
                                   uint ***timeInterestIntervalIndex,
                                   uint  **timeInterestSubjTailIndex,
                                   uint  **subjTailCaseMap) {
        *timeInterestIntervalCount = uivector(1, observationSize);
        *timeInterestIntervalIndex = (uint **) new_vvector(1, observationSize, NRUTIL_UPTR);
        *timeInterestSubjTailIndex = uivector(1, subjCount);
        *subjTailCaseMap = uivector(1, subjCount);
        for (uint i = 1; i <= subjCount; i++) {
          (*timeInterestSubjTailIndex)[i] = 0;
          (*subjTailCaseMap)[i] = 0;
        }
        double *subjTailCaseMaxValue = dvector(1, subjCount);
        for (uint i = 1; i <= subjCount; i++) {
          subjTailCaseMaxValue[i] = 0.0;
        }
        for (uint i = 1; i <= observationSize; i++) {
          if (responseIn[RF_timeIndex][i] > subjTailCaseMaxValue[subjMap[subjIn[i]]]) {
            subjTailCaseMaxValue[subjMap[subjIn[i]]] = responseIn[timeIndex][i];
            (*subjTailCaseMap)[subjMap[subjIn[i]]] = i;
          }
          (*timeInterestIntervalCount)[i] = 0;
          for (uint j = 1; j <= sortedTimeInterestSize; j++) {
            if ((responseIn[startTimeIndex][i] < timeInterest[j]) &&
                (timeInterest[j] <= responseIn[timeIndex][i])) {
              (*timeInterestIntervalCount)[i] ++;
            }
          }
        }
        for (uint i = 1; i <= observationSize; i++) {
          if ((*timeInterestIntervalCount)[i] > 0) {
            (*timeInterestIntervalIndex)[i] = uivector(1, (*timeInterestIntervalCount)[i]);
            uint k = 0;
            for (uint j = 1; j <= sortedTimeInterestSize; j++) {
              if ((responseIn[startTimeIndex][i] < timeInterest[j]) &&
                  (timeInterest[j] <= responseIn[timeIndex][i])) {
                (*timeInterestIntervalIndex)[i][++k] = j;
                if ((*timeInterestSubjTailIndex)[subjMap[subjIn[i]]] < j) {
                  (*timeInterestSubjTailIndex)[subjMap[subjIn[i]]] = j;
                }
              }
            }
          }
          else {
            (*timeInterestIntervalIndex)[i] = NULL;
          }
        }
        for (uint j = 1; j <= subjCount; j++) {
          uint i = (*subjTailCaseMap)[j];
          if (i < 1 || i > observationSize) {
            RF_nativeError("\nRF-SRC: *** ERROR *** ");
            RF_nativeError("\nRF-SRC: subjTailCaseMap[%u] = %u out of bounds (1..%u).", j, i, observationSize);
            if (FALSE) {
              RF_nativeError("\nTime Interest Count:  ");
              RF_nativeError("\n      index    subject           start time            stop time      count");
              for (uint i = 1; i <= observationSize; i++) {
                RF_nativeError("\n %10d %10d %20.8f %20.8f %10d", i, subjIn[i], responseIn[startTimeIndex][i], responseIn[timeIndex][i], (*timeInterestIntervalCount)[i]);
              }
              RF_nativeError("\n");
            }
            RF_nativeExit();
          }
          uint tailCount = sortedTimeInterestSize - (*timeInterestSubjTailIndex)[j];
          if (tailCount > 0) {
            if ((*timeInterestIntervalCount)[i] > 0) {
              uint *tempTIII = uivector(1, (*timeInterestIntervalCount)[i] + tailCount);
              for (uint k = 1; k <= (*timeInterestIntervalCount)[i]; k++) {
                tempTIII[k] = (*timeInterestIntervalIndex)[i][k];
              }
              for (uint k = 1; k <= tailCount; k++) {
                tempTIII[(*timeInterestIntervalCount)[i] + k] = (*timeInterestSubjTailIndex)[j] + k;
              }
              free_uivector((*timeInterestIntervalIndex)[i], 1, (*timeInterestIntervalCount)[i]);
              (*timeInterestIntervalIndex)[i] = tempTIII;
              (*timeInterestIntervalCount)[i] = (*timeInterestIntervalCount)[i] + tailCount;
            }
            else {
              (*timeInterestIntervalIndex)[i] = uivector(1, tailCount);
              for (uint k = 1; k <= tailCount; k++) {
                (*timeInterestIntervalIndex)[i][k] = (*timeInterestSubjTailIndex)[j] + k;
              }
              (*timeInterestIntervalCount)[i] = tailCount;
            }
          }
        }
        free_dvector(subjTailCaseMaxValue, 1, subjCount);
}
void unstackSubjectTimeInterestIndex(char mode,
                                     uint observationSize,
                                     uint  subjCount,
                                     uint  *timeInterestIntervalCount,
                                     uint **timeInterestIntervalIndex,
                                     uint  *timeInterestSubjTailIndex,
                                     uint  *subjTailCaseMap) {
  for (uint i = 1; i <= observationSize; i++) {
    if (timeInterestIntervalCount[i] > 0) {
      free_uivector(timeInterestIntervalIndex[i], 1, timeInterestIntervalCount[i]);
    }
  }
  free_new_vvector(timeInterestIntervalIndex, 1, observationSize, NRUTIL_UPTR);
  free_uivector(timeInterestIntervalCount, 1, observationSize);
  free_uivector(timeInterestSubjTailIndex, 1, subjCount);
  free_uivector(subjTailCaseMap, 1, subjCount);
}
