
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "shared/globalCore.h"
#include           "shared/externalCore.h"
#include           "global.h"
#include           "external.h"

// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "processEnsemble.h"
#include "survivalTDC.h"
#include "terminal.h"
#include "shared/classification.h"
#include "shared/regression.h"
#include "shared/terminalBase.h"
#include "shared/termBaseOps.h"
#include "shared/nrutil.h"
#include "shared/error.h"
void processEnsembleInSitu(char mode, uint treeID) {
  char perfFlag;
  if ((RF_opt & OPT_PERF) ||
      (RF_opt & OPT_OENS) ||
      (RF_opt & OPT_IENS) ||
      (RF_opt & OPT_FENS)) {
#ifdef _OPENMP
    omp_set_lock(&RF_lockPerf);
#endif
    RF_serialTreeIndex[++RF_serialTreeID] = treeID;
    perfFlag = getPerfFlag(mode, RF_serialTreeID);
    if (!perfFlag) {
#ifdef _OPENMP
      omp_unset_lock(&RF_lockPerf);
#endif
    }
#ifdef _OPENMP
    omp_set_lock(&RF_lockEnsbUpdtCount);
#endif
    RF_ensembleUpdateCount++;
#ifdef _OPENMP
    omp_unset_lock(&RF_lockEnsbUpdtCount);
#endif
    updateEnsemble(mode, treeID);
#ifdef _OPENMP
    omp_set_lock(&RF_lockEnsbUpdtCount);
#endif
    RF_ensembleUpdateCount--;
#ifdef _OPENMP
    omp_unset_lock(&RF_lockEnsbUpdtCount);
#endif
    if (perfFlag) {
      char ensbUpdtCountFlag = FALSE;
      while (!ensbUpdtCountFlag) {
#ifdef _OPENMP
        omp_set_lock(&RF_lockEnsbUpdtCount);
#endif
        if (RF_ensembleUpdateCount  == 0) {
          ensbUpdtCountFlag = TRUE;
        }
#ifdef _OPENMP
        omp_unset_lock(&RF_lockEnsbUpdtCount);
#endif
      }
      normalizeEnsembleEstimates(mode);
#ifdef _OPENMP
      omp_unset_lock(&RF_lockPerf);
#endif
    }  
  }  
}
void updateEnsemble (char mode, uint treeID) {
  if ((mode == RF_GROW) || (mode == RF_REST)) {
    updateEnsembleGrow(mode, treeID);
  }
  else {
    updateEnsemblePred(mode, treeID);
  }
}
void updateEnsembleGrow(char mode, uint treeID) {
  char oobFlag, fullFlag, outcomeFlag;
  double  **ensembleKHZnum;
  double  **ensembleCHFnum;
  double  **ensembleHRWnum;
  double   *ensembleDen;
  double  **subjectHazardIBG;
  double  **subjectHazardOOB;
  double  **subjectNelsonAalenIBG;
  double  **subjectNelsonAalenOOB;
  double  **subjectHazardRawIBG;
  double  **subjectHazardRawOOB;
#ifdef _OPENMP
  omp_lock_t   *lockDENptr;
#endif
  LeafLinkedObj *leafLinkedPtr;
  uint subj, i, j, ii, jj;
  Terminal     *tTerm;
  TerminalBase *tTermBase;
  TerminalSurvival *sTerm;
  TerminalBase **subjTailPtrIBG, **subjTailPtrOOB;
  uint *subjMaxTimeIndxIBG, *subjMaxTimeIndxOOB;
  subjectHazardIBG = dmatrix(1, RF_subjCount, 1, RF_sortedTimeInterestSize);
  subjectHazardOOB = dmatrix(1, RF_subjCount, 1, RF_sortedTimeInterestSize);
  subjectNelsonAalenIBG = dmatrix(1, RF_subjCount, 1, RF_sortedTimeInterestSize);
  subjectNelsonAalenOOB = dmatrix(1, RF_subjCount, 1, RF_sortedTimeInterestSize);
  subjectHazardRawIBG = dmatrix(1, RF_subjCount, 1, RF_sortedTimeInterestSize);
  subjectHazardRawOOB = dmatrix(1, RF_subjCount, 1, RF_sortedTimeInterestSize);
  subjTailPtrIBG      = (TerminalBase **) new_vvector(1, RF_subjCount, NRUTIL_TPTR);
  subjMaxTimeIndxIBG  = uivector(1, RF_subjCount);
  subjTailPtrOOB      = (TerminalBase **) new_vvector(1, RF_subjCount, NRUTIL_TPTR);
  subjMaxTimeIndxOOB  = uivector(1, RF_subjCount);
  for (i = 1; i <= RF_subjCount; i++) {
    for (jj = 1; jj <= RF_sortedTimeInterestSize; jj++) {
      subjectHazardIBG[i][jj] = 0.0;
      subjectHazardOOB[i][jj] = 0.0;
      subjectNelsonAalenIBG[i][jj] = 0.0;
      subjectNelsonAalenOOB[i][jj] = 0.0;
      subjectHazardRawIBG[i][jj] = 0.0;
      subjectHazardRawOOB[i][jj] = 0.0;
    }
    subjTailPtrIBG[i] = NULL;
    subjTailPtrOOB[i] = NULL;
    subjMaxTimeIndxIBG[i] = 0;
    subjMaxTimeIndxOOB[i] = 0;
  }
  if ((mode == RF_GROW) || (mode == RF_REST)) {
    leafLinkedPtr = RF_leafLinkedObjHead[treeID] -> fwdLink;
    while (leafLinkedPtr != NULL) {
      tTermBase = leafLinkedPtr -> termPtr;
      tTerm = (Terminal *) tTermBase;
      sTerm = tTermBase -> survivalBase;
      for (i = 1; i <= tTerm -> ibgMembrCount; i++) {
        ii = tTerm -> ibgMembrIndx[i];
        subj = RF_subjIn[ii];
        for (jj = 1; jj <= SG_timeInterestIntervalCount[ii]; jj++) {
          subjectHazardIBG[RF_subjMap[subj]][SG_timeInterestIntervalIndex[ii][jj]] = sTerm -> hazard[SG_timeInterestIntervalIndex[ii][jj]];
          subjectNelsonAalenIBG[RF_subjMap[subj]][SG_timeInterestIntervalIndex[ii][jj]] = sTerm -> nelsonAalen[SG_timeInterestIntervalIndex[ii][jj]];
          subjectHazardRawIBG[RF_subjMap[subj]][SG_timeInterestIntervalIndex[ii][jj]] = tTerm -> ratio[SG_timeInterestIntervalIndex[ii][jj]];
        }
        if (SG_timeInterestIntervalCount[ii] > 0) {
          if (subjMaxTimeIndxIBG[RF_subjMap[subj]] < SG_timeInterestIntervalIndex[ii][ SG_timeInterestIntervalCount[ii] ]) {
            subjMaxTimeIndxIBG[RF_subjMap[subj]] = SG_timeInterestIntervalIndex[ii][ SG_timeInterestIntervalCount[ii] ];
            subjTailPtrIBG[RF_subjMap[subj]] = tTermBase;
          }
        }
      }
      for (i = 1; i <= tTerm -> oobMembrCount; i++) {
        ii = tTerm -> oobMembrIndx[i];
        subj = RF_subjIn[ii];
        for (jj = 1; jj <= SG_timeInterestIntervalCount[ii]; jj++) {
          subjectHazardOOB[RF_subjMap[subj]][SG_timeInterestIntervalIndex[ii][jj]] = sTerm -> hazard[SG_timeInterestIntervalIndex[ii][jj]];
          subjectNelsonAalenOOB[RF_subjMap[subj]][SG_timeInterestIntervalIndex[ii][jj]] = sTerm -> nelsonAalen[SG_timeInterestIntervalIndex[ii][jj]];
          subjectHazardRawOOB[RF_subjMap[subj]][SG_timeInterestIntervalIndex[ii][jj]] = tTerm -> ratio[SG_timeInterestIntervalIndex[ii][jj]];          
        }
        if (SG_timeInterestIntervalCount[ii] > 0) {
          if (subjMaxTimeIndxOOB[RF_subjMap[subj]] < SG_timeInterestIntervalIndex[ii][ SG_timeInterestIntervalCount[ii] ]) {
            subjMaxTimeIndxOOB[RF_subjMap[subj]] = SG_timeInterestIntervalIndex[ii][ SG_timeInterestIntervalCount[ii] ];
            subjTailPtrOOB[RF_subjMap[subj]] = tTermBase;
          }
        }
      }
      leafLinkedPtr = leafLinkedPtr -> fwdLink;
    }
  }
  double **subjectHazard;
  double **subjectNelsonAalen;
  double **subjectHazardRaw;
  uint     membershipSize;
  uint    *membershipIndex;
  ensembleKHZnum = NULL;  
  ensembleCHFnum = NULL;  
  ensembleHRWnum = NULL;  
  ensembleDen    = NULL;  
  oobFlag = fullFlag = FALSE;
  if (RF_opt & OPT_OENS) {
    if (RF_oobSize[treeID] > 0) {
      oobFlag = TRUE;
    }
  }
  if (RF_opt & OPT_IENS) {
    fullFlag = TRUE;
  }
  outcomeFlag = TRUE;
  while ((oobFlag == TRUE) || (fullFlag == TRUE)) {
    if (oobFlag == TRUE) {
      ensembleKHZnum = SG_oobEnsembleKHZnum;
      ensembleCHFnum = SG_oobEnsembleCHFnum;
      ensembleHRWnum = SG_oobEnsembleHRWnum;
      ensembleDen    = RF_oobEnsembleDen;
      membershipSize  = RF_oobSize[treeID];
      membershipIndex = RF_oobMembershipIndex[treeID];
      subjectHazard      = subjectHazardOOB;
      subjectNelsonAalen = subjectNelsonAalenOOB;
      subjectHazardRaw   = subjectHazardRawOOB;
#ifdef _OPENMP
      lockDENptr      = RF_lockDENoens;
#endif
    }
    else {
      ensembleKHZnum = SG_fullEnsembleKHZnum;
      ensembleCHFnum = SG_fullEnsembleCHFnum;
      ensembleHRWnum  = SG_fullEnsembleHRWnum;
      ensembleDen    = RF_fullEnsembleDen;
      membershipSize  = RF_ibgSize[treeID];
      membershipIndex = RF_ibgMembershipIndex[treeID];
      subjectHazard      = subjectHazardIBG;
      subjectNelsonAalen = subjectNelsonAalenIBG;
      subjectHazardRaw   = subjectHazardRawIBG;
#ifdef _OPENMP
      lockDENptr      = RF_lockDENfens;
#endif
    }
    for (i = 1; i <= membershipSize; i++) {
      ii = membershipIndex[i];
#ifdef _OPENMP        
      omp_set_lock(&(lockDENptr[ii]));
#endif
      ensembleDen[ii] ++;
      for (j = 1; j <= RF_sortedTimeInterestSize;  j++) {
        ensembleKHZnum[j][ii] += subjectHazard[ii][j];
        ensembleCHFnum[j][ii] += subjectNelsonAalen[ii][j];
        ensembleHRWnum[j][ii] += subjectHazardRaw[ii][j];
      }
#ifdef _OPENMP
      omp_unset_lock(&(lockDENptr[ii]));
#endif
    }
    if (outcomeFlag == TRUE) {
      outcomeFlag = FALSE;
    }
    if (oobFlag == TRUE) {
      oobFlag = FALSE;
    }
    else {
      fullFlag = FALSE;
    }
  }  
  free_dmatrix(subjectHazardIBG, 1, RF_subjCount, 1, RF_sortedTimeInterestSize);
  free_dmatrix(subjectHazardOOB, 1, RF_subjCount, 1, RF_sortedTimeInterestSize);
  free_dmatrix(subjectNelsonAalenIBG, 1, RF_subjCount, 1, RF_sortedTimeInterestSize);
  free_dmatrix(subjectNelsonAalenOOB, 1, RF_subjCount, 1, RF_sortedTimeInterestSize);
  free_dmatrix(subjectHazardRawIBG, 1, RF_subjCount, 1, RF_sortedTimeInterestSize);
  free_dmatrix(subjectHazardRawOOB, 1, RF_subjCount, 1, RF_sortedTimeInterestSize);
  free_new_vvector(subjTailPtrIBG, 1, RF_subjCount, NRUTIL_TPTR);
  free_uivector(subjMaxTimeIndxIBG, 1, RF_subjCount);
  free_new_vvector(subjTailPtrOOB, 1, RF_subjCount, NRUTIL_TPTR);
  free_uivector(subjMaxTimeIndxOOB, 1, RF_subjCount);
}
void updateEnsemblePred(char mode, uint treeID) {
  char fullFlag;
  double  **ensembleKHZnum;
  double  **ensembleCHFnum;
  double  **ensembleHRWnum;
  double   *ensembleDen;
  double  **subjectHazardTST;
  double  **subjectHazardRawTST;
  double  **subjectNelsonAalenTST;
#ifdef _OPENMP
  omp_lock_t   *lockDENptr;
#endif
  LeafLinkedObj *leafLinkedPtr;
  uint subj, i, j, ii, jj;
  Terminal     *tTerm;
  TerminalBase *tTermBase;
  TerminalSurvival *sTerm;
  TerminalBase **subjTailPtrTST;
  uint *subjMaxTimeIndxTST;
    subjectHazardTST = dmatrix(1, SG_fsubjCount, 1, RF_sortedTimeInterestSize);
    subjectNelsonAalenTST = dmatrix(1, SG_fsubjCount, 1, RF_sortedTimeInterestSize);
    subjectHazardRawTST = dmatrix(1, SG_fsubjCount, 1, RF_sortedTimeInterestSize);
    subjTailPtrTST      = (TerminalBase **) new_vvector(1, SG_fsubjCount, NRUTIL_TPTR);
    subjMaxTimeIndxTST  = uivector(1, SG_fsubjCount);
    for (i = 1; i <= SG_fsubjCount; i++) {
      for (jj = 1; jj <= RF_sortedTimeInterestSize; jj++) {
        subjectHazardTST[i][jj] = 0.0;
        subjectNelsonAalenTST[i][jj] = 0.0;
        subjectHazardRawTST[i][jj] = 0.0;
      }
      subjTailPtrTST[i] = NULL;
      subjMaxTimeIndxTST[i] = 0;
    }
    leafLinkedPtr = RF_leafLinkedObjHead[treeID] -> fwdLink;
    while (leafLinkedPtr != NULL) {
      tTermBase = leafLinkedPtr -> termPtr;
      tTerm = (Terminal *) tTermBase;
      sTerm = tTermBase -> survivalBase;
      for (i = 1; i <= tTerm -> tstMembrCount; i++) {
        ii = tTerm -> tstMembrIndx[i];
        subj = SG_fsubjIn[ii];
        for (jj = 1; jj <= SG_ftimeInterestIntervalCount[ii]; jj++) {
          subjectHazardTST[SG_fsubjMap[subj]][SG_ftimeInterestIntervalIndex[ii][jj]] = sTerm -> hazard[SG_ftimeInterestIntervalIndex[ii][jj]];
          subjectNelsonAalenTST[SG_fsubjMap[subj]][SG_ftimeInterestIntervalIndex[ii][jj]] = sTerm -> nelsonAalen[SG_ftimeInterestIntervalIndex[ii][jj]];
          subjectHazardRawTST[SG_fsubjMap[subj]][SG_ftimeInterestIntervalIndex[ii][jj]] = tTerm -> ratio[SG_ftimeInterestIntervalIndex[ii][jj]];
        }
        if (SG_ftimeInterestIntervalCount[ii] > 0) {
          if (subjMaxTimeIndxTST[SG_fsubjMap[subj]] < SG_ftimeInterestIntervalIndex[ii][ SG_ftimeInterestIntervalCount[ii] ]) {
            subjMaxTimeIndxTST[SG_fsubjMap[subj]] = SG_ftimeInterestIntervalIndex[ii][ SG_ftimeInterestIntervalCount[ii] ];
            subjTailPtrTST[SG_fsubjMap[subj]] = tTermBase;
          }
        }
      }
      leafLinkedPtr = leafLinkedPtr -> fwdLink;
    }
  double **subjectHazard;
  double **subjectNelsonAalen;
  double **subjectHazardRaw;
  uint     membershipSize;
  uint    *membershipIndex;
  ensembleKHZnum = NULL;  
  ensembleCHFnum = NULL;  
  ensembleHRWnum = NULL;  
  ensembleDen    = NULL;  
  fullFlag = FALSE;
  if (RF_opt & OPT_FENS) {
    fullFlag = TRUE;
  }
  while (fullFlag == TRUE) {
    ensembleKHZnum = SG_fullEnsembleKHZnum;
    ensembleCHFnum = SG_fullEnsembleCHFnum;
    ensembleHRWnum = SG_fullEnsembleHRWnum;
    ensembleDen    = RF_fullEnsembleDen;
    membershipSize  = SG_fsubjCount;
    membershipIndex = RF_fidentityMembershipIndex;
    subjectHazard      = subjectHazardTST;
    subjectNelsonAalen = subjectNelsonAalenTST;
    subjectHazardRaw   = subjectHazardRawTST;
#ifdef _OPENMP
    lockDENptr      = RF_lockDENfens;
#endif
    for (i = 1; i <= membershipSize; i++) {
      ii = membershipIndex[i];
#ifdef _OPENMP        
      omp_set_lock(&(lockDENptr[ii]));
#endif
      ensembleDen[ii] ++;
      for (j = 1; j <= RF_sortedTimeInterestSize;  j++) {
        ensembleKHZnum[j][ii] += subjectHazard[ii][j];
        ensembleCHFnum[j][ii] += subjectNelsonAalen[ii][j];
        ensembleHRWnum[j][ii] += subjectHazardRaw[ii][j];
      }
#ifdef _OPENMP
      omp_unset_lock(&(lockDENptr[ii]));
#endif
    }
    fullFlag = FALSE;
  }  
    free_dmatrix(subjectHazardTST, 1, SG_fsubjCount, 1, RF_sortedTimeInterestSize);
    free_dmatrix(subjectNelsonAalenTST, 1, SG_fsubjCount, 1, RF_sortedTimeInterestSize);
    free_dmatrix(subjectHazardRawTST, 1, SG_fsubjCount, 1, RF_sortedTimeInterestSize);
    free_new_vvector(subjTailPtrTST, 1, SG_fsubjCount, NRUTIL_TPTR);
    free_uivector(subjMaxTimeIndxTST, 1, SG_fsubjCount);
}
void normalizeEnsembleEstimates(char mode) {
  char oobFlag, fullFlag;
  double **ensembleKHZptr;
  double **ensembleCHFptr;
  double **ensembleHRWptr;
  double **ensembleKHZnum;
  double **ensembleCHFnum;
  double **ensembleHRWnum;
  double  *ensembleDen;
  uint    *subjSlot;
  uint obsSize;
  uint i, j;
  oobFlag = fullFlag = FALSE;
  if (mode != RF_PRED) {
    obsSize = RF_subjCount;
    subjSlot = RF_subjSlot;
  }
  else {
    obsSize = SG_fsubjCount;
    subjSlot= SG_fsubjSlot;    
  }
  switch (mode) {
  case RF_PRED:
    oobFlag = FALSE;
    if (RF_opt & OPT_FENS) {
      fullFlag = TRUE;
    }
    break;
  default:
    if (RF_opt & OPT_OENS) {
        oobFlag = TRUE;
    }
    if (RF_opt & OPT_IENS) {
      fullFlag = TRUE;
    }
    break;
  }
  for (i = 1; i <= obsSize; i++) {
    SG_ensembleID_[i] = subjSlot[i];
  }
  while ((oobFlag == TRUE) || (fullFlag == TRUE)) {
    if (oobFlag == TRUE) {
      ensembleDen    = RF_oobEnsembleDen;
      ensembleKHZptr = SG_oobEnsembleKHZptr;
      ensembleCHFptr = SG_oobEnsembleCHFptr;
      ensembleHRWptr = SG_oobEnsembleHRWptr;
      ensembleKHZnum = SG_oobEnsembleKHZnum;
      ensembleCHFnum = SG_oobEnsembleCHFnum;
      ensembleHRWnum = SG_oobEnsembleHRWnum;
    }
    else {
      ensembleDen    = RF_fullEnsembleDen;
      ensembleKHZptr = SG_fullEnsembleKHZptr;
      ensembleCHFptr = SG_fullEnsembleCHFptr;
      ensembleHRWptr = SG_fullEnsembleHRWptr;
      ensembleKHZnum = SG_fullEnsembleKHZnum;
      ensembleCHFnum = SG_fullEnsembleCHFnum;
      ensembleHRWnum = SG_fullEnsembleHRWnum;
    }
    for (i = 1; i <= obsSize; i++) {
      if (ensembleDen[i] != 0) {
        for (j = 1; j <= RF_timeInterestSize; j++) {
          ensembleKHZptr[j][i] = ensembleKHZnum[j][i] / ensembleDen[i];
          ensembleCHFptr[j][i] = ensembleCHFnum[j][i] / ensembleDen[i];
          ensembleHRWptr[j][i] = ensembleHRWnum[j][i] / ensembleDen[i];
        }
      }
      else {
        for (j = 1; j <= RF_timeInterestSize; j++) {
          ensembleKHZptr[j][i] = RF_nativeNaN;
          ensembleCHFptr[j][i] = RF_nativeNaN;
          ensembleHRWptr[j][i] = RF_nativeNaN;
        }
      }
    }
    if (oobFlag == TRUE) {
      oobFlag = FALSE;
    }
    else {
      fullFlag = FALSE;
    }
  }  
}
void summarizeFaithfulBlockPerformance (char        mode,
                                        uint        b,
                                        uint        blockID,
                                        double   ***blkEnsembleCLSnum,
                                        double    **blkEnsembleRGRnum,
                                        double     *blkEnsembleDen,
                                        double    **responsePtr,
                                        double   ***perfCLSblk,
                                        double    **perfRGRblk) {
  uint      obsSize;
  obsSize = RF_subjCount;
    if (RF_rTargetFactorCount > 0) {
      getPerformance(b,
                     mode,
                     obsSize,
                     responsePtr,
                     blkEnsembleDen,
                     blkEnsembleCLSnum,
                     NULL,
                     perfCLSblk[blockID],
                     NULL);
    }
    if (RF_rTargetNonFactorCount > 0) {
      getPerformance(b,
                     mode,
                     obsSize,
                     responsePtr,
                     blkEnsembleDen,
                     NULL,
                     blkEnsembleRGRnum,
                     NULL,
                     perfRGRblk[blockID]);
    }
}
void getPerformance(uint      serialTreeID,
                    char      mode,
                    uint      obsSize,
                    double  **responsePtr,
                    double    *denomPtr,
                    double  ***outcomeCLS,
                    double   **outcomeRGR,
                    double  **perfCLSptr,
                    double   *perfRGRptr) {
  uint j, k;
    if (perfCLSptr != NULL) {
      for (j = 1; j <= RF_rTargetFactorCount; j++) {
        if (TRUE) {
          double *cpv = dvector(1, RF_rFactorSize[RF_rFactorMap[RF_rTargetFactor[j]]]);
          double *maxVote = dvector(1, obsSize);
          getMaxVote(obsSize,
                     RF_rFactorSize[RF_rFactorMap[RF_rTargetFactor[j]]],
                     outcomeCLS[j],
                     denomPtr,
                     maxVote);
          perfCLSptr[j][1] = getClassificationIndex(obsSize,
                                                    responsePtr[RF_rTargetFactor[j]],
                                                    denomPtr,
                                                    maxVote);
          getConditionalClassificationIndex(obsSize,
                                            RF_rFactorSize[RF_rFactorMap[RF_rTargetFactor[j]]],
                                            responsePtr[RF_rTargetFactor[j]],
                                            outcomeCLS[j],
                                            maxVote,
                                            denomPtr,
                                            cpv);
          for (k = 1; k <= RF_rFactorSize[RF_rFactorMap[RF_rTargetFactor[j]]]; k++) {
            perfCLSptr[j][1+k] = cpv[k];
          }
          free_dvector(cpv, 1, RF_rFactorSize[RF_rFactorMap[RF_rTargetFactor[j]]]);
          free_dvector(maxVote, 1, obsSize);
        }
      }
    }
    if (perfRGRptr != NULL) {
      for (j = 1; j <= RF_rTargetNonFactorCount; j++) {
        perfRGRptr[j] = getMeanSquareError(obsSize,
                                           responsePtr[RF_rTargetNonFactor[j]],
                                           outcomeRGR[j],
                                           denomPtr);
      }
    }
}
char getPerfFlag (char mode, uint serialTreeID) {
  char result;
  if (RF_opt & OPT_PERF) {
    result = TRUE;
  }
  else {
    result = FALSE;
  }
  if (result) {
    if (serialTreeID % RF_perfBlock == 0){
    }
    else {
      if (serialTreeID == RF_ntree) {
      }
      else {
        result = FALSE;
      }
    }
  }
  return result;
}
static double clipUnitTime(double x) {
  const double upper = 1.0 - sqrt(DBL_EPSILON);
  if (x < 0.0) {
    return 0.0;
  }
  if (x > upper) {
    return upper;
  }
  return x;
}
static uint firstTimeInterestGreater(double value) {
  int low, high, mid, result;
  low = 1;
  high = (int) RF_sortedTimeInterestSize;
  result = high + 1;
  while (low <= high) {
    mid = low + ((high - low) >> 1);
    if (RF_timeInterest[mid] > value) {
      result = mid;
      high = mid - 1;
    }
    else {
      low = mid + 1;
    }
  }
  return (uint) result;
}
static uint firstTimeInterestGreaterOrEqual(double value) {
  int low, high, mid, result;
  low = 1;
  high = (int) RF_sortedTimeInterestSize;
  result = high + 1;
  while (low <= high) {
    mid = low + ((high - low) >> 1);
    if (RF_timeInterest[mid] >= value) {
      result = mid;
      high = mid - 1;
    }
    else {
      low = mid + 1;
    }
  }
  return (uint) result;
}
static double getCaseWGridRightUsed(double right) {
  uint idxNext;
  right = clipUnitTime(right);
  idxNext = firstTimeInterestGreater(right);
  if (idxNext <= 1) {
    return 0.0;
  }
  return clipUnitTime(RF_timeInterest[idxNext - 1]);
}
static double getCaseWRightUsed(double right,
                                double rightExtended,
                                char   wMode) {
  switch (wMode) {
  case 1:
    return getCaseWGridRightUsed(right);
  case 3:
    return clipUnitTime(rightExtended);
  case 2:
  default:
    return clipUnitTime(right);
  }
}
static double getCaseWObserved(double **ensembleKHZptr,
                               uint     subjIndex,
                               double   left,
                               double   right) {
  double result;
  double uLeft, uRight, a, b;
  uint idx1, idx2, k;
  result = 0.0;
  left  = clipUnitTime(left);
  right = clipUnitTime(right);
  if (!(left < right)) {
    return result;
  }
  idx1 = firstTimeInterestGreater(left);
  if (idx1 > RF_sortedTimeInterestSize) {
    return result;
  }
  idx2 = firstTimeInterestGreaterOrEqual(right);
  if (idx2 > RF_sortedTimeInterestSize) {
    idx2 = RF_sortedTimeInterestSize;
  }
  uLeft = (idx1 > 1) ? clipUnitTime(RF_timeInterest[idx1 - 1]) : 0.0;
  for (k = idx1; k <= idx2; k++) {
    uRight = clipUnitTime(RF_timeInterest[k]); 
    a = clipUnitTime((left  > uLeft)  ? left  : uLeft);
    b = clipUnitTime((right < uRight) ? right : uRight);
    if (b > a) {
      result += ensembleKHZptr[k][subjIndex] *
                (1.0 - uRight * uRight) *
                (atanh(b) - atanh(a));
    }
    uLeft = uRight;
  }
  return result;
}
static double getCaseWGrid(double **ensembleKHZptr,
                           uint     subjIndex,
                           double   left,
                           double   right) {
  uint idxNext;
  double rightSnap;
  left  = clipUnitTime(left);
  right = clipUnitTime(right);
  if (!(left < right)) {
    return 0.0;
  }
  idxNext = firstTimeInterestGreater(right);
  if (idxNext <= 1) {
    return 0.0;
  }
  rightSnap = clipUnitTime(RF_timeInterest[idxNext - 1]);
  if (!(left < rightSnap)) {
    return 0.0;
  }
  return getCaseWObserved(ensembleKHZptr, subjIndex, left, rightSnap);
}
static char isCaseOrderedBefore(double **responseIn,
                                uint     caseA,
                                uint     caseB) {
  if (responseIn[RF_startTimeIndex][caseA] < responseIn[RF_startTimeIndex][caseB]) {
    return TRUE;
  }
  if (responseIn[RF_startTimeIndex][caseA] > responseIn[RF_startTimeIndex][caseB]) {
    return FALSE;
  }
  if (responseIn[RF_timeIndex][caseA] < responseIn[RF_timeIndex][caseB]) {
    return TRUE;
  }
  if (responseIn[RF_timeIndex][caseA] > responseIn[RF_timeIndex][caseB]) {
    return FALSE;
  }
  return (caseA < caseB);
}
static double getCaseWExtendedRight(double **responseIn,
                                    uint    *caseList,
                                    uint     caseCount,
                                    uint     caseID) {
  uint p, cand, nextCase;
  double right;
  right = responseIn[RF_timeIndex][caseID];
  nextCase = 0;
  for (p = 1; p <= caseCount; p++) {
    cand = caseList[p];
    if (cand == caseID) {
      continue;
    }
    if (isCaseOrderedBefore(responseIn, caseID, cand)) {
      if ((nextCase == 0) || isCaseOrderedBefore(responseIn, cand, nextCase)) {
        nextCase = cand;
      }
    }
  }
  if (nextCase > 0) {
    if (right < responseIn[RF_startTimeIndex][nextCase]) {
      right = responseIn[RF_startTimeIndex][nextCase];
    }
  }
  return right;
}
static double getCaseW(double **ensembleKHZptr,
                       uint     subjIndex,
                       double   left,
                       double   right,
                       double   rightExtended,
                       char     wMode) {
  switch (wMode) {
  case 1:
    return getCaseWGrid(ensembleKHZptr, subjIndex, left, right);
  case 3:
    return getCaseWObserved(ensembleKHZptr, subjIndex, left, rightExtended);
  case 2:
  default:
    return getCaseWObserved(ensembleKHZptr, subjIndex, left, right);
  }
}
void calculateRiskLegacy(char mode) {
  calculateRiskCore(mode, 1);
}
void calculateRisk(char mode) {
  calculateRiskCore(mode, 2);
}
void calculateRiskExtended(char mode) {
  calculateRiskCore(mode, 3);
}
void calculateRiskCore(char mode, char wMode) {
  double **ensembleKHZptr;
  double  *ensembleDen;
  double  *riskPtr;
  double  *wCasePtr;
  double   caseWRisk;
  double unscaledRiskPartI, unscaledRiskPartII, unscaledRiskTotal;
  double caseLeft, caseRight, caseRightExtended;
  double delta;
  uint caseID;
  char oobFlag, fullFlag;
  uint idx1, idxk;
  uint i, j, k;
  uint     subjCount;
  double **responseIn;
  uint    *subjTailCaseMap;
  uint    *timeInterestSubjTailIndex;
  uint    *subjSlotCount;
  uint   **subjList;
  uint   **timeInterestIntervalIndex;
  uint    *timeInterestIntervalCount;
  oobFlag = fullFlag = FALSE;
  switch (mode) {
  case RF_PRED:
    if (RF_opt & OPT_FENS) {
      fullFlag = TRUE;
    }
    subjCount  = SG_fsubjCount;
    responseIn = RF_fresponseIn;
    subjTailCaseMap = SG_fsubjTailCaseMap;
    timeInterestSubjTailIndex = SG_ftimeInterestSubjTailIndex;
    subjSlotCount   = SG_fsubjSlotCount;
    subjList = SG_fsubjList;
    timeInterestIntervalIndex  = SG_ftimeInterestIntervalIndex;
    timeInterestIntervalCount  = SG_ftimeInterestIntervalCount;
    break;
  default:
    if (RF_opt & OPT_OENS) {
      oobFlag = TRUE;
    }
    if (RF_opt & OPT_IENS) {
      fullFlag = TRUE;
    }
    subjCount  = RF_subjCount;
    responseIn = RF_responseIn;
    subjTailCaseMap = SG_subjTailCaseMap;
    timeInterestSubjTailIndex = SG_timeInterestSubjTailIndex;
    subjSlotCount   = RF_subjSlotCount;
    subjList = RF_subjList;
    timeInterestIntervalIndex  = SG_timeInterestIntervalIndex;    
    timeInterestIntervalCount  = SG_timeInterestIntervalCount;
    break;
  }
  for (i = 1; i <= subjCount; i++) {
    for (j = 1; j <= subjSlotCount[i]; j++) {
      caseID = subjList[i][j];
      caseLeft = responseIn[RF_startTimeIndex][caseID];
      caseRight = responseIn[RF_timeIndex][caseID];
      caseRightExtended = caseRight;
      if (wMode == 3) {
        caseRightExtended = getCaseWExtendedRight(responseIn,
                                                  subjList[i],
                                                  subjSlotCount[i],
                                                  caseID);
      }
      SG_absWCaseTimeLeft_[caseID] = clipUnitTime(caseLeft);
      SG_absWCaseTimeRight_[caseID] = getCaseWRightUsed(caseRight,
                                                        caseRightExtended,
                                                        wMode);
    }
  }
  while ((oobFlag == TRUE) || (fullFlag == TRUE)) {
    if (oobFlag == TRUE) {
      ensembleDen    = RF_oobEnsembleDen;
      ensembleKHZptr = SG_oobEnsembleKHZptr;
      riskPtr = SG_oobRisk_;
      wCasePtr = SG_oobWCase_;
    }
    else {
      ensembleDen    = RF_fullEnsembleDen;
      ensembleKHZptr = SG_fullEnsembleKHZptr;
      riskPtr = SG_fullRisk_;
      wCasePtr = SG_fullWCase_;      
    }
    unscaledRiskTotal = 0;
    for (i = 1; i <= subjCount; i++) {
      if (ensembleDen[i] > 0) {
        if (responseIn[RF_statusIndex][subjTailCaseMap[i]] == 1) {
          if (timeInterestSubjTailIndex[i] > 0) {
            if ( (timeInterestSubjTailIndex[i] == RF_sortedTimeInterestSize) ||
                 fabs(RF_timeInterest[timeInterestSubjTailIndex[i]] - responseIn[RF_timeIndex][subjTailCaseMap[i]]) < EPSILON2 ) {
              unscaledRiskPartII = ensembleKHZptr[timeInterestSubjTailIndex[i]][i];
              unscaledRiskPartII = - log(unscaledRiskPartII);
            }
            else {
              unscaledRiskPartII = ensembleKHZptr[timeInterestSubjTailIndex[i] + 1][i];
              unscaledRiskPartII = - log(unscaledRiskPartII);        
            }
          }
          else {
            unscaledRiskPartII = 0.0;
          }
        }
        else {
          unscaledRiskPartII = 0.0;
        }
        unscaledRiskPartI = 0.0;
        for (j = 1; j <= subjSlotCount[i]; j++) {
          caseID = subjList[i][j];
          caseWRisk = 0.0;
          if (timeInterestIntervalCount[caseID] > 0) {
            if (timeInterestIntervalIndex[caseID][1] <= timeInterestSubjTailIndex[i]) {
              idx1 = timeInterestIntervalIndex[caseID][1];
              delta = RF_timeInterest[ idx1 ] - responseIn[RF_startTimeIndex][caseID];
              caseWRisk += ensembleKHZptr[idx1][i] * delta;
              for (k = 2; k <= timeInterestIntervalCount[caseID]; k++) {
                idxk = timeInterestIntervalIndex[caseID][k];
                if (idxk <= timeInterestSubjTailIndex[i]) {            
                  caseWRisk += ensembleKHZptr[idxk][i] * SG_timeInterestDelta[idxk];
                }
                else {
                  break;
                }
              }
            }
          }
          unscaledRiskPartI += caseWRisk;
          caseLeft = responseIn[RF_startTimeIndex][caseID];
          caseRight = responseIn[RF_timeIndex][caseID];
          caseRightExtended = caseRight;
          if (wMode == 3) {
            caseRightExtended = getCaseWExtendedRight(responseIn,
                                                      subjList[i],
                                                      subjSlotCount[i],
                                                      caseID);
          }
          wCasePtr[caseID] = getCaseW(ensembleKHZptr,
                                      i,
                                      caseLeft,
                                      caseRight,
                                      caseRightExtended,
                                      wMode);
        }          
        unscaledRiskTotal = unscaledRiskPartI + unscaledRiskPartII;
      }
      else {
        unscaledRiskTotal = RF_nativeNaN;
        for (j = 1; j <= subjSlotCount[i]; j++) {
          caseID = subjList[i][j];
          wCasePtr[caseID] = RF_nativeNaN;
        }
      }
      riskPtr[i] = unscaledRiskTotal;
    }
    if (oobFlag == TRUE) {
      oobFlag = FALSE;
    }
    else {
      fullFlag = FALSE;
    }
  }  
}
void calculateRiskRaw(char mode) {
  double **ensembleHRWptr;
  double *ensembleDen;
  double *riskRawPtr;
  double logLkhPartI, logLkhPartII;
  double delta;
  char oobFlag, fullFlag;
  uint i, j;
  uint     subjCount;
  double **responseIn;
  uint    *subjTailCaseMap;
  oobFlag = fullFlag = FALSE;
  switch (mode) {
  case RF_PRED:
    if (RF_opt & OPT_FENS) {
      fullFlag = TRUE;
    }
    subjCount  = SG_fsubjCount;
    responseIn = RF_fresponseIn;
    subjTailCaseMap = SG_fsubjTailCaseMap;
    break;
  default:
    if (RF_opt & OPT_OENS) {
      oobFlag = TRUE;
    }
    if (RF_opt & OPT_IENS) {
      fullFlag = TRUE;
    }
    subjCount  = RF_subjCount;
    responseIn = RF_responseIn;
    subjTailCaseMap = SG_subjTailCaseMap;
    break;
  }
  while ((oobFlag == TRUE) || (fullFlag == TRUE)) {
    if (oobFlag == TRUE) {
      ensembleHRWptr = SG_oobEnsembleHRWptr;
      ensembleDen    = RF_oobEnsembleDen;
      riskRawPtr      = SG_oobRiskRaw_;
    }
    else {
      ensembleHRWptr = SG_fullEnsembleHRWptr;
      ensembleDen    = RF_fullEnsembleDen;
      riskRawPtr      = SG_fullRiskRaw_;
    }
    for (i = 1; i <= subjCount; i++) {
      logLkhPartI  = 0.0;
      logLkhPartII = 0.0;
      if (ensembleDen[i] > 0) {
        if (responseIn[RF_statusIndex][subjTailCaseMap[i]] == 1) {
          for (j = 1; j <= RF_sortedTimeInterestSize; j++) {
            if (responseIn[RF_timeIndex][subjTailCaseMap[i]] - RF_timeInterest[j] > EPSILON2 ) {          
              delta = 1.0 - ensembleHRWptr[j][i];
              if (delta > EPSILON2) {
                logLkhPartI += log (delta);
              }
              else {
                delta = 1.0 - (ensembleHRWptr[j][i] * ensembleDen[i] / (ensembleDen[i] + 1));
                logLkhPartI += log (delta);                
              }
            }
            else {
              break;
            }
          }
          if (j < RF_sortedTimeInterestSize) {
            logLkhPartII = log (ensembleHRWptr[j+1][i]);
          }
        }
        else {
          for (j = 1; j <= RF_sortedTimeInterestSize; j++) {
            delta = 1.0 - ensembleHRWptr[j][i];
            if (delta > EPSILON2) {
              logLkhPartI += log (delta);
            }
            else {
              delta = 1.0 - (ensembleHRWptr[j][i] * ensembleDen[i] / (ensembleDen[i] + 1));
              logLkhPartI += log (delta);                
            }
          }
        }
      }
      riskRawPtr[i] = logLkhPartI + logLkhPartII;
    }
    if (oobFlag == TRUE) {
      oobFlag = FALSE;
    }
    else {
      fullFlag = FALSE;
    }
  }  
}
