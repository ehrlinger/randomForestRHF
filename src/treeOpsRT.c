
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "shared/globalCore.h"
#include           "shared/externalCore.h"
#include           "global.h"
#include           "external.h"

// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "treeOpsRT.h"
#include "treeOps.h"
#include "terminal.h"
#include "node.h"
#include "stackOutput.h"
#include "rhfMain.h"
#include "termOps.h"
#include "greedyInfo.h"
#include "splitGreedy.h"
#include "splitTDC.h"
#include "descriptor.h"
#include "server.h"
#include "shared/stackMembershipVectors.h"
#include "shared/stackPreDefined.h"
#include "shared/terminalBase.h"
#include "shared/nrutil.h"
#include "shared/restoreTree.h"
#include "shared/bootstrap.h"
#include "shared/nodeBaseOps.h"
#include "shared/termBaseOps.h"
#include "shared/polarityNew.h"
#include "shared/error.h"
#include <unistd.h>
void acquireTreeRT(char mode, uint treeID) {
  Node     *root;
  NodeBase *rootBase;
  uint  nSize, xSize;
  uint *bootMembrIndx;
  uint  bootMembrSize;
  char  bootResult;
  uint  i, k;
#ifdef _OPENMP
#endif
  nSize = SG_augmObjCommon -> nSize;
  xSize = SG_augmObjCommon -> pSize;
  stackMembershipVectors(RF_subjSize,
                         &RF_bootMembershipFlag[treeID],
                         &RF_oobMembershipFlag[treeID],
                         &RF_bootMembershipCount[treeID],
                         &RF_ibgMembershipIndex[treeID],
                         &RF_oobMembershipIndex[treeID]);
  RF_nodeMembership[treeID] = (NodeBase **) new_vvector(1, nSize, NRUTIL_NPTR);
  RF_tTermMembership[treeID] = (TerminalBase **) new_vvector(1, nSize, NRUTIL_TPTR);
  RF_leafLinkedObjHead[treeID] = RF_leafLinkedObjTail[treeID] = makeLeafLinkedObj();
  root = makeNode(xSize);
  root -> nSize = nSize;
  rootBase = (NodeBase *) root;
  RF_root[treeID] = rootBase;
  rootBase -> parent = NULL;
  rootBase -> nodeID = 1;
  for (k = 1; k <= rootBase -> xSize; k++) {
    rootBase -> permissible[k] = TRUE;
  }
  rootBase -> allMembrSizeAlloc = rootBase -> allMembrSize = nSize;
  rootBase -> allMembrIndx = uivector(1, rootBase -> allMembrSizeAlloc);
  for (i = 1; i <= nSize; i++) {
    rootBase -> allMembrIndx[i] = i;
  }
  SG_bsfOrder = 0;
  if ( (RF_opt & OPT_BOOT_TYP1) || (RF_opt & OPT_BOOT_TYP2)) {
    bootMembrSize = RF_bootstrapSize;
  }
  else {
    bootMembrSize = nSize;
  }
  bootMembrIndx  = uivector(1, bootMembrSize);
  bootResult = bootstrap (mode,
                          treeID,
                          rootBase,  
                          rootBase -> allMembrIndx,
                          rootBase -> allMembrSize,
                          bootMembrSize,
                          RF_bootstrapIn,
                          RF_subjSize,
                          RF_subjWeight,
                          RF_subjWeightType,
                          RF_subjWeightSorted,
                          RF_subjWeightDensitySize,
                          nSize,
                          bootMembrIndx,
                          RF_bootMembershipFlag,
                          RF_oobMembershipFlag,
                          RF_bootMembershipCount,
                          RF_oobSize,
                          RF_ibgSize,
                          RF_ibgMembershipIndex,
                          RF_oobMembershipIndex,
                          SG_BOOT_CT_ptr);
  if (bootResult) {
      rootBase -> repMembrIndx = bootMembrIndx;
      rootBase -> repMembrSize = rootBase -> repMembrSizeAlloc = bootMembrSize;
      root -> augm = getAugmentationObjGeneric(SG_augmObjCommon, treeID, root);
      restoreTreeLOT(treeID, root, SG_offsetTree);
  }
  else {
  }
  if (bootResult) {
      RF_nodeCount[treeID]  = (RF_tLeafCount[treeID] << 1) - 1;
  }
  else {
    RF_nodeCount[treeID] = 1;
    RF_tLeafCount[treeID] = 1;
  }
  if (bootResult) {
    rootBase -> repMembrIndx = NULL;
    rootBase -> repMembrSize = rootBase -> repMembrSizeAlloc = 0;
  }
  free_uivector(bootMembrIndx, 1, bootMembrSize);
  free_new_vvector(RF_nodeMembership[treeID], 1, nSize, NRUTIL_NPTR);
}
void predictForestRT(DescriptorObj *headDO) {
  DescriptorObj *currDO;
  uint bb;
  char alive;
  alive = TRUE;
  while (alive) {
#ifdef _OPENMP
    omp_set_lock(&SG_lockDO);
#endif
    currDO = headDO -> fwdLink;
    while (currDO != NULL) {
      if (currDO -> userState  == SG_DESC_POSTREAD) {
        break;
      }
      currDO = currDO -> fwdLink;
    }
#ifdef _OPENMP
    omp_unset_lock(&SG_lockDO);
#endif
    if (currDO != NULL) {
      RF_fobservationSize = currDO -> nSize;
      RF_fobservationIn = currDO -> fxArrayIn;
      SG_fobservationInAugm = currDO -> fxArrayInAugm;
      getAugmentationObjCommonGenericTest(SG_augmObjCommon,
                                          RF_fobservationSize,
                                          RF_fobservationIn,
                                          NULL);  
      currDO -> userState = SG_DESC_PREDICTING;
      stackPreDefinedPredictArrays(RF_ntree,
                                   RF_fobservationSize,
                                   & RF_fidentityMembershipIndexSize,
                                   & RF_fidentityMembershipIndex,
                                   & RF_fnodeMembership,
                                   &RF_ftTermMembership);
      RF_fullEnsembleDen = dvector(1, RF_fobservationSize);
      for (uint i = 1; i <= RF_fobservationSize; i++) {
        RF_fullEnsembleDen[i] = 0.0;
      }
      SG_fullEnsembleKHZnum = (double **) new_vvector(1, RF_sortedTimeInterestSize, NRUTIL_DPTR);
      for (uint j = 1; j <= RF_sortedTimeInterestSize; j++) {
        SG_fullEnsembleKHZnum[j] = dvector(1, RF_subjCount);
        for (uint i = 1; i <= RF_subjCount; i++) {
          SG_fullEnsembleKHZnum[j][i] = 0.0;
        }
      }
#ifdef _OPENMP
#pragma omp parallel for num_threads(RF_numThreads - 2)
#endif
      for (bb = 1; bb <= RF_getTreeCount; bb++) {
        predictTreeRT(RF_getTreeIndex[bb]);
      }
      for (uint i = 1; i <= currDO -> nSize; i++) {
      }
      free_dvector(RF_fullEnsembleDen, 1, RF_fobservationSize);
      for (uint j = 1; j <= RF_sortedTimeInterestSize; j++) {
        free_dvector(SG_fullEnsembleKHZnum[j], 1, RF_subjCount);
      }
      free_new_vvector(SG_fullEnsembleKHZnum, 1, RF_sortedTimeInterestSize, NRUTIL_DPTR);
      freeAugmentationObjCommonGenericTestOnly(SG_augmObjCommon);
      unstackPreDefinedPredictArrays(RF_ntree,
                                     RF_fobservationSize,
                                     RF_fidentityMembershipIndexSize,
                                     RF_fidentityMembershipIndex,
                                     RF_fnodeMembership,
                                     RF_ftTermMembership);
      if (currDO -> userState == SG_DESC_PREDICTING) {
        currDO -> userState = SG_DESC_POSTPREDICTING;
      }
      else {
      }
#ifdef _OPENMP      
#else
      alive = FALSE;
#endif
    }
    else {
#ifdef _OPENMP
    omp_set_lock(&SG_lockDO);
#endif
      if (headDO -> userState == SG_DESC_CLOSED) {
        alive = FALSE;
      }
#ifdef _OPENMP
      omp_unset_lock(&SG_lockDO);
#endif
    }
  }
}
void predictTreeRT(uint treeID) {
  Node     *root;
  root = (Node *) RF_root[treeID];
  root -> tstMembrSize = root -> tstMembrSizeAlloc = RF_fobservationSize;
  root -> tstMembrIndx = RF_fidentityMembershipIndex;
  getTestMembershipLOT(treeID, root);
  root -> tstMembrIndx = NULL;
  root -> tstMembrSize = root -> tstMembrSizeAlloc = 0;
  freeTestMembership(treeID, (NodeBase *) root);
}
void freeTestMembership(uint treeID, NodeBase *parent) {
  if (parent != NULL) {
    if ((parent -> left != NULL) && (parent -> right != NULL)) {
      freeTestMembership(treeID, ((NodeBase *) parent) -> left);
      freeTestMembership(treeID, ((NodeBase *) parent) -> right);
    }
    if (((Node*) parent) -> tstMembrSizeAlloc > 0) {
      if (((Node*) parent) -> tstMembrIndx != NULL) {
        free_uivector(((Node*) parent) -> tstMembrIndx, 1, ((Node*) parent) -> tstMembrSizeAlloc);
        ((Node*) parent) -> tstMembrIndx = NULL;
      }
      ((Node*) parent) -> tstMembrSizeAlloc = 0;
    }
  }
}
