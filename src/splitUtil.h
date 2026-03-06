#ifndef RF_SPLIT_UTIL_H
#define RF_SPLIT_UTIL_H
#include "shared/splitInfo.h"
#include "shared/sampling.h"
char getPreSplitResult (uint treeID, NodeBase *parent, uint nodeSize, double **response);
char getVarianceSinglePass(uint    repMembrSize,
                           uint   *repMembrIndx,
                           double *targetResponse,
                           double *mean,
                           double *variance);
void getRandomPair(uint treeID, uint relativeFactorSize, uint absoluteFactorSize, double *absoluteLevel, uint *result);
void createRandomBinaryPair(uint    treeID,
                            uint    relativeFactorSize,
                            uint    absoluteFactorSize,
                            uint    groupSize,
                            double *absolutelevel,
                            uint   *pair);
void convertRelToAbsBinaryPair(uint    treeID,
                               uint    relativeFactorSize,
                               uint    absoluteFactorSize,
                               uint    relativePair,
                               double *absoluteLevel,
                               uint   *pair);
DistributionObj *stackRandomCovariatesDefault(uint treeID, NodeBase *parent, uint mtry);
void unstackRandomCovariatesDefault(uint treeID, DistributionObj *obj);
DistributionObj *stackRandomCovariatesSimple(uint treeID, NodeBase *parent, uint mtry);
void unstackRandomCovariatesSimple(uint treeID, DistributionObj *obj);
char selectRandomCovariatesDefault(uint      treeID,
                                   NodeBase *parent,
                                   DistributionObj *obj,
                                   uint      mtry,
                                   char     *factorFlag,
                                   uint     *xvarID,
                                   uint     *xvarCount);
char selectRandomCovariatesSimple(uint      treeID,
                                  NodeBase *parent,
                                  DistributionObj *obj,
                                  uint      mtry,
                                  char     *factorFlag,
                                  uint     *xvarID,
                                  uint     *xvarCount);
uint stackAndConstructSplitVectorGenericPhase1 (uint       treeID,
                                                NodeBase  *parent,
                                                uint       covariate,
                                                double    *observation, ...);
uint stackAndConstructSplitVectorGenericPhase2 (uint         treeID,
                                                NodeBase    *parent,
                                                uint         covariate,
                                                double      *splitVector,
                                                uint         vectorSize,
                                                char         factorFlag,
                                                char        *deterministicSplitFlag,
                                                uint        *mwcpSizeAbsolute,
                                                void       **splitVectorPtr);
void unstackSplitVectorGeneric(uint       treeID,
                               NodeBase  *parent,
                               uint       splitLength,
                               char       factorFlag,
                               uint       splitVectorSize,
                               uint       mwcpSizeAbsolute,
                               char       deterministicSplitFlag,
                               void      *splitVectorPtr,
                               uint      *indxx);
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
                        uint *currentMembrIter);
char updateMaximumSplitCart(uint    treeID,
                            Node   *parent,
                            double  delta,
                            uint    covariate,
                            uint    index,
                            char    factorFlag,
                            uint    mwcpSizeAbsolute,
                            void   *splitVectorPtr,
                            SplitInfoMax *infoMax);
#endif
