#ifndef RF_SPLIT_TDC_H
#define RF_SPLIT_TDC_H
#include "greedyInfo.h"
#include "shared/sampling.h"
#include "shared/splitInfo.h"
char virtuallySplitNodeTDC (uint       treeID,
                            GreedyObj *greedyMembr,
                            char   *localSplitIndicator,
                            uint   *localSplitIndx,
                            double *localSplitValue,
                            uint   *leftSize);
void initializeRisk(uint treeID, Node *parent);
void saveRisk(uint treeID, Node *parent, double v0, double u0, double c0, double rn0);
#endif
