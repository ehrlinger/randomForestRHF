#ifndef RF_TREE_OPS_RT_H
#define RF_TREE_OPS_RT_H
#include "node.h"
#include "greedyInfo.h"
void acquireTreeRT (char mode, uint treeID);
void predictForestRT(DescriptorObj *headDO);
void predictTreeRT(uint treeID);
void freeTestMembership(uint treeID, NodeBase *parent);
#endif
