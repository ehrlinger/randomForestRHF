#ifndef RF_TREE_OPS_H
#define RF_TREE_OPS_H
#include "node.h"
#include "greedyInfo.h"
void acquireTree (char mode, uint treeID);
void freeTree (uint treeID, NodeBase *parent);
char growTreeLOT (uint treeID, Node *root);
char forkAndUpdate (uint treeID, GreedyObj *greedyMembr, uint *leafCount, NodeBase **nodeMembership);
void saveTree(uint treeID, NodeBase *parent, ulong *offset, ulong *offsetCT, ulong *offsetID_rmbr, ulong *offsetID_imbr, ulong *offsetID_ombr);
void restoreTreeLOT(uint treeID, Node *parent, ulong *offsetTree);
void restoreNodeMembershipLOT(uint treeID, LeafLinkedObj *head);
void getTestMembershipLOT(uint treeID, Node *parent);
#endif
