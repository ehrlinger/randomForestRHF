#ifndef RF_GREEDY_INFO_H
#define RF_GREEDY_INFO_H
#include "node.h"
#include "splitInfoDerived.h"
typedef struct greedyObj GreedyObj;
struct greedyObj {
  Node *parent;
  Node *leftCart;
  Node *rightCart;
  GreedyObj *fwdLink;
  GreedyObj *bakLink;
  GreedyObj *head;
  SplitInfoDerived *splitInfoDerivedCart;
  char leafFlag;
  double eRiskLossCart;
  double eRiskLossRaw;
  char bestSplitType;  
};
typedef struct lotObj LatOptTreeObj;
struct lotObj {
  uint firstIn;
  uint lastIn;
  uint size;
  uint strikeout;
  uint lag;
  double *risk;
  uint riskSize;
  double firstOD;
  uint treeSize;
};
#endif
