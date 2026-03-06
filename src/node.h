#ifndef RF_NODE_H
#define RF_NODE_H
#include "shared/nodeBase.h"
typedef struct node Node;
struct node {
  struct nodeBase base;
  struct augmentationObj *augm;
  unsigned int nSize;
  double eRiskCart;
  double eRiskCartOOB;
  double eRiskRaw;
  double eRiskRawOOB;
  double v0, u0, c0, rn0;
  double phi;
  double mean;
  char outcome;
  struct splitInfoDerived *splitInfoDerived;
  unsigned int  tstMembrSizeAlloc;
  unsigned int  tstMembrSize;
  unsigned int *tstMembrIndx;
};
#endif
