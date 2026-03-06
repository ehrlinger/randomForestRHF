#ifndef RF_GREEDY_OPS_H
#define RF_GREEDY_OPS_H
#include "greedyInfo.h"
GreedyObj *makeGreedyObj(Node *parent, GreedyObj *head);
void freeGreedyObj(GreedyObj *gObj);
void freeGreedyObjList(GreedyObj *gObj);
LatOptTreeObj *makeLatOptTreeObj(uint lag);
void freeLatOptTreeObj(LatOptTreeObj *lotObj);
void insertRisk(uint treeID, LatOptTreeObj *obj, double value);
#endif
