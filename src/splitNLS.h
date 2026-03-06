#ifndef RF_SPLIT_NLS_H
#define RF_SPLIT_NLS_H
#include "greedyInfo.h"
char virtuallySplitNodeNLS (uint       treeID,
                            GreedyObj *greedyMembr,
                            char   *localSplitIndicator,
                            uint   *localSplitIndx,
                            double *localSplitValue,
                            uint   *leftSize);
void initializeRiskRaw(uint treeID, Node *parent);
void saveRiskRaw(uint treeID, Node *parent, double phi);
void stackAndGetSplitSurv(uint      treeID,
                          NodeBase *parent,
                          uint  **eventTimeCount,
                          uint  **eventTimeIndex,
                          uint   *eventTimeSize,
                          uint  **parentEvent,
                          uint  **parentAtRisk,
                          uint  **leftEvent,
                          uint  **leftAtRisk,
                          uint  **rightEvent,
                          uint  **rightAtRisk);
void unstackSplitSurv(uint      treeID,
                      NodeBase *parent,
                      uint *eventTimeCount,
                      uint *eventTimeIndex,
                      uint  eventTimeSize,
                      uint *parentEvent,
                      uint *parentAtRisk,
                      uint *leftEvent,
                      uint *leftAtRisk,
                      uint *rightEvent,
                      uint *rightAtRisk);
uint getEventTime(uint      treeID,
                  NodeBase *parent,
                  uint   *repMembrIndx,
                  uint    repMembrSize,
                  uint   *eventTimeCount,
                  uint   *eventTimeIndex);
void stackSplitEventAndRisk(uint      treeID,
                            NodeBase *parent,
                            uint    genEventTimeSize,
                            uint  **genParentEvent,
                            uint  **genParentAtRisk,
                            uint  **genLeftEvent,
                            uint  **genLeftAtRisk,
                            uint  **genRightEvent,
                            uint  **genRightAtRisk);
void unstackSplitEventAndRisk(uint      treeID,
                              NodeBase *parent,
                              uint    genEventTimeSize,
                              uint   *genParentEvent,
                              uint   *genParentAtRisk,
                              uint   *genLeftEvent,
                              uint   *genLeftAtRisk,
                              uint   *genRightEvent,
                              uint   *genRightAtRisk);
void getSplitEventAndRisk(uint      treeID,
                          NodeBase *parent,
                          uint   *repMembrIndx,
                          uint    repMembrSize,
                          uint   *eventTimeCount,
                          uint   *eventTimeIndex,
                          uint    eventTimeSize,
                          uint   *parentEvent,
                          uint   *parentAtRisk);
double getLogLikelihood(uint size, uint *event, uint *atRisk);
#endif
