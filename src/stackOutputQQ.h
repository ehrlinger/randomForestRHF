#ifndef RF_STACK_OUTPUT_QQ_H
#define RF_STACK_OUTPUT_QQ_H
void stackTNQualitativeObjects(char     mode,
                               uint   **pSG_rmbrTNodeCT_,
                               uint   **pSG_imbrTNodeCT_,
                               uint   **pSG_ombrTNodeCT_,
                               uint   ***pSG_rmbrTNodeCT_ptr,
                               uint   ***pSG_imbrTNodeCT_ptr,
                               uint   ***pSG_ombrTNodeCT_ptr,
                               uint   **pSG_rmbrTNodeID_,
                               uint   **pSG_imbrTNodeID_,
                               uint   **pSG_ombrTNodeID_);
void stackTNQualitativeObjectsForestPtr(char mode,
                                        uint *sexp_rmbrTNodeID_,
                                        uint *sexp_imbrTNodeID_,
                                        uint *sexp_ombrTNodeID_,
                                        uint ****pSG_rmbrTNodeID_ptr,
                                        uint ****pSG_imbrTNodeID_ptr,
                                        uint ****pSG_ombrTNodeID_ptr);
void writeTNQualitativeObjectsOutput(char mode,
                                     uint  **rmbrTNodeCT,
                                     uint  **imbrTNodeCT,
                                     uint  **ombrTNodeCT,
                                     uint ***rmbrTNodeID,
                                     uint ***imbrTNodeID,
                                     uint ***ombrTNodeID);
void unstackTNQualitativeObjectsForestPtr(char mode,
                                          uint ***rmbrTNodeID_ptr,
                                          uint ***imbrTNodeID_ptr,
                                          uint ***ombrTNodeID_ptr);
void stackTNQuantitativeObjects(char mode,
                                double **pSG_termNelsonAalen_,
                                double **pSG_termHazard_,
                                double ****pSG_termNelsonAalen_ptr,
                                double ****pSG_termHazard_ptr);
void writeTNQuantitativeObjectsOutput(char mode,
                                      double ***termNelsonAalenPtr,
                                      double ***termHazardPtr);
void restoreTNQualitativeObjectsInput(char mode,
                                      uint treeID,
                                      uint  **rmbrTNodeCT,
                                      uint  **imbrTNodeCT,
                                      uint  **ombrTNodeCT,
                                      uint ***rmbrTNodeID,
                                      uint ***imbrTNodeID,
                                      uint ***ombrTNodeID);
#endif
