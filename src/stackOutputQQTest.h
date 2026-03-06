#ifndef RF_STACK_OUTPUT_QQ_TEST_H
#define RF_STACK_OUTPUT_QQ_TEST_H
void stackTNQualitativeObjectsTest(char      mode,
                                   uint    **pSG_ombrTNodeCT_,
                                   uint   ***pSG_ombrTNodeCT_ptr,
                                   uint    **pSG_ombrTNodeID_);
void stackTNQualitativeObjectsForestPtrTest(char     mode,
                                            uint    *sexp_ombrTNodeID_,
                                            uint ****pSG_ombrTNodeID_ptr);
void writeTNQualitativeObjectsOutputTest(char mode,
                                         uint  **ombrTNodeCT,
                                         uint ***ombrTNodeID);
void unstackTNQualitativeObjectsForestPtrTest(char mode,
                                              uint ***ombrTNodeID_ptr);
#endif
