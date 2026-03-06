#ifndef RF_STACK_OUTPUT_H
#define RF_STACK_OUTPUT_H
void stackDefinedOutputObjects(char mode);
void unstackDefinedOutputObjects(char mode);
void stackForestObjectsPtrOnly(char mode);
void stackTreeObjectsPtrOnly(char mode, uint treeID);
void unstackTreeObjectsPtrOnly(uint treeID);
void unstackForestObjectsPtrOnly(char mode);
void stackForestObjectsOutput(char mode);
void writeForestObjectsOutput(char mode);
void stackForestObjectsAuxOnlySGT(void);
void unstackForestObjectsAuxOnlySGT(void);
#endif
