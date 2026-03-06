#ifndef  RF_NRUTIL_DERIVED_H
#define  RF_NRUTIL_DERIVED_H
enum alloc_typeDerived{
  NRUTIL_FOLDPTR,
  NRUTIL_CDLIPTR,
};
void *new_vvectorDerived(unsigned long long nl, unsigned long long nh, enum alloc_typeDerived type);
void free_new_vvectorDerived(void *v, unsigned long long nl, unsigned long long nh, enum alloc_typeDerived type);
#endif
