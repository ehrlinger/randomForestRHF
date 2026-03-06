
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "shared/globalCore.h"
#include           "shared/externalCore.h"
#include           "global.h"
#include           "external.h"

// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "nrutilDerived.h"
#include "shared/nrutil.h"
#include "shared/error.h"
struct fold;
void *new_vvectorDerived(unsigned long long nl, unsigned long long nh, enum alloc_typeDerived type) {
  void *v;
  v = NULL;  
  switch(type) {
  case NRUTIL_FOLDPTR:
    v = (struct fold**) gvector(nl, nh, sizeof(struct fold*)) -nl+NR_END;
    break;
  case NRUTIL_CDLIPTR:    
    v = (struct cdlInfo**) gvector(nl, nh, sizeof(struct cdlInfo*)) -nl+NR_END;
    break;
  default:
    v = NULL;
    nrerror("\n  Illegal case in new_vvectorDerived().");
    break;
  }
  return v;
}
void free_new_vvectorDerived(void *v, unsigned long long nl, unsigned long long nh, enum alloc_typeDerived type) {
  switch(type) {
  case NRUTIL_FOLDPTR:
    free_gvector((struct fold**) v +nl-NR_END, nl, nh, sizeof(struct fold*));
    break;
  case NRUTIL_CDLIPTR:
    free_gvector((struct cdlInfo**) v +nl-NR_END, nl, nh, sizeof(struct cdlInfo*));
    break;
  default:
    nrerror("\n  Illegal case in free_new_vvector().");
    break;
  }
}
