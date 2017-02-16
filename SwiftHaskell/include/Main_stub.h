#include "HsFFI.h"
#ifdef __cplusplus
extern "C" {
#endif
extern HsInt32 square(HsInt32 a1);
extern HsWord64 countBytes(HsWord8 a1, HsPtr a2, HsWord64 a3);
extern HsPtr getSequence(HsPtr a1);
extern void callbackExample(HsFunPtr a1);
extern void contextCallbackExample(HsPtr a1, HsFunPtr a2, HsFunPtr a3);
extern HsFunPtr makeMultiplier(HsInt32 a1);
extern void freeMultiplier(HsFunPtr a1);
extern HsInt32 Main_d9Dt(StgStablePtr the_stableptr, HsInt32 a1);
#ifdef __cplusplus
}
#endif

