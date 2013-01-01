#include <ept/progresscallback.h>

namespace ept {

   bool ProgressCallback::Pulse(pkgAcquire *Owner)
   {
      pkgAcquireStatus::Pulse(Owner);
      UpdatePulse(FetchedBytes, CurrentCPS, CurrentItems);
      return true;
   }

}
