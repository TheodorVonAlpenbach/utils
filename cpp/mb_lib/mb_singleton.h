#ifndef _SINGLETON_H
#define _SINGLETON_H

namespace mb {
  // Implements a double guard pattern inside a singleton, aka the
  // double checked pattern. Ref
  // http://www.cs.wustl.edu/~schmidt/editorial-3.html

  template<class T, class BlockMutex = block_mutex>
  class dc_singleton {
  public:
    static T *instance (void) {
      // Perform the Double-Checked Locking to
      // ensure proper initialization.
      if (instance_ == 0) {
	BlockMutex lock;
	if (instance_ == 0)
	  instance_ = new T;
      }
      return instance_;
    }

  protected:
    static TYPE *instance_ = 0;
  }; // dc_singleton

} // mb

#endif //_SINGLETON_H

