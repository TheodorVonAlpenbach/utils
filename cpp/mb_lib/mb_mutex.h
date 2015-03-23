#ifndef _MUTEX_H
#define _MUTEX_H

namespace mb {

#ifdef WIN32
  // MCF dependent mutex implementation. Actually a wrapper class for
  // the CRITICAL_SECTION family of win32.
  class mutex {
  public:
    mutex() : cs_() {
      InitializeCriticalSection(&cs_); 
    }
    virtual ~mutex() { DeleteCriticalSection(&cs_); }

    void lock() { EnterCriticalSection(&cs_); }
    void unlock() { LeaveCriticalSection(&cs_); }

  private:
    CRITICAL_SECTION cs_;
  };
#endif // WIN32

  // Apply the sentry notion from Stroustrup 21.3.8 to the mutex
  // class. When applied to a code block, ie. an instance of the class
  // is initiallized, the rest of the block is protected by the
  // underlying mutex class until the block ends, ie. the mutex class
  // instance goes out of scope. Note you must provide a mb::mutex
  // reference in the constructor.
  class block_mutex {
    mutex& m_; // Only allocated/freed once
  public:
    block_mutex(mutex& m) : m_(m) { m_.lock(); }
    virtual ~block_mutex() { m_.unlock(); }
  }; // block_mutex
  
} // mb

// Written by Mats Bergstrøm, 2002-04-04

#endif //_MUTEX_H


