#ifndef _FI_MSG_H
#define _FI_MSG_H

#include <iostream>
#include <fstream>
#include <string> // needed?

/*#ifdef __BORLANDC__
extern std::ofstream eos; // must be defined in some borland specific c-file
#else
#define eos cerr
#endif*/

#define endl std::endl
#define eos std::cerr

namespace mb
{
  void Error   (const char*, const char* = "Not implemented");
  void Warning (const char*, const char* = "Not implemented");
}

//----------------------------------------------------------------------------
inline void
mb::Error(const char* loc, const char* msg)
//----------------------------------------------------------------------------
{
  eos << "\nError in method " << loc << ":\n" << msg << endl;
}

//----------------------------------------------------------------------------
inline void
mb::Warning(const char* loc, const char* msg)
//----------------------------------------------------------------------------
{
  eos << "\nWarning from method " << loc << ":\n" << msg << endl;
}

#endif //_FI_MSG_H
