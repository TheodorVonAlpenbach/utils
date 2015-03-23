#ifndef mb_int64_H
#define mb_int64_H

// Define __int64 on GNU

#if defined(__GNUG__)

typedef long long __int64;

#endif

#ifdef _MSC_VER

#define atoi64 _atoi64

#endif // _MSC_VER

#ifdef  __BORLANDC__

#include <sysutils.hpp>
#define atoi64 StrToInt64

#endif // __BORLANDC__

#endif // End of mb_int64_H
