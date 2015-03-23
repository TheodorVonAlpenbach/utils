#ifndef MB_MINMAX_H
#define MB_MINMAX_H

#ifdef _MSC_VER

#undef min
#undef max

// mb 2002-03-26: What's the point of this file? Why not include this
// in mb_math?

namespace mb {
  template<class T> T min(T m1, T m2) { return (m1 < m2) ? m1 : m2; }
  template<class T> T min(T m1, T m2, T m3) { return min(min(m1, m2), m3); }
  template<class T> T max(T m1, T m2) { return (m1 < m2) ? m2 : m1; }
} // End of namespace std

#endif // _MSC_VER

#endif
