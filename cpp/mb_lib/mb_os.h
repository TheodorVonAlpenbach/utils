#ifndef _MB_OS_H_
#define _MB_OS_H_

#include <vector>
#include <list>
#include <map>
#include <iostream>
#include <fstream>
#include <sstream>

namespace mb {
  template<class In, class Key>
  inline std::ostream& print(In first, In last, std::ostream& os, const char*
			     delim, const char* prefix, const char*
			     suffix, const Key& key)
  {
    os << prefix;
    if (first == last)
      return os << suffix;
    os << key(*first);
    while (++first != last) {os << delim << key(*first);}
    os << suffix;
    return os;
  }

  template<class In>
  inline std::ostream& print(In first,
			     In last,
			     std::ostream& os = std::cout,
			     const char* delim = "",
			     const char* prefix = "",
			     const char* suffix = "")
  {
    os << prefix;
    if (first == last)
      return os << suffix;
    os << (*first);
    while (++first != last) {os << delim << (*first);}
    os << suffix;
    return os;
  }

  template<class C, class Key>
  inline std::ostream& print_in(const C& c,
				std::ostream& os,
				const char* delim,
				const char* prefix,
				const char* suffix,
				const Key& key)
  {return print(c.begin(), c.end(), os, delim, prefix, suffix, key);}

  template<class C>
  inline std::ostream& print_in(const C& c,
				std::ostream& os = std::cout,
				const char* delim = "",
				const char* prefix = "",
				const char*
				suffix = "")
  {return print(c.begin(), c.end(), os, delim, prefix, suffix);}

  template<class In>
  inline std::ostream& print_p(In first, In last, std::ostream& os = std::cout,   // CHANGED, std::cout to std::cout
			       const char* delim = "",
			       const char* prefix = "",
			       const char* suffix = "")
  {
    os << prefix;
    if (first == last)
      return os << suffix;
    os << (**first);
    while (++first != last) {os << delim << (**first);}
    os << suffix;
    return os;
  }

  template<class C>
  inline std::ostream& print_p_in(const C& c, std::ostream& os = std::cout,  // CHANGED, std::cout to std::cout
				  const char* delim = "",
				  const char* prefix = "",
				  const char* suffix = "")
  {return print_p(c.begin(), c.end(), os, delim, prefix, suffix);}

  // Uses operator<< to flush object to string
  template <class T>
  inline std::string printString(const T& x) {
    std::ostringstream ost;
    ost << x;
    return ost.str();
  }

  // Reads IS into a single std::string
  inline void printFile(std::string content, std::string filename) {
    // Writes CONTENT to new file with FILENAME. If latter file
    // exists, it is overwritten.
    std::ofstream os(filename.c_str());
    os << content;
    os.flush();
  }

}// mb

template <class T, class S>
inline std::ostream& operator<< (std::ostream& os, const std::pair<T,S>& p) { 
  return os << "(" << p.first << " " << p.second << ")";
}

template <class T, class S>
inline std::ostream& operator<< (std::ostream& os, const std::map<T,S>& m) { 
  return mb::print_in(m, os, " ", "(", ")"); 
}

template <class T>
inline std::ostream& operator<< (std::ostream& os, const std::list<T>& l) { 
  return mb::print_in(l, os, " ", "(", ")"); 
}

template <class T>
inline std::ostream& operator<< (std::ostream& os, const std::vector<T>& l) { 
  return mb::print_in(l, os, " ", "#[", "]"); 
}

#endif //ifndef _MB_OS_H_
