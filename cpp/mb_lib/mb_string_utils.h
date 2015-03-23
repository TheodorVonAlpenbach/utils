#ifndef _MB_STRING_UTILS
#define _MB_STRING_UTILS

#include <limits>
#include <string>
#include <list>
#include <map>
#include <algorithm>
#include <functional>
#include <sstream>
#include <vector>
#include <fstream>
#include <cstdarg>
#include <cstdio>
#include <iostream>
#include <mb_minmax.h>

namespace mb {
  // Like vprintf, printf, but result is returned as a std::string
  static const std::string whitespaces = " \t\n";

  std::string vstrprintf (std::string fmt, va_list argp);
  std::string strprintf  (std::string fmt, ...);
  void        errprintf  (std::string fmt, ...);

  // Manipulation
  std::string singleQuote(const std::string&); // s -> 's'
  std::string padString(const std::string& s, const int length, const char c = ' ');
  std::vector<std::string> split_string(const std::string& s, const std::string& del = whitespaces);
  std::string trim_string(const std::string& s, const std::string& del = whitespaces);

  // Reads S with '<<' into a std::vector of strings.
  std::vector<std::string> readString(const std::string& s);

  // Functors
  class CharMap;
  struct AppendMappedCharTo;
  class StringMap;

  //// Inlines
  // Conversion
  inline std::string int2string    (const int i, std::string fmt = "%d")    { return strprintf(fmt, i); }
  inline std::string double2string (const double d, std::string fmt = "%f") { return strprintf(fmt, d); }
  inline int    string2int    (const std::string& s)                   { return atoi(s.c_str()); }
  inline double string2double (const std::string& s)                   { return atof(s.c_str()); }

  // Count words in S
  inline int countWords(const std::string& s) {return readString(s).size();}


  //// Implementations
  inline std::string padString(const std::string& s, const int length, const char c) 
  { return s + std::string(max<int>(length - s.size(), 1), c); }

  inline std::vector<std::string> readString(const std::string& s) {
    std::istringstream ist(s.c_str());
    std::string word;
    std::vector<std::string> words;
    while (ist >> word) { words.push_back(word); }
    return words;
  }

  template<class In> // In is an iterator of std::string
  inline std::string concat(In first, In last, const std::string& delim = "",
			    const std::string& prefix = "",
			    const std::string& suffix = "") {
    // see mb::print(...) for a model
    std::string s = prefix;
    if (first == last--)
      return s + suffix;
    while (first != last)
      {s += *first + delim; ++first;}
    s += *first + suffix;
    return s;
  }

  template<class C> // C is container<(const) std::string>
  inline std::string concat(const C& c, const std::string& delim = "",
			    const std::string& prefix = "",
			    const std::string& suffix = "")
  {return concat(c.begin(), c.end(), delim, prefix, suffix);}

  inline std::string trim_string(const std::string& s, const std::string& del) {
    int pos = s.find_first_not_of(del);
    int n = s.find_last_not_of(del);
    return s.substr(pos, n+1);
  }

  // Next three functions are provided by lbs
  inline char first_non_blank_string(std::string s) {
    int n = s.find_first_not_of(whitespaces);
    if (n == std::string::npos)
      return '\0';
    return s[n];
  }

  inline std::vector<std::string>& split_string(const std::string &s, char delim, std::vector<std::string> &elems) {
    std::stringstream ss(s);
    std::string item;
    while (std::getline(ss, item, delim)) {
        elems.push_back(item);
    }
    return elems;
  }

  inline std::vector<std::string> split_string(const std::string &s, char delim) {
    std::vector<std::string> elems;
    split_string(s, delim, elems);
    return elems;
  }  

  // inline int count_matches_string(const std::string& s, const std::string& del) {
  //   int c = 0;
  //   for (std::string::iterator i = const_cast<std::string::iterator>(del.begin()); i != del.end(); ++i)
  //     c += std::count(s.begin(), s.end(), *i);
  //   return c;
  // }

  inline std::string singleQuote(const std::string& s)
  { // puts single quotes around argument std::string
    std::string ret_s = s + "'"; // s -> s'
    return ret_s.insert(0, "'"); // s' -> 's'
  }

  inline std::string vstrprintf(std::string fmt, va_list argp) {
    const int printf_bufsize_ = 5000;
    char buf[printf_bufsize_];
    vsprintf(buf, fmt.c_str(), argp);
    return buf;
  }

  inline std::string strprintf(std::string fmt, ...) {
    va_list argp;
    va_start(argp, fmt);
    std::string res = vstrprintf(fmt, argp);
    va_end(argp);
    return res;
  }

  inline void errprintf(std::string fmt, ...) {
    va_list argp;
    va_start(argp, fmt);
    std::cerr << vstrprintf(fmt, argp);
    va_end(argp);
  }

  class CharMap : std::unary_function<char, std::string> {
    typedef std::map<char, std::string> char_map;
    const char_map& m_;
  public:
    CharMap(const char_map& m) : m_(m) {}
    std::string operator()(const char c) const {
      char_map::const_iterator ci = m_.find(c);
      return (ci == m_.end()) ? std::string(1, c) : ci->second;
    }
  };

  struct AppendMappedCharTo : std::unary_function<std::string, void> {
    std::string& s_;
    const CharMap& cm_;
    AppendMappedCharTo(std::string& s, const CharMap& cm) : s_(s), cm_(cm) {}
    void operator()(const char c) const {s_ += cm_(c);}
  };

  class StringMap : std::unary_function<std::string, std::string> {
    CharMap cm_;
  public:
    StringMap(const std::map<char, std::string>& m) : cm_(m) {}
    std::string operator()(const std::string& s) const {
      std::string rs;
      std::for_each(s.begin(), s.end(), AppendMappedCharTo(rs, cm_));
      return rs;
    }
  };

} //mb

#endif //define _MB_STRING_UTILS
