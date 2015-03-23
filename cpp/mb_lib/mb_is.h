#ifndef _MB_IS_H_
#define _MB_IS_H_

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
 
namespace mb {
  inline std::istream& iosBadbit(std::istream& is, const char* msg = "")
  {
    is.clear(std::ios::badbit); 
    if (msg != "") std::cerr << "is error: " << msg << std::flush;
    return is;
  }

  inline char* createBuffer(const std::string& file)
  {
    std::ifstream is(file.c_str(), std::ios::in | std::ios::binary);
    if (!is.is_open()) throw;
    is.seekg(0, std::ios::end);
    const int blob_size = is.tellg();
    is.seekg(0, std::ios::beg);
    char* buffer = new char[blob_size];  
    is.read(buffer, blob_size); // read into buffer
    is.close();
    return buffer;
  }

  // Read IS up to next newline character. Prefix and suffix std::strings
  // may be provided as optional arguments.
  inline std::string readLine(std::istream& is, char end_char = '\n', std::string prefix = "", std::string suffix = "") {
    char c = 0;
    while (is.get(c) && c != end_char)
      prefix.insert(prefix.end(), c);
    return prefix + suffix;
  }

  // Returns first line in FILENAME. See readLine for optionals.
  inline std::string readLineFile(const std::string& filename, char end_char = '\n', std::string prefix = "", std::string suffix = "") {
    std::ifstream is(filename.c_str());
    return readLine(is, end_char, prefix, suffix);
  }

  inline std::string readLines(std::istream& is, int n = 1, char end_char = '\n',
			       std::string infix = "", std::string prefix = "",
			       std::string suffix = "") 
  {
    // TODO: add infix
    if (!is.eof() && n > 0)
      { prefix += readLine(is, end_char); }
    int i = 1;
    for (; i < n && is; ++i)
      prefix += infix + readLine(is, end_char);
    return prefix + suffix;
  }

  // Reads IS with '<<' into a std::listof std::strings.
  inline std::list<std::string> readStrings(std::istream& is) {
    std::list<std::string> ss;
    std::copy(std::istream_iterator<std::string>(is), std::istream_iterator<std::string>(),
	      std::back_inserter(ss));
    return ss;
  }

  // Reads FILENAME with '<<' into a std::listof std::strings.
  inline std::list<std::string> readStringsFile(const std::string& filename) {
    std::ifstream is(filename.c_str());
    return readStrings(is);
  }

  inline std::string file2string(const string& path) {
    std::ifstream is(path.c_str());
    std::string res((std::istreambuf_iterator<char>(is)), std::istreambuf_iterator<char>());
    return res;
  }

  // Reads IS into a single std::string
  inline std::string readFile(std::istream& is) {
    char c;
    std::string s;
    while (is.get(c)) { s.insert(s.end(), c); }
    return s; }

  // Reads FILENAME into a single std::string  
  inline std::string readFile(const std::string& filename) {
    std::ifstream is(filename.c_str()); return readFile(is); }

  inline std::list<std::string> readList(std::istream& is, char mid = 0, char start = '(', char end = ')')
  {
    std::list<std::string> ret;
    char c = 0;
    is >> c;
    if (c == 0) return ret;
    if (c != start) {
      std::cerr << "readList: error start arg";
      return ret;
    }
    std::string s = "";
    is >> c;
    while (is) {
      if (c == end) {
	if (s != "")
	  ret.push_back(s);
	return ret;
      }
      else if ((mid == 0 && isspace(c)) || c == mid) {
	ret.push_back(s);
	s = "";
	is >> c;
      }
      else {	
	s += c;
	is.get(c);
      }
    }
    return ret;
  }

  inline std::list<std::string> readListS(std::string s, char mid = 0, char start = '(', char end = ')') {
    std::istringstream iss(s);
    return readList(iss, mid, start, end);
  }

} //mb

#endif //ifndef _MB_IS_H_
