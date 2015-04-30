#ifndef _MB_TIME_H
#define _MB_TIME_H

#include <ctime>
#include <mb_string_utils.h>
#include <mb_algorithm.h>
#include <mb_math.h>

// for struct tm, see http://www.cplusplus.com/reference/ctime/tm/
inline std::ostream& operator<<(std::ostream& os, const tm& to) {
  os << "{" 
     << to.tm_sec << "," 
     << to.tm_min << ","
     << to.tm_hour << ","
     << to.tm_mday << ","
     << to.tm_mon << ","
     << to.tm_year << ","
     << to.tm_wday << ","
     << to.tm_yday << ","
     << to.tm_isdst << "}";
  return os;
}

namespace mb {
  bool using_DST_ = true;

  enum time_unit { seconds_, minutes_, hours_, days_, weeks_ };

  enum weekdays_ { sunday_, monday_, tuesday_, wednesday_, thursday_, friday_, saturday_ }; // 0..6

  time_unit default_time_unit_ = days_;

  // Private
  typedef std::map<time_unit, int> TimeUnitMap;

  // Private
  inline TimeUnitMap initTimeUnitMap() {
    TimeUnitMap m;
    m[seconds_] = 1;
    m[minutes_] = 60 * m[seconds_];
    m[hours_] = 60 * m[minutes_];
    m[days_] = 24 * m[hours_];
    m[weeks_] = 7 * m[days_];
    return m;
  }

  // Private
  static TimeUnitMap time_unit_map_ = initTimeUnitMap();

  inline time_t now() {
    time_t current_time;
    time(&current_time);
    return current_time; 
  }

  inline tm time2tm(time_t time = now()) { return *localtime(&time); }
  inline time_t tm2time(tm to) { return mktime(&to); }

  inline time_t iso_date2time(std::string iso_string) {
    std::vector<std::string> ss = split_string(iso_string, '-');
    std::vector<int> ii = transform_vector(ss, std::ptr_fun(string2int));
    tm to = {0,0,0,0,0,0};
    to.tm_year = ii[0] - 1900;
    to.tm_mon = ii[1] - 1;
    to.tm_mday = ii[2];
    return tm2time(to);
  }

  inline time_t iso_time2time(std::string iso_string) {
    std::vector<std::string> ss = split_string(iso_string, ':');
    std::vector<int> ii = transform_vector(ss, std::ptr_fun(string2int));
    tm to = {0,0,0,1,0,70};
    to.tm_hour = ii[0];
    if (ii.size() > 1) to.tm_min = ii[1];
    if (ii.size() > 2) to.tm_sec = ii[2];
    return tm2time(to);
  }

  inline time_t iso_dttm2time(std::string iso_dttm) {
    std::vector<std::string> ss = split_string(iso_dttm, 'T');
    return iso_date2time(ss[0]) + iso_time2time(ss[1]);
  }

  inline time_t iso2time(std::string iso_string) {
    std::cerr << "Warning: iso2time is deprecated. Use iso_date2time instead." << std::endl;
    return iso_date2time(iso_string);
  }

  inline tm iso2tm(const std::string& iso_string) { return time2tm(iso2time(iso_string)); }

  inline int year(const tm& time_obj) { return time_obj.tm_year + 1900; }
  inline int mb_month(const tm& time_obj) { return time_obj.tm_mon + 1; }
  inline int mb_day(const tm& time_obj) { return time_obj.tm_mday; }
  inline int week_day(const tm& time_obj) { return time_obj.tm_wday; } // [0=Sun .. 6=Sat]
  inline int year_day(const tm& time_obj) { return time_obj.tm_yday; }

  inline std::string iso_date(time_t time = now()) {
    tm to = time2tm(time);
    return strprintf("%4d-%02d-%02d", year(to), mb_month(to), mb_day(to));
  }

  inline int in_DST_period(time_t time) { return (using_DST_ && (time2tm(time).tm_isdst > 0)) ? 1 : 0; }

  inline double diff_time(time_t a, time_t b, time_unit tu = default_time_unit_) {
    // returns time in TIME_UNIT between a and b
    
    double dt = difftime(a, b);
    int DST = (in_DST_period(b) - in_DST_period(a));
    DST = DST * 3600.0;
    double res = dt + DST;
    int dv = time_unit_map_[tu];
    res = res/dv;
    
    return (difftime(a, b) + (in_DST_period(b) - in_DST_period(a))*3600.0) / time_unit_map_[tu]; 
  }

  inline time_t add_time(time_t time, double d_time, time_unit tu = default_time_unit_) 
  { return time + mb::round(d_time*time_unit_map_[tu]); }

  inline tm add_time(const tm& to, double d_time, time_unit tu = default_time_unit_) 
  { return time2tm(add_time(tm2time(to), d_time, tu)); }

  inline std::string add_time(std::string iso_time, double d_time, time_unit tu = default_time_unit_) 
  { return iso_date(add_time(iso2time(iso_time), d_time, tu)); }

  inline double operator-(const tm& a, const tm& b) 
  { return diff_time(tm2time(a), tm2time(b)); }

  inline int operator+(const tm& to, double units)
  { return add_time(tm2time(to), units); }

  inline double sec2min  (double secs) { return secs/60; }
  inline double sec2hour (double secs) { return sec2min(secs)/60; }
  inline double sec2day  (double secs) { return sec2hour(secs)/24; }
  inline double sec2week (double secs) { return sec2day(secs)/7; }

  // Interval stuff
  inline std::pair<time_t, time_t> iso2time(std::pair<std::string, std::string> iso_interval)
  { return std::pair<time_t, time_t>(iso2time(iso_interval.first), iso2time(iso_interval.second)); }

  inline int diff_time(std::pair<time_t, time_t> x, time_unit tu = default_time_unit_) { 
    return diff_time(x.first, x.second, tu); 
  }

  inline int week_number(const tm& to) {
    // returns 1..53 (NB! week 53 could be current year or previous)
    int wd = week_day(to);
    int yd = year_day(to);
    int n_weeks = yd/7;
    int wd_first_year_day = modulo(wd-yd, 7);
    if (1 <= wd_first_year_day && wd_first_year_day < 5)
      // year starts with week 1
      return n_weeks + 1;
    // else year starts with week 52 or 53
    if (n_weeks > 0) 
      return n_weeks;
    else // 52 or 53
      return 1 + week_number(add_time(to, -1, weeks_));
  }
  inline int week_number(time_t time) { return week_number(time2tm(time)); }

}; // mb

#endif //_MB_TIME_H
