/**
 * Author : vandejd1 Created : Nov 17, 2010
 * 
 * Copyright (C) 2010 The Johns Hopkins University Applied Physics Laboratory (JHU/APL) All rights
 * reserved
 */
package picante.time;

/**
 * 
 * @author vandejd1
 */
public class Calendarizer {
  private static final int[] daysPerMonthForRegYear =
      new int[] {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

  private static final int[] daysPerMonthForLeapYear =
      new int[] {31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

  /**
   * returns the number of days in the given year
   * 
   * @param year the 4 digit year
   * @return integer number of days in the year <code>year</code>
   */
  public static int daysInYear(int year) {
    return isLeapYear(year) ? 366 : 365;
  }

  /**
   * returns the number of days in the given year
   * 
   * @param year the 4 digit year
   * @return integer number of days in the year <code>year</code>
   */
  public static int daysInMonth(int month, int year) {
    return isLeapYear(year) ? daysPerMonthForLeapYear[month - 1]
        : daysPerMonthForRegYear[month - 1];
  }

  /**
   * returns true if the year is a leap year
   * 
   * @param year
   * @return boolean representing whether <code>year</code> is leap year or not
   */
  public static boolean isLeapYear(int year) {
    return (((year % 4) == 0) && ((year % 100) != 0)) || ((year % 400) == 0);
  }

  private static int[] getDaysPerMonth(int yr) {
    if (isLeapYear(yr)) {
      return daysPerMonthForLeapYear;
    } else {
      return daysPerMonthForRegYear;
    }
  }

  /**
   * returns an array with two elements: the month (Jan is month 1), and the day of the month (the
   * first day of the month is day 1)
   * 
   * @param yr the four digit year
   * @param doy the day of year, with Jan 1 being day 1
   * @return array month and month day as integers in an array
   */
  public static int[] getMonthAndMonthDay(int yr, int doy) {
    // Figure out the month and month day numbers.
    // Note that January is month 1 and the first day
    // of the month is day number 1.

    int[] daysPerMonth = getDaysPerMonth(yr);

    int month = 0;
    int daysAtMonthBoundaries = daysPerMonth[month];
    // loop until the contributions from the months exceeds the doy
    while (doy > daysAtMonthBoundaries) {
      month++;
      daysAtMonthBoundaries += daysPerMonth[month];
    }
    // go back to the number of days for last whole month included
    daysAtMonthBoundaries -= daysPerMonth[month];

    month++; // makes month number start at 1 (i.e., January would be
             // month 1)

    // the days in the current month equal however many days are left after
    // we get to the previous whole month
    int monthDay = doy - daysAtMonthBoundaries;

    return new int[] {month, monthDay};
  }

  /**
   * simple conversion method for getting the day of year from a given Year/Month/Day
   * 
   * @param yr
   * @param monthNumber
   * @param dayOfMonth
   * @return integer value of the day of year for the given <code>year/monthNumber/dayOfMonth</code>
   */
  public static int getDayOfYear(int yr, int monthNumber, int dayOfMonth) {
    int[] daysPerMonth = getDaysPerMonth(yr);

    int monthIndexHi = monthNumber - 1; // change from month number (Jan=1)
                                        // to month index number (Jan=0)

    int doy = dayOfMonth;
    for (int tmpMonth = 0; tmpMonth < monthIndexHi; tmpMonth++) {
      doy += daysPerMonth[tmpMonth];
    }

    return doy;
  }
}
