package picante.time;

import static picante.math.PicanteMath.round;

import com.google.common.annotations.VisibleForTesting;
import com.google.common.base.Splitter;
import java.util.Calendar;
import java.util.TimeZone;
import java.util.regex.Pattern;

/**
 * This class represents a single epoch (i.e. moment) in time in the Coordinated Universal Time
 * (abbreviated to UTC) time standard.
 *
 * <p>Since there is no year zero in the Julian calendar, negative years are shifted by 1 from the
 * equivalent B.C. year. For example, the date "18 B.C. Jun 3, 12:29:28.291" would be constructed as
 *
 * <pre>new UTCEpoch(-17, 6, 3, 12, 29, 28.291)</pre>
 *
 * @author vandejd1
 */
public class UTCEpoch implements Comparable<UTCEpoch> {

  /** Holds a single UTC Epoch (i.e. a "moment" in time) */
  private final int year;

  private final short doy;
  private final byte hour;
  private final byte min;
  private final double sec;

  /** A string in the form year-doyThh:mm:ss.sss */
  @Override
  public String toString() {
    // First round to the nearest millisecond:
    int[] vals = roundToMilliseconds();
    int other_yr = vals[0];
    int other_doy = vals[1];
    int other_hr = vals[2];
    int other_min = vals[3];
    int other_intSec = vals[4];
    int other_millis = vals[5];
    return String.format(
        "%4d-%03dT%02d:%02d:%02d.%03d",
        other_yr, other_doy, other_hr, other_min, other_intSec, other_millis);
  }

  /**
   * similar to toString() but the seconds field has been rounded to the nearest second
   *
   * @return a {@link String} representing the {@link UTCEpoch} in which the seconds field has been
   *     rounded to the nearest second
   */
  public String noSubsecondsString() {
    // First round to the nearest millisecond:
    int[] vals = roundToMilliseconds();
    int other_yr = vals[0];
    int other_doy = vals[1];
    int other_hr = vals[2];
    int other_min = vals[3];
    int other_intSec = vals[4];
    return String.format(
        "%4d-%03dT%02d:%02d:%02d", other_yr, other_doy, other_hr, other_min, other_intSec);
  }

  private static Splitter timeSplitter = Splitter.on(Pattern.compile("[ \\-T:]"));

  /**
   * Utility for reversing UTCEpoch.toString(). Use for any other purpose is not recommended
   *
   * @param timeString the output of a call to UTCEpoch toString()
   * @return
   */
  public static UTCEpoch fromString(String timeString) {
    Iterable<String> timeFields = timeSplitter.split(timeString.trim());
    double[] timeVals = new double[5];
    int i = 0;
    for (String f : timeFields) {
      timeVals[i++] = Double.parseDouble(f);
    }

    UTCEpoch utc =
        new UTCEpoch(
            (int) timeVals[0],
            (int) timeVals[1],
            (int) timeVals[2],
            (int) timeVals[3],
            timeVals[4]);
    return utc;
  }

  // this should be private, but I made it package private for testing
  // purposes only!!!
  @VisibleForTesting
  int[] roundToMilliseconds() {
    int other_yr = getYear();
    int other_doy = getDoy();
    int other_hr = getHour();
    int other_min = getMin();
    int other_intSec = (int) getSec();
    int other_millis = getNumberOfMillisAsInt(getSec());

    if (other_millis == 1000) {
      other_millis = 0;
      other_intSec++;
      if (other_intSec == 60) {
        other_intSec = 0;
        other_min++;
        if (other_min == 60) {
          other_min = 0;
          other_hr++;
          if (other_hr == 24) {
            other_hr = 0;
            other_doy++;
            if (other_doy > Calendarizer.daysInYear(other_yr)) {
              other_doy = 1;
              other_yr++;
            }
          }
        }
      }
    }
    return new int[] {other_yr, other_doy, other_hr, other_min, other_intSec, other_millis};
  }

  /**
   * @return a newly created {@link UTCEpoch} where the seconds field has been rounded to the
   *     nearest millisecond
   */
  public UTCEpoch createValueRoundedToMillisecs() {
    int[] vals = roundToMilliseconds();
    int other_yr = vals[0];
    int other_doy = vals[1];
    int other_hr = vals[2];
    int other_min = vals[3];
    int other_intSec = vals[4];
    int other_millis = vals[5];
    return new UTCEpoch(
        other_yr, other_doy, other_hr, other_min, other_intSec + other_millis / 1000.0);
  }

  private int getNumberOfMillisAsInt(double sec) {
    int intSec = (int) sec;
    double rawMillis = sec - intSec;
    int intMillis = (int) ((rawMillis * 1000.0) + 0.5);
    return intMillis;
  }

  // this should be private, but I made it package private for testing
  // purposes only!!!
  @VisibleForTesting
  int[] roundToMicroseconds() {
    int other_yr = getYear();
    int other_doy = getDoy();
    int other_hr = getHour();
    int other_min = getMin();
    double sec = getSec();
    int other_intSec = (int) sec;
    double otherJustMillis = 1000.0 * (sec - other_intSec);
    int other_intMillis = (int) otherJustMillis;
    double otherJustMicros = 1000.0 * (otherJustMillis - other_intMillis);
    int other_intMicros = (int) round(otherJustMicros);

    if (other_intMicros == 1000) {
      other_intMicros = 0;
      other_intMillis++;
      if (other_intMillis == 1000) {
        other_intMillis = 0;
        other_intSec++;
        if (other_intSec == 60) {
          other_intSec = 0;
          other_min++;
          if (other_min == 60) {
            other_min = 0;
            other_hr++;
            if (other_hr == 24) {
              other_hr = 0;
              other_doy++;
              if (other_doy > Calendarizer.daysInYear(other_yr)) {
                other_doy = 1;
                other_yr++;
              }
            }
          }
        }
      }
    }
    return new int[] {
      other_yr, other_doy, other_hr, other_min, other_intSec, other_intMillis, other_intMicros
    };
  }

  /**
   * @return a newly created {@link UTCEpoch} where the seconds field has been rounded to the
   *     nearest microssecond
   */
  public UTCEpoch createValueRoundedToMicrosecs() {
    int[] vals = roundToMicroseconds();
    int other_yr = vals[0];
    int other_doy = vals[1];
    int other_hr = vals[2];
    int other_min = vals[3];
    int other_intSec = vals[4];
    int other_millis = vals[5];
    int other_micros = vals[6];
    return new UTCEpoch(
        other_yr,
        other_doy,
        other_hr,
        other_min,
        other_intSec + other_millis / 1000.0 + other_micros / 1.e6);
  }

  /**
   * @return the current {@link UTCEpoch}
   */
  public static UTCEpoch getCurrentUTC() {
    Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT+0"));
    return new UTCEpoch(
        cal.get(Calendar.YEAR),
        cal.get(Calendar.DAY_OF_YEAR),
        cal.get(Calendar.HOUR_OF_DAY),
        cal.get(Calendar.MINUTE),
        cal.get(Calendar.SECOND));
  }

  /**
   * @param year a standard year, which starts from 1
   * @param doy the day of the year, starting from 1, which corresponds to January 1st
   * @param hour the hour of the day, ranging from 0 - 23
   * @param min the minute of the hour, ranging from 0 - 59
   * @return UTCEpoch with the minute field incremented by 1; leap days are accounted for, but not
   *     leap seconds
   */
  public static UTCEpoch getNextMin(int year, int doy, int hour, int min) {
    int nextMin = min + 1;
    int nextHour = hour;
    int nextDoy = doy;
    int nextYear = year;
    if (nextMin > 59) {
      UTCEpoch next = UTCEpoch.getNextHour(nextYear, nextDoy, nextHour);
      nextMin = 0;
      nextHour = next.getHour();
      nextDoy = next.getDoy();
      nextYear = next.getYear();
    }

    return new UTCEpoch(nextYear, nextDoy, nextHour, nextMin, 0);
  }

  /**
   * @param year a standard year, which starts from 1
   * @param doy the day of the year, starting from 1, which corresponds to January 1st
   * @param hour the hour of the day, ranging from 0 - 23
   * @param min the minute of the hour, ranging from 0 - 59
   * @return UTCEpoch with the minute field decremented by 1; leap days are accounted for, but not
   *     leap seconds
   */
  public static UTCEpoch getPreviousMin(int year, int doy, int hour, int min) {
    int prevMin = min - 1;
    int prevHour = hour;
    int prevDoy = doy;
    int prevYear = year;
    if (prevMin < 0) {
      UTCEpoch prev = UTCEpoch.getPreviousHour(prevYear, prevDoy, prevHour);
      prevMin = 59;
      prevHour = prev.getHour();
      prevDoy = prev.getDoy();
      prevYear = prev.getYear();
    }

    return new UTCEpoch(prevYear, prevDoy, prevHour, prevMin, 0);
  }

  /**
   * @param year a standard year, which starts from 1
   * @param doy the day of the year, starting from 1, which corresponds to January 1st
   * @return UTCEpoch with the hour field incremented by 1; leap days are accounted for, but not
   *     leap seconds
   */
  public static UTCEpoch getNextHour(int year, int doy, int hour) {
    int nextHour = hour + 1;
    int nextDoy = doy;
    int nextYear = year;
    if (nextHour > 23) {
      UTCEpoch next = UTCEpoch.getNextDay(nextYear, nextDoy);
      nextHour = 0;
      nextDoy = next.getDoy();
      nextYear = next.getYear();
    }

    return new UTCEpoch(nextYear, nextDoy, nextHour, 0, 0);
  }

  /**
   * @param year a standard year, which starts from 1
   * @param doy the day of the year, starting from 1, which corresponds to January 1st
   * @param hour the hour of the day, ranging from 0 - 23
   * @return UTCEpoch with the hour field decremented by 1; leap days are accounted for, but not
   *     leap seconds
   */
  public static UTCEpoch getPreviousHour(int year, int doy, int hour) {
    int prevHour = hour - 1;
    int prevDoy = doy;
    int prevYear = year;
    if (prevHour < 0) {
      UTCEpoch prev = UTCEpoch.getPreviousDay(prevYear, prevDoy);
      prevHour = 23;
      prevDoy = prev.getDoy();
      prevYear = prev.getYear();
    }

    return new UTCEpoch(prevYear, prevDoy, prevHour, 0, 0);
  }

  /**
   * @param year a standard year, which starts from 1
   * @param doy the day of the year, starting from 1, which corresponds to January 1st
   * @return UTCEpoch with the day field incremented by 1; leap days are accounted for, but not leap
   *     seconds
   */
  public static UTCEpoch getNextDay(int year, int doy) {
    int nextDoy = doy + 1;
    int nextYear = year;
    if (nextDoy > Calendarizer.daysInYear(year)) {
      nextYear = year + 1;
      nextDoy = 1;
    }

    return new UTCEpoch(nextYear, nextDoy, 0, 0, 0);
  }

  /**
   * @param year a standard year, which starts from 1
   * @param doy the day of the year, starting from 1, which corresponds to January 1st
   * @return UTCEpoch with the day field decremented by 1; leap days are accounted for, but not leap
   *     seconds
   */
  public static UTCEpoch getPreviousDay(int year, int doy) {
    int prevDoy = doy - 1;
    int prevYear = year;
    if (prevDoy < 1) {
      prevYear--;
      prevDoy = Calendarizer.daysInYear(prevYear);
    }

    return new UTCEpoch(prevYear, prevDoy, 0, 0, 0);
  }

  /**
   * @param year a standard year, which starts from 1
   * @param month a standard month, starting from 1, which corresponds to January
   * @return UTCEpoch with the month field incremented by 1; leap days are accounted for, but not
   *     leap seconds
   */
  public static UTCEpoch getNextMonth(int year, int month) {
    int nextMonth = month + 1;
    int nextYear = year;
    if (nextMonth > 12) {
      nextYear = year + 1;
      nextMonth = 1;
    }

    return new UTCEpoch(nextYear, nextMonth, 1, 0, 0, 0);
  }

  /**
   * @param year a standard year, which starts from 1
   * @param month a standard month, starting from 1, which corresponds to January
   * @return UTCEpoch with the month field decremented by 1; leap days are accounted for, but not
   *     leap seconds
   */
  public static UTCEpoch getPreviousMonth(int year, int month) {
    int previousMonth = month - 1;
    int previousYear = year;
    if (previousMonth < 1) {
      previousYear = year - 1;
      previousMonth = 12;
    }

    return new UTCEpoch(previousYear, previousMonth, 1, 0, 0, 0);
  }

  /**
   * TODO NEEDS CHECKS
   *
   * @param year a standard year, which starts from 1
   * @param month a standard month, starting from 1, which corresponds to January
   * @param monthDay a standard day of month, starting from 1, which accounts for leap years
   * @param hour the hour of the day, ranging from 0 - 23
   * @param min the minute of the hour, ranging from 0 - 59
   * @param sec the second of the minute, ranging from 0.0 - <60.0 for minutes not containing a leap
   *     second, from 0.0 - <61.0 for minutes that contain a positive leapsecond, and 0.0 - <59.0
   *     for minutes that contain a negative leapsecond
   */
  public UTCEpoch(int year, int month, int monthDay, int hour, int min, double sec) {
    this.year = year;
    this.doy = (short) Calendarizer.getDayOfYear(year, month, monthDay);
    this.hour = (byte) hour;
    this.min = (byte) min;
    this.sec = sec;
  }

  /**
   * TODO NEEDS CHECKS
   *
   * @param year a standard year starting from 1
   * @param doy the day of the year, starting from 1, which corresponds to January 1st
   * @param hour the hour of the day, ranging from 0 - 23
   * @param min the minute of the hour, ranging from 0 - 59
   * @param sec the second of the minute, ranging from 0.0 - <60.0 for minutes not containing a leap
   *     second, from 0.0 - <61.0 for minutes that contain a positive leapsecond, and 0.0 - <59.0
   *     for minutes that contain a negative leapsecond
   */
  public UTCEpoch(int year, int doy, int hour, int min, double sec) {
    this.year = year;
    this.doy = (short) doy;
    this.hour = (byte) hour;
    this.min = (byte) min;
    this.sec = sec;
  }

  /**
   * @return the standard year starting from 1
   */
  public int getYear() {
    return year;
  }

  /**
   * @return the integer month of the year starting from 1 that corresponds to January (1 to 12)
   */
  public int getMonth() {
    return Calendarizer.getMonthAndMonthDay(year, doy)[0];
  }

  /**
   * @return the integer day of month, starting from 1, i.e. January 8th will have a day of month of
   *     8
   */
  public int getDom() {
    return Calendarizer.getMonthAndMonthDay(year, doy)[1];
  }

  /**
   * @return the integer day of year, starting from 1 that corresponds to January 1st (1 to 365/366
   *     for leap years)
   */
  public int getDoy() {
    return doy;
  }

  /**
   * @return the integer hour of the day, starting from 0 (0 to 59)
   */
  public int getHour() {
    return hour;
  }

  /**
   * @return the integer minute of the hour, starting from 0 (0 to 59)
   */
  public int getMin() {
    return min;
  }

  /**
   * @return the double second of the minute, starting from 0.0 (0.0 to 59.999... except in the case
   *     of a positive leap second where the range is 0.0 to 60.999... or 0.0 to 58.999... in case
   *     of a negative leap second, note a negative leap second has never been issued)
   */
  public double getSec() {
    return sec;
  }

  @Override
  public int compareTo(UTCEpoch o) {
    if (year < o.year) {
      return -1;
    }
    if (year > o.year) {
      return 1;
    }

    if (doy < o.doy) {
      return -1;
    }
    if (doy > o.doy) {
      return 1;
    }

    if (hour < o.hour) {
      return -1;
    }
    if (hour > o.hour) {
      return 1;
    }

    if (min < o.min) {
      return -1;
    }
    if (min > o.min) {
      return 1;
    }

    if (sec < o.sec) {
      return -1;
    }
    if (sec > o.sec) {
      return 1;
    }

    return 0;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + doy;
    result = prime * result + hour;
    result = prime * result + min;
    long temp;
    temp = Double.doubleToLongBits(sec);
    result = prime * result + (int) (temp ^ (temp >>> 32));
    result = prime * result + year;
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    UTCEpoch other = (UTCEpoch) obj;
    if (doy != other.doy) {
      return false;
    }
    if (hour != other.hour) {
      return false;
    }
    if (min != other.min) {
      return false;
    }
    if (Double.doubleToLongBits(sec) != Double.doubleToLongBits(other.sec)) {
      return false;
    }
    if (year != other.year) {
      return false;
    }
    return true;
  }

  /**
   * @return a newly created {@link UTCEpoch} where the seconds field has been rounded to the
   *     nearest whole second
   */
  public UTCEpoch createValueRoundedToSeconds() {
    int[] vals = roundToSeconds();
    int other_yr = vals[0];
    int other_doy = vals[1];
    int other_hr = vals[2];
    int other_min = vals[3];
    int other_sec = vals[4];
    return new UTCEpoch(other_yr, other_doy, other_hr, other_min, other_sec);
  }

  private int[] roundToSeconds() {
    int other_yr = getYear();
    int other_doy = getDoy();
    int other_hr = getHour();
    int other_min = getMin();
    int other_sec = (int) (getSec() + 0.5);

    if (other_sec == 60) {
      other_sec = 0;
      other_min++;
      if (other_min == 60) {
        other_min = 0;
        other_hr++;
        if (other_hr == 24) {
          other_hr = 0;
          other_doy++;
          if (other_doy > Calendarizer.daysInYear(other_yr)) {
            other_doy = 1;
            other_yr++;
          }
        }
      }
    }
    return new int[] {other_yr, other_doy, other_hr, other_min, other_sec};
  }

  /**
   * @return a newly created {@link UTCEpoch} where the minute field has been rounded to the nearest
   *     whole minute (the seconds field is set to 0.0)
   */
  public UTCEpoch createValueRoundedToMinutes() {
    int[] vals = roundToMinutes();
    int other_yr = vals[0];
    int other_doy = vals[1];
    int other_hr = vals[2];
    int other_min = vals[3];
    double other_sec = 0;
    return new UTCEpoch(other_yr, other_doy, other_hr, other_min, other_sec);
  }

  private int[] roundToMinutes() {
    int other_yr = getYear();
    int other_doy = getDoy();
    int other_hr = getHour();
    int other_min = (int) (0.5 + getMin() + getSec() / 60.0);

    if (other_min == 60) {
      other_min = 0;
      other_hr++;
      if (other_hr == 24) {
        other_hr = 0;
        other_doy++;
        if (other_doy > Calendarizer.daysInYear(other_yr)) {
          other_doy = 1;
          other_yr++;
        }
      }
    }
    return new int[] {other_yr, other_doy, other_hr, other_min};
  }

  /**
   * @return the fractional day of year, where 1.0 is midnight January 1st.
   */
  public double getAsFractionalDoy() {
    return getDoy() + getHour() / 24.0 + getMin() / (24.0 * 60.0) + getSec() / (24.0 * 3600.0);
  }
}
