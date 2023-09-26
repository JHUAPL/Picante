package picante.time;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkNotNull;
import static picante.math.PicanteMath.ceil;
import static picante.math.PicanteMath.floor;
import static picante.math.PicanteMath.max;
import static picante.math.PicanteMath.min;
import static picante.math.PicanteMath.sin;
import java.util.Arrays;

/*
 * Note that this class has only package visibility so we don't use this class directly outside this
 * package (we'll be using a new implementation from Scott eventually, so we want to force people to
 * only use the IUTCProvider interface, i.e. through BasicUTCProvider).
 */

public class UTCFactory {

  private final static int CMAP_ITERATIONS = 3;

  private final static int DAYS_PER_400_YEARS = 365 * 400 + 97;

  private final static int DAYS_PER_100_YEARS = 365 * 100 + 24;

  private final static int DAYS_PER_4_YEARS = 365 * 4 + 1;

  private final static int DAYS_PER_YEAR = 365;

  private final static double SECONDS_PER_DAY = 86400.0;

  private final int[] dayTable;
  private final double[] taiTable;
  private final double deltaTa;
  private final double eb;
  private final double k;
  private final double[] m;

  /**
   * Constructor
   * 
   * @param dayTable
   * @param taiTable
   * @param deltaTa
   * @param eb
   * @param k
   * @param m0
   * @param m1
   */
  public UTCFactory(int[] dayTable, double[] taiTable, double deltaTa, double eb, double k,
      double m0, double m1) {
    super();
    checkArgument(dayTable.length == taiTable.length);
    this.dayTable = checkNotNull(dayTable);
    this.taiTable = checkNotNull(taiTable);
    this.deltaTa = deltaTa;
    this.eb = eb;
    this.k = k;
    this.m = new double[] {m0, m1};
  }

  /**
   * Given an {@link UTCEpoch}, computes the TDB (Barycentric Dynamical Time) time for that epoch
   * 
   * @param utc an {@link UTCEpoch}
   * @return the elapsed seconds since Greenwich noon on January 1st 2000 in the TDB time system for
   *         the input {@link UTCEpoch}
   */
  public double getTDB(UTCEpoch utc) {
    return getTDB(utc.getYear(), utc.getDoy(), utc.getHour(), utc.getMin(), utc.getSec());
  }

  /**
   * Given an {@link UTCEpoch}, computes the TAI (International Atomic Time) time for that epoch
   * 
   * @param utc an {@link UTCEpoch}
   * @return the elapsed seconds since Greenwich noon on January 1st 2000 in the TAI time system for
   *         the input {@link UTCEpoch}
   */
  public double getTAI(UTCEpoch utc) {
    return getTAI(utc.getYear(), utc.getDoy(), utc.getHour(), utc.getMin(), utc.getSec());
  }

  /**
   * Given an epoch, computes the TDB (Barycentric Dynamical Time) time for that epoch
   * 
   * @param year
   * @param dayOfYear
   * @param hour
   * @param minute
   * @param seconds
   * @return the elapsed seconds since Greenwich noon on January 1st 2000 in the TDB time system for
   *         the input {@link UTCEpoch}
   */
  double getTDB(int year, int dayOfYear, int hour, int minute, double seconds) {

    double tai = getTAI(year, dayOfYear, hour, minute, seconds);

    return convertTAItoTDB(tai);
  }

  /**
   * Given an epoch, computes the TAI (International Atomic Time) time for that epoch
   * 
   * @param year
   * @param dayOfYear
   * @param hour
   * @param minute
   * @param seconds
   * @return the elapsed seconds since Greenwich noon on January 1st 2000 in the TAI time system for
   *         the input {@link UTCEpoch}
   */
  double getTAI(int year, int dayOfYear, int hour, int minute, double seconds) {
    int dOffset = year <= 0 && !Calendarizer.isLeapYear(year-1) ? -1 : 0;

    /*
     * Handle negative years.
     */
/*-
    if (year <= 0) {

      int q = year / 400;
      int r = year - 400 * q;

      if (r < 0) {
        q--;
        r += 400;
      }

      if (year == 0) {
        year = year + 400;
        q = q - 1;
      }

      dOffset = q * DAYS_PER_400_YEARS;
    }
*/
    DaySecond ds = new DaySecond();
    ds.daysSince1AD = daysPast0001(year, dayOfYear) + dOffset;
    ds.secondsOfDay = hour * 3600.0 + minute * 60.0 + seconds;

    /*
     * Check to see if this is a valid UTC value.
     */
    if (!isValidUTC(ds)) {
      throw new RuntimeException("Invalid UTC time specified");
    }

    double tai = convertUTCtoTAI(ds);

    return tai;
  }

  /**
   * Given a TDB (Barycentric Dynamical Time) time, computes the {@link UTCEpoch} for that time
   *
   * @param tdb the elapsed seconds since Greenwich noon on January 1st 2000 in the TDB time system
   *        for the input {@link UTCEpoch}
   * @return the {@link UTCEpoch} for the input TDB time
   */
  public UTCEpoch getUTCfromTDB(double tdb) {
    /*
     * Start by converting TDB to TAI.
     */
    double tai = convertTDBtoTAI(tdb);

    return getUTCfromTAI(tai);
  }

  /**
   * Given a TAI (International Atomic Time) time, computes the {@link UTCEpoch} for that time
   *
   * @param tai the elapsed seconds since Greenwich noon on January 1st 2000 in the TAI time system
   *        for the input {@link UTCEpoch}
   * @return the {@link UTCEpoch} for the input TAI time
   */
  public UTCEpoch getUTCfromTAI(double tai) {
    /*
     * Now convert tai to UTC day seconds.
     */
    DaySecond ds = convertTAItoUTC(tai, new DaySecond());

    int yr400 = ds.daysSince1AD / DAYS_PER_400_YEARS;
    int rem = ds.daysSince1AD - DAYS_PER_400_YEARS * yr400;

    /*
     * Handle the case of years prior to 1 JAN 1.
     */
    if (rem < 0) {
      yr400--;
      rem += DAYS_PER_400_YEARS;
    }

    int yr100 = min(3, rem / DAYS_PER_100_YEARS);
    rem -= yr100 * DAYS_PER_100_YEARS;

    int yr4 = min(24, rem / DAYS_PER_4_YEARS);
    rem -= yr4 * DAYS_PER_4_YEARS;

    int yr1 = min(3, rem / DAYS_PER_YEAR);
    rem -= yr1 * DAYS_PER_YEAR;

    int dayOfYear = rem + 1;

    int year = yr400 * 400 + yr100 * 100 + yr4 * 4 + yr1 + 1;

    /*
     * We only want to convert the seconds per day that are 86399 seconds or less. Hold onto the
     * extra seconds.
     */
    double extraSeconds = max(0.0, ds.secondsOfDay - SECONDS_PER_DAY + 1);
    double tSeconds = ds.secondsOfDay - extraSeconds;

    /*
     * Split out the fields for hour, minute, and seconds.
     */
    int q = (int) (tSeconds / 3600.0);
    double r = tSeconds - q * 3600.0;

    if (r < 0.0) {
      q--;
      r += 3600.0;
    }

    int hours = q;
    tSeconds = r;

    q = (int) (tSeconds / 60.0);
    r = tSeconds - q * 60.0;

    if (r < 0.0) {
      q--;
      q += 60.0;
    }

    int minutes = q;
    tSeconds = r;

    double seconds = tSeconds + extraSeconds;

    return new UTCEpoch(year, dayOfYear, hours, minutes, seconds);
  }

  /**
   * 
   * @param tdtSecondsPastJ2000
   * @return
   */
  double convertTDTtoTDB(double tdtSecondsPastJ2000) {

    return tdtSecondsPastJ2000
        + k * sin(m[0] + m[1] * tdtSecondsPastJ2000 + eb * sin(m[0] + m[1] * tdtSecondsPastJ2000));

  }

  /**
   * 
   * @param tdb
   * @return
   */
  double convertTDBtoTDT(double tdb) {

    /*
     * The following text is taken from the SPICELIB routine UNITIM. It provides a detailed
     * explanation of why the algorithm used to invert the TDT to TDB conversion works.
     * 
     * TODO: Fix the formatting of these pasted comments, the eclipse code formatter... strikes...
     * yet again.
     * 
     * What we have to do here is invert the formula used to get TDB from TDT that was used above.
     * 
     * Of course solving the equation
     * 
     * TDB = TDT + K*SIN { M0 + M1*TDT + EB*SIN( MO + M1*TDT ) }
     * 
     * analytically for TDT if given TDB is no piece of cake. However, we can get as close as we
     * want to TDT if we notice a few tricks. First, let's let f(t) denote the function
     * 
     * f(t) = SIN( M0 + M1*t + EB*SIN( M0 + M1*t ) )
     * 
     * With this simpler notation we can rewrite our problem as that of solving the equation
     * 
     * y = t + K*f(t)
     * 
     * for t given y. Whichever t satisfies this equation will be unique. The uniqueness of the
     * solution is ensured because the expression on the right-hand side of the equation is monotone
     * increasing in t.
     * 
     * Let's suppose that t is the solution, then the following is true.
     * 
     * t = y - K*f(t)
     * 
     * but we can also replace the t on the right hand side of the equation by y - K*f(t). Thus
     * 
     * t = y - K*f( y - K*f(t)) = y - K*f( y - K*f( y - K*f(t))) = y - K*f( y - K*f( y - K*f( y -
     * K*f(t)))) = y - K*f( y - K*f( y - K*f( y - K*f( y - K*f(t))))) . . . = y - K*f( y - K*f( y -
     * K*f( y - K*f( y - K*f(y - ... )))
     * 
     * and so on, for as long as we have patience to perform the substitutions.
     * 
     * The point of doing this recursive substitution is that we hope to move t to an insignificant
     * part of the computation. This would seem to have a reasonable chance of success since K is a
     * small number and f is bounded by 1.
     * 
     * Following this idea, we will attempt to solve for t using the recursive method outlined
     * below.
     * 
     * We will make our first guess at t, call it t_0.
     * 
     * t_0 = y
     * 
     * Our next guess, t_1, is given by:
     * 
     * t_1 = y - K*f(t_0)
     * 
     * And so on:
     * 
     * t_2 = y - K*f(t_1) [ = y - K*f(y - K*f(y)) ] t_3 = y - K*f(t_2) [ = y - K*f(y - K*f(y -
     * K*f(y))) ] . . . t_n = y - K*f(t_(n-1)) [ = y - K*f(y - K*f(y - K*f(y...)))]
     * 
     * The questions to ask at this point are:
     * 
     * 1) Do the t_i's converge? 2) If they converge, do they converge to t? 3) If they converge to
     * t, how fast do they get there?
     * 
     * 1) The sequence of approximations converges. | t_n - t_(n-1) | = [ y - K*f( t_(n-1) ) ] - [ y
     * - K*f( t_(n-2) ) ] = K*[ f( t_(n-2) ) - f( t_(n-1) ) ]
     * 
     * The function f has an important property. The absolute value of its derivative is always less
     * than M1*(1+EB). This means that for any pair of real numbers s,t | f(t) - f(s) | <
     * M1*(1+EB)*| t - s |.
     * 
     * From this observation, we can see that | t_n - t_(n-1) | < K*M1*(1+EB)*| t_(n-1) - t_(n-2) |
     * 
     * With this fact available, we could (with a bit more work) conclude that the sequence of t_i's
     * converges and that it converges at a rate that is at least as fast as the sequence L, L**2,
     * L**3, ....
     * 
     * Where L = K*M1*(1+EB) << 1.
     * 
     * 2) If we let t be the limit of the t_i's then it follows that
     * 
     * t = y - K*f(t).
     * 
     * or that
     * 
     * y = t + K*f(t).
     * 
     * 3) As we already pointed out, the sequence of t_i's converges at least as fast as the
     * geometric series L, L**2, ...
     * 
     * 
     * Since K*M1*(1+EB) is quite small (on the order of 10**-9) 3 iterations should get us as close
     * as we can get to the solution for TDT.
     * 
     * In our case, the number of iterations is defined in the parameter CMAP_ITERATIONS, rather
     * than 3 explicitly.
     */
    double result = tdb;

    for (int i = 0; i < CMAP_ITERATIONS; i++) {
      result = tdb - k * sin(m[0] + m[1] * result + eb * sin(m[0] + m[1] * result));
    }

    return result;
  }

  /**
   * 
   * @param taiSec
   * @return
   */
  double convertTAItoTDB(double taiSec) {
    /*
     * The conversion from TAI to tdb (in this case TDB), simply requires moving from TAI to TDT and
     * then TDT to TDB.
     */
    return convertTDTtoTDB(taiSec + deltaTa);

  }

  /**
   * 
   * @param tdbSec
   * @return
   */
  double convertTDBtoTAI(double tdbSec) {
    /*
     * The conversion from tdb (TDB in this case) to TAI requires moving first from tdb to TDT and
     * then to TAI.
     */
    return convertTDBtoTDT(tdbSec) - deltaTa;
  }

  /**
   * 
   * @param taiSecondsPastJ2000
   * @param buffer
   * @return
   */
  DaySecond convertTAItoUTC(double taiSecondsPastJ2000, DaySecond buffer) {

    /*
     * First look up the index of the element equal to or just less than the supplied
     * taiSecondsPastJ2000 in taiTable.
     */
    int index = getTAITableIndex(taiSecondsPastJ2000);

    /*
     * Since taiTable is constructed in the manner that contains the tai times at the start and end
     * of a day containing a leapsecond, if the supplied index is even, then the value lies in a day
     * when a leapsecond occurs.
     */
    if (index % 2 == 0) {
      buffer.daysSince1AD = dayTable[index];
      buffer.secondsOfDay = taiSecondsPastJ2000 - taiTable[index];
      return buffer;
    }

    /*
     * Otherwise we are on a completely normal day without leapseconds. We do have a little work to
     * handle the case of times that occur prior to the start of the leapsecond table, but that's
     * simply handled by constraining the index into the taiTable to be 0 in that event.
     */
    index = max(index, 0);

    /*
     * Now break the resultant offset into seconds of day and days since 1 AD. This requires a
     * little modulo arithmetic mental gymnastics.
     */
    double numerator = taiSecondsPastJ2000 - taiTable[index];
    double quotient = numerator / SECONDS_PER_DAY;

    /*
     * Now we need to properly truncate the quotient into an integer. This requires some creative
     * use of the floor and ceil functions in the math library. Truncate everything "towards" zero.
     */
    quotient = (quotient < 0) ? ceil(quotient) : floor(quotient);

    double remainder = numerator - quotient * SECONDS_PER_DAY;

    if (remainder < 0) {
      quotient--;
      remainder += SECONDS_PER_DAY;
    }

    buffer.daysSince1AD = (int) quotient + dayTable[index];
    buffer.secondsOfDay = remainder;

    return buffer;
  }

  /**
   * {@inheritDoc}
   * 
   * @return {@inheritDoc} This particular implementation is forgiving, if the supplied utcTime
   *         isn't strictly valid; it simply computes rollover assuming an offset of from the start
   *         of the supplied day component by the supplied number of seconds. This is then converted
   *         properly to TAI.
   */
  double convertUTCtoTAI(DaySecond utcTime) {

    /*
     * Assume that the caller has provided us with a utcTime that makes sense. If they provide
     * something that has a seconds component that is invalid, we just apply the standard rules of
     * rollover and convert it nonetheless.
     */
    int dayIndex = max(0, getDayTableIndex(utcTime.daysSince1AD));

    double seconds =
        utcTime.secondsOfDay + (utcTime.daysSince1AD - dayTable[dayIndex]) * SECONDS_PER_DAY;

    return taiTable[dayIndex] + seconds;
  }

  /**
   * {@inheritDoc}
   * <p>
   * This implementation of the leapseconds provider interface defines a value UTC time as one that
   * contains the number of seconds ellapsed on a particular day that is in the strictly allowed
   * range. Specifically the seconds per day value must lie in one of the following three sets:
   * <ul>
   * <li>[0,86400)</li>
   * <li>[0, 86401), in the case of a normal leapsecond</li>
   * <li>[0, 86399), in the case of a "negative" leapsecond</li>
   * </ul>
   * </p>
   */
  boolean isValidUTC(DaySecond utcTime) {

    /*
     * Locate the day in dayTable that occurs just prior to the specified day.
     */
    int index = getDayTableIndex(utcTime.daysSince1AD);

    /*
     * If index is even, then we know we fall on a day where DUT changed.
     */
    double validSecondsCount =
        (index % 2 == 0) ? (taiTable[index + 1] - taiTable[index]) : SECONDS_PER_DAY;

    return (utcTime.secondsOfDay < validSecondsCount) && (utcTime.secondsOfDay >= 0.0);
  }

  /**
   * 
   * @param year
   * @param dayOfYear
   * @return
   */
  int daysPast0001(int year, int dayOfYear) {
    int yearM1 = year - 1;
    return 365 * yearM1 + ((yearM1) / 4) - ((yearM1) / 100) + ((yearM1) / 400) + dayOfYear - 1;
  }

  /**
   * Locate the index in the taiTable where the supplied TAI seconds past J2000 would be located.
   * 
   * @param taiSecondsPastJ2000
   * 
   * @return an index into taiTable of the element just less than or equal to the supplied
   *         taiSecondsPastJ2000. If the supplied value occurs prior to all elements in the table,
   *         then -1 is returned. If the supplied value exceeds all elements in the table, then the
   *         last valid index in taiTable is returned.
   */
  int getTAITableIndex(double taiSecondsPastJ2000) {

    /*
     * We can assume that the taiTable is a monotonically increasing sequence of tai reference
     * epochs, based on how it was constructed. Therefore simply apply the binary search algorithm,
     * and permute the output if its negative to create the desired "lastLessThanOrEqualTo"
     * behavior.
     */
    int index = Arrays.binarySearch(taiTable, taiSecondsPastJ2000);

    if (index < 0) {
      return -(index + 2);
    }

    return index;
  }

  /**
   * Locate the index in the dayTable where the supplied days since 1AD would be located.
   * 
   * @param daysSince1AD
   * 
   * @return an index into dayTable of the element just less than or equal to the supplied
   *         daysSince1AD. If the supplied value occurs prior to all elements in the table, then -1
   *         is returned. If the supplied value exceeds all elements in the table then the last
   *         valid index in dayTable is returned.
   */
  int getDayTableIndex(int daysSince1AD) {

    int index = Arrays.binarySearch(dayTable, daysSince1AD);

    if (index < 0) {
      return -(index + 2);
    }

    return index;
  }


}
