package picante.time;

import java.text.ParseException;
import picante.spice.kernelpool.parser.time.TimeParser;
import picante.units.FundamentalPhysicalConstants;

/**
 * 
 * @author Hari.Nair@jhuapl.edu
 *
 */
public class UTCStringParser implements ITimeStringParser {

  /**
   * Parse UTC strings in the format yyyydddhhmmss or yyyymmddhhmmss.
   * 
   * @param utcString String to parse
   * @return Equivalent UTCEpoch
   * @throws ParseException
   */
  public static UTCEpoch getUTC(String utcString) throws ParseException {
    if (utcString.length() == 13) {
      // yyyydddhhmmss
      int y = Integer.parseInt(utcString.substring(0, 4));
      int doy = Integer.parseInt(utcString.substring(4, 7));
      int hr = Integer.parseInt(utcString.substring(7, 9));
      int mn = Integer.parseInt(utcString.substring(9, 11));
      int s = Integer.parseInt(utcString.substring(11, 13));
      return new UTCEpoch(y, doy, hr, mn, s);
    } else if (utcString.length() == 14) {
      // yyyymmddhhmmss
      int y = Integer.parseInt(utcString.substring(0, 4));
      int m = Integer.parseInt(utcString.substring(4, 6));
      int d = Integer.parseInt(utcString.substring(6, 8));
      int h = Integer.parseInt(utcString.substring(8, 10));
      int mn = Integer.parseInt(utcString.substring(10, 12));
      int s = Integer.parseInt(utcString.substring(12, 14));
      return new UTCEpoch(y, m, d, h, mn, s);

    } else {
      throw new InvalidUTCStringException(utcString);
    }
  }

  /**
   * Analog to NAIF TPARSE, see {@link TimeParser} for notes.
   * 
   * @param utcString time string to parse
   * @return seconds past the J2000 epoch
   */
  private static double tparse(String utcString) {

    TimeParser parser = new TimeParser();
    double secondsPastJ2000;

    try {
      secondsPastJ2000 = parser.tparse(utcString.trim());
    } catch (picante.spice.kernelpool.parser.time.ParseException e) {
      throw new InvalidUTCStringException(utcString);
    }

    return secondsPastJ2000;
  }

  /**
   * Convert a UTC string to a {@link UTCEpoch} object. See {@link TimeParser} for notes on allowed
   * formats. As with NAIF's TPARSE, all dates (including before 1582 Oct 15) are assumed to be in
   * the Gregorian Calendar,
   * <p>
   * To convert a {@link UTCEpoch} to a TDB time, use the {@link TimeConversion} class:
   * 
   * <pre>
   * TimeConversion tc = TimeConversion.createUsingInternalConstants();
   * UTCEpoch utc = UTCStringParser.createUTCEpoch(utcString);
   * double tdb = tc.utcToTDB(utc);
   * </pre>
   * 
   * @param utcString time string to parse
   * @return equivalent UTCEpoch
   * @author Hari.Nair@jhuapl.edu
   */
  public static UTCEpoch createUTCEpoch(String utcString) {
    double secondsPastJ2000 = tparse(utcString);
    double jd = secondsPastJ2000 / FundamentalPhysicalConstants.SECONDS_PER_DAY
        + FundamentalPhysicalConstants.JULIAN_DATE_OF_J2000;
    return new JulianDate(jd).toUTCEpoch();
  }

  @Override
  public TSEpoch createEpoch(String utcString) {
    return TimeAdapter.getInstance().getTSEpochFromUTC(createUTCEpoch(utcString));
  }

}
