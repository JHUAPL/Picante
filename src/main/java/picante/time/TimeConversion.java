package picante.time;

import picante.spice.kernel.tk.lsk.LSK;
import picante.spice.kernelpool.parser.time.TimeParser;
import picante.units.FundamentalPhysicalConstants;
import java.time.Instant;
import java.time.Month;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.Locale;
import java.util.function.Function;

/**
 * Class to convert between Julian Date (UTC), UTC, TAI, TDB, TDT, GPS, and {@link Instant}.
 *
 * @author Hari.Nair@jhuapl.edu
 */
public class TimeConversion {
  private final TimeSystems timeSystems;

  public TimeSystems getTimeSystems() {
    return timeSystems;
  }

  private static TimeConversion fromInternalConstants = null;

  /**
   * @return instance using built in constants. Equivalent to calling {@link
   *     #TimeConversion(TimeSystems)} with {@link TimeSystems#createUsingInternalConstants()}.
   */
  public static TimeConversion createUsingInternalConstants() {
    if (fromInternalConstants == null)
      fromInternalConstants = new TimeConversion(TimeSystems.createUsingInternalConstants());
    return fromInternalConstants;
  }

  /**
   * Get an instance using a leap second kernel
   *
   * @param lsk Leap second kernel
   */
  public TimeConversion(LSK lsk) {
    TimeSystems.Builder builder = TimeSystems.builder();
    lsk.configure(builder);
    this.timeSystems = builder.build();
  }

  /**
   * @param timeSystems To get an instance using built in constants, call this with {@link
   *     TimeSystems#createUsingInternalConstants()}.
   */
  public TimeConversion(TimeSystems timeSystems) {
    this.timeSystems = timeSystems;
  }

  /**
   *
   *
   * <pre>
   * String format = "YYYY/Mon/DD HR:MN:SC.### (TDB) ::TDB";
   * TimeConversion tc = new TimeConversion(TimeSystems.createUsingInternalConstants());
   * String formattedString = tc.format(format).apply(tdb);
   * </pre>
   *
   * @param pattern If one of the following, return a string representing the time in the desired
   *     format:
   *     <table border="1">
   *        <tr>
   *        <td>FORMAT</td>
   *        <td>OUTPUT STRING</td>
   *        <td>EXAMPLE</td>
   *        </tr>
   *        <tr>
   *        <td>C</td>
   *        <td>Calendar format, UTC</td>
   *        <td>1986 APR 12 16:31:09.814</td>
   *        </tr>
   *        <tr>
   *        <td>D</td>
   *        <td>Day-of-Year format, UTC</td>
   *        <td>1986-102 // 16:31:12.814</td>
   *        </tr>
   *        <tr>
   *        <td>J</td>
   *        <td>year component of time</td>
   *        <td>JD 2446533.18834276</td>
   *        </tr>
   *        <tr>
   *        <td>ISOC</td>
   *        <td>ISO Calendar format, UTC</td>
   *        <td>1986-04-12T16:31:12.814</td>
   *        </tr>
   *        <tr>
   *        <td>ISOD</td>
   *        <td>ISO Day-of-Year format, UTC</td>
   *        <td>1986-102T16:31:12.814</td>
   *        </tr>
   *        </table>
   *     <p>Otherwise, markers in format string will be replaced according to the following table:
   *     <table border="1">
   *        <tr>
   *        <td>MARKER</td>
   *        <td>MEANING</td>
   *        </tr>
   *        <tr>
   *        <td>YYYY</td>
   *        <td>year component of time</td>
   *        </tr>
   *        <tr>
   *        <td>YR</td>
   *        <td>last two digits of year component of time</td>
   *        </tr>
   *        <tr>
   *        <td>MM</td>
   *        <td>numeric representation of month component</td>
   *        </tr>
   *        <tr>
   *        <td>MON</td>
   *        <td>upper case three letter abbreviation for month</td>
   *        </tr>
   *        <tr>
   *        <td>Mon</td>
   *        <td>capitalized three letter abbreviation for month</td>
   *        </tr>
   *        <tr>
   *        <td>DD</td>
   *        <td>Day of month</td>
   *        </tr>
   *        <tr>
   *        <td>DOY</td>
   *        <td>Day of year</td>
   *        </tr>
   *        <tr>
   *        <td>HR</td>
   *        <td>hour component of time</td>
   *        </tr>
   *        <tr>
   *        <td>MN</td>
   *        <td>minute component of time</td>
   *        </tr>
   *        <tr>
   *        <td>SC.###</td>
   *        <td>seconds component of time with three digit millisecond</td>
   *        </tr>
   *        <tr>
   *        <td>SC.######</td>
   *        <td>seconds component of time with six digit microsecond</td>
   *        </tr>
   *        <tr>
   *        <td>SC</td>
   *        <td>seconds component of time</td>
   *        </tr>
   *        </table>
   *     <p>You may include "::TDB" or "::TDT" to output the formatted string as TDB or TDT. The
   *     default is to convert to UTC before output.
   * @return A {@link Function} that can be used to represent the time by a string.
   */
  public Function<Double, String> format(String pattern) {

    boolean tdbOutput = false;
    boolean tdtOutput = false;
    boolean round = false;

    pattern = pattern.trim();

      switch (pattern) {
          case "C" -> pattern = "YYYY MON DD HR:MN:SC.### ::RND";
          case "D" -> pattern = "YYYY-DOY // HR:MN:SC.### ::RND";
          case "J" -> pattern = "JD JULIAND.### ::RND";
          case "ISOC" -> pattern = "YYYY-MM-DDTHR:MN:SC.### ::RND";
          case "ISOD" -> pattern = "YYYY-DOYTHR:MN:SC.### ::RND";
      }

    if (pattern.contains("::RND")) {
      round = true;
      pattern = pattern.replace("::RND", "");
    }

    if (pattern.contains("::TDB")) {
      tdbOutput = true;
      pattern = pattern.replace("::TDB", "");
    }

    if (pattern.contains("::TDT")) {
      tdtOutput = true;
      pattern = pattern.replace("::TDT", "");
    }

    if (pattern.contains("::TT")) {
      tdtOutput = true;
      pattern = pattern.replace("::TT", "");
    }

    final String finalPattern = pattern.trim();
    final boolean finalTDB = tdbOutput;
    final boolean finalTDT = tdtOutput;
    final boolean finalRound = round;

    return new Function<>() {

      @Override
      public String apply(Double tdb) {
        UTCEpoch utc;
        if (finalTDB || finalTDT) {
          if (finalTDT) tdb = tdbToTDT(tdb);

          // this is the Julian Ephemeris Date
          double jed =
              tdb / FundamentalPhysicalConstants.SECONDS_PER_DAY
                  + FundamentalPhysicalConstants.JULIAN_DATE_OF_J2000;

          // this is not really UTC but is convenient when we want to represent TDB in a calendar
          // format
          utc = new JulianDate(jed).toUTCEpoch();
        } else {
          utc = tdbToUTC(tdb);
        }

        String format = finalPattern;

        if (format.contains("JULIAND")) {
          double jd = JulianDate.fromUTCEpoch(utc).getDate();
          if (format.contains("JULIAND.######")) {
            if (finalRound) jd = Math.round(jd * 1e6) / 1e6;
            format = format.replaceAll("JULIAND.######", String.format("%.6f", jd));
          } else if (format.contains("JULIAND.###")) {
            if (finalRound) jd = Math.round(jd * 1e3) / 1e3;
            format = format.replaceAll("JULIAND.###", String.format("%.3f", jd));
          } else {
            if (finalRound) jd = Math.round(jd);
            format = format.replaceAll("JULIAND", String.format("%.0f", jd));
          }
        } else {
          if (finalRound) utc = utc.createValueRoundedToMicrosecs();

          if (format.contains("SC.######")) {
            format =
                format.replaceAll(
                    "SC.######",
                    String.format(
                        "%02d.%06d",
                        (int) utc.getSec(), (int) (1e6 * (utc.getSec() - (int) utc.getSec()))));
          } else {
            if (finalRound) utc = utc.createValueRoundedToMillisecs();
            if (format.contains("SC.###")) {
              format =
                  format.replaceAll(
                      "SC.###",
                      String.format(
                          "%02d.%03d",
                          (int) utc.getSec(), (int) (1e3 * (utc.getSec() - (int) utc.getSec()))));
            } else {
              if (finalRound) utc = utc.createValueRoundedToSeconds();
              if (format.contains("SC")) {
                format = format.replaceAll("SC", String.format("%02d", (int) utc.getSec()));
              } else {
                if (finalRound) utc = utc.createValueRoundedToMinutes();
              }
            }
          }

          format = format.replaceAll("YYYY", String.format("%04d", utc.getYear()));
          format = format.replaceAll("YR", String.format("%02d", utc.getYear() % 100));
          format = format.replaceAll("MM", String.format("%02d", utc.getMonth()));
          format =
              format.replaceAll(
                  "MON", Month.of(utc.getMonth()).name().substring(0, 3).toUpperCase());
          format =
              format.replaceAll(
                  "Mon",
                  Month.of(utc.getMonth()).name().substring(0, 1).toUpperCase()
                      + Month.of(utc.getMonth()).name().substring(1, 3).toLowerCase());
          format =
              format.replaceAll(
                  "mon", Month.of(utc.getMonth()).name().substring(0, 3).toLowerCase());
          format = format.replaceAll("DD", String.format("%02d", utc.getDom()));
          format = format.replaceAll("DOY", String.format("%03d", utc.getDoy()));
          format = format.replaceAll("HR", String.format("%02d", utc.getHour()));
          format = format.replaceAll("MN", String.format("%02d", utc.getMin()));
        }

        return format.trim();
      }
    };
  }

  /**
   * Convert TDB seconds to Julian date (UTC). To convert to a Julian Ephemeris Date instead: <br>
   *
   * <pre>
   * JED = J2000 + TDB / 86400
   * </pre>
   *
   * @param tdb TDB seconds
   * @return Julian date
   */
  public JulianDate tdbToJulian(double tdb) {
    UTCEpoch utc = tdbToUTC(tdb);
    return JulianDate.fromUTCEpoch(utc);
  }

  /**
   * Convert a Julian Date (UTC) to TDB. To convert a Julian Ephemeris Date instead: <br>
   *
   * <pre>
   * TDB = (JED - J2000) * 86400
   * </pre>
   *
   * @param jd Julian Date (UTC)
   * @return TDB seconds
   */
  public double julianToTDB(JulianDate jd) {
    return utcToTDB(jd.toUTCEpoch());
  }

  /**
   * @param tdb TDB seconds
   * @return corresponding {@link Instant}
   */
  public Instant tdbToInstant(double tdb) {
    UTCEpoch utc = tdbToUTC(tdb);
    int sec = (int) utc.getSec();
    int nanos = (int) (1e9 * (utc.getSec() - sec));
    OffsetDateTime odt =
        OffsetDateTime.of(
            utc.getYear(),
            utc.getMonth(),
            utc.getDom(),
            utc.getHour(),
            utc.getMin(),
            sec,
            nanos,
            ZoneOffset.UTC);

    return Instant.from(odt);
  }

  /**
   * @param instant instant
   * @return TDB seconds
   */
  public double instantToTDB(Instant instant) {
    DateTimeFormatter formatter =
        DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSSSSSSSS")
            .withLocale(Locale.getDefault())
            .withZone(ZoneOffset.UTC);
    return utcStringToTDB(formatter.format(instant));
  }

  /**
   * Convert a UTC String to TDB (Barycentric Dynamical Time)
   *
   * @param utcString UTC string
   * @return TDB seconds
   */
  public double utcStringToTDB(String utcString) {
    UTCEpoch utc = UTCStringParser.createUTCEpoch(utcString);
    return utcToTDB(utc);
  }

  /**
   * @param utc UTC object
   * @return TDB seconds
   */
  public double utcToTDB(UTCEpoch utc) {
    return timeSystems.getTDB().getTime(timeSystems.getUTC().getTSEpoch(utc));
  }

  /**
   * @param tdb TDB seconds
   * @return UTC object
   */
  public UTCEpoch tdbToUTC(double tdb) {
    return timeSystems.getUTC().getTime(timeSystems.getTDB().getTSEpoch(tdb));
  }

  /**
   * Convert TAI (International Atomic Time) to TDB (Barycentric Dynamical Time)
   *
   * @param tai TAI seconds
   * @return TDB seconds
   */
  public double taiToTDB(double tai) {
    return uniformTimeConverter(timeSystems.getTAI(), timeSystems.getTDB(), tai);
  }

  /**
   * Convert TDB (Barycentric Dynamical Time) to TAI (International Atomic Time)
   *
   * @param tdb TDB seconds
   * @return TAI seconds
   */
  public double tdbToTAI(double tdb) {
    return uniformTimeConverter(timeSystems.getTDB(), timeSystems.getTAI(), tdb);
  }

  /**
   * Convert TDB (Barycentric Dynamical Time) to TDT (Terrestrial Dynamical Time, also known as TT)
   *
   * @param tdb TDB seconds
   * @return TDT seconds
   */
  public double tdbToTDT(double tdb) {
    return uniformTimeConverter(timeSystems.getTDB(), timeSystems.getTDT(), tdb);
  }

  /**
   * Convert TDT (Terrestrial Dynamical Time, also known as TT) to TDB (Barycentric Dynamical Time)
   *
   * @param tdt TDT seconds
   * @return TDB seconds
   */
  public double tdtToTDB(double tdt) {
    return uniformTimeConverter(timeSystems.getTDT(), timeSystems.getTDB(), tdt);
  }

  /**
   * Convert TDB (Barycentric Dynamical Time) to GPS (Global Positioning System Time)
   *
   * @param tdb TDB seconds
   * @return GPS seconds
   */
  public double tdbToGPS(double tdb) {
    return uniformTimeConverter(timeSystems.getTDB(), timeSystems.getGPS(), tdb);
  }

  /**
   * Convert TDB (Barycentric Dynamical Time) to GPS (Global Positioning System Time)
   *
   * @param gps GPS seconds
   * @return TDB seconds
   */
  public double gpsToTDB(double gps) {
    return uniformTimeConverter(timeSystems.getGPS(), timeSystems.getTDB(), gps);
  }

  private double uniformTimeConverter(
      TimeSystem<Double> from, TimeSystem<Double> to, double fromTime) {
    return to.getTime(from.getTSEpoch(fromTime));
  }

  /**
   * Convert a tdb time to a UTC string. Similar to NAIF's et2utc() function. Use {@link
   * #format(String)} for a more flexible output format.
   *
   * @param tdb TDB seconds
   * @param format can be C, D, J, ISOC, or ISOD<br>
   *     Examples:<br>
   *     "C" "1987 APR 12 16:31:12.814"<br>
   *     "D" "1979-114 // 14:19:57.184"<br>
   *     "J" "JD 2446533.18834276"<br>
   *     "ISOC" "1987-04-12T16:31:12.814"<br>
   *     "ISOD" "1987-102T16:31:12.814"
   * @return UTC string
   */
  public String tdbToUTCString(double tdb, String format) {
    UTCEpoch utc = tdbToUTC(tdb).createValueRoundedToMillisecs();
    return utcEpochToFormattedString(utc, format);
  }

  /**
   * Convert a tdb time to a TDB string. Similar to NAIF's etcal() function.
   *
   * @param tdb TDB seconds
   * @param format can be C, D, J, ISOC, or ISOD<br>
   *     Examples:<br>
   *     "C" "1987 APR 12 16:31:12.814"<br>
   *     "D" "1979-114 // 14:19:57.184"<br>
   *     "J" "JD 2446533.18834276"<br>
   *     "ISOC" "1987-04-12T16:31:12.814"<br>
   *     "ISOD" "1987-102T16:31:12.814"
   * @return TDB string
   */
  public String tdbToTDBString(double tdb, String format) {
    // this is the Julian Ephemeris Date
    double jed =
        tdb / FundamentalPhysicalConstants.SECONDS_PER_DAY
            + FundamentalPhysicalConstants.JULIAN_DATE_OF_J2000;

    // this is not really UTC but is convenient when we want to represent TDB in a calendar format
    UTCEpoch utc = new JulianDate(jed).toUTCEpoch();
    return utcEpochToFormattedString(utc, format);
  }

  /**
   * Parses a calendar string in TDB and returns seconds past J2000
   *
   * @param tdbString TDB calendar string
   * @return TDB seconds
   */
  public double tdbStringToTDB(String tdbString) {
    tdbString = tdbString.replace("TDB", "");
    TimeParser parser = new TimeParser();
    double secondsPastJ2000;

    try {
      secondsPastJ2000 = parser.tparse(tdbString.trim());
    } catch (picante.spice.kernelpool.parser.time.ParseException e) {
      throw new InvalidUTCStringException(tdbString);
    }
    return secondsPastJ2000;
  }

  /**
   * Even though it says UTCEpoch, this is used for TDB calendar dates too
   *
   * @param utc UTC object
   * @param format can be C, D, J, ISOC, or ISOD<br>
   *     Examples:<br>
   *     "C" "1987 APR 12 16:31:12.814"<br>
   *     "D" "1979-114 // 14:19:57.184"<br>
   *     "J" "JD 2446533.18834276"<br>
   *     "ISOC" "1987-04-12T16:31:12.814"<br>
   *     "ISOD" "1987-102T16:31:12.814"
   * @return formatted string
   */
  private String utcEpochToFormattedString(UTCEpoch utc, String format) {
    utc = utc.createValueRoundedToMillisecs();

    if (format.equalsIgnoreCase("ISOC")) {

      if (utc.getYear() < 1)
        throw new RuntimeException("Years before 1 A.D. are not supported in ISO format.");

      return String.format(
          "%d-%02d-%02dT%02d:%02d:%06.3f",
          utc.getYear(), utc.getMonth(), utc.getDom(), utc.getHour(), utc.getMin(), utc.getSec());

    } else if (format.equalsIgnoreCase("ISOD")) {

      if (utc.getYear() < 1)
        throw new RuntimeException("Years before 1 A.D. are not supported in ISO format.");

      return String.format(
          "%d-%03dT%02d:%02d:%06.3f",
         utc.getYear(), utc.getDoy(), utc.getHour(), utc.getMin(), utc.getSec());

    } else if (format.equalsIgnoreCase("C")) {

      String yearString;
      if (utc.getYear() < 1)
        yearString = String.format("%d B.C.", Math.abs(utc.getYear()) + 1);
      else if (utc.getYear() < 1000)
        yearString = String.format("%d A.D.", utc.getYear());
      else yearString=String.format("%d", utc.getYear());

      return String.format(
          "%s %3s %02d %02d:%02d:%06.3f",
          yearString,
          Month.of(utc.getMonth()).name().substring(0, 3),
          utc.getDom(),
          utc.getHour(),
          utc.getMin(),
          utc.getSec());

    } else if (format.equalsIgnoreCase("D")) {

      String yearString;
      if (utc.getYear() < 1)
        yearString = String.format("%d B.C. ", Math.abs(utc.getYear()) + 1);
      else if (utc.getYear() < 1000)
        yearString = String.format("%d A.D. ", utc.getYear());
      else yearString=String.format("%d-", utc.getYear());

      return String.format(
          "%s%03d // %02d:%02d:%06.3f",
          yearString, utc.getDoy(), utc.getHour(), utc.getMin(), utc.getSec());

    } else if (format.equalsIgnoreCase("J")) {
      JulianDate jd = JulianDate.fromUTCEpoch(utc);
      return String.format("JD %.8f", jd.getDate());
    }

    return null;
  }
}
