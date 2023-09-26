package picante.time;

/**
 * parses time strings in a way commonly used when lanuching the summary plotting mechanism
 *
 * @author brownle1 vandejd1
 */
public class TimeValueParser {
  /**
   * @param utcString a date string with all the digits together: YYYYDDDHHMMSS or YYYYMMDDHHMMSS
   *
   * @return the TSEpoch from the parsed date
   */
  public static TSEpoch getEpochFromString(String utcString) {
    UTCEpoch utc;
    if (utcString.length() == 13) {
      // create UTC from date string
      utc = new UTCEpoch(Integer.parseInt(utcString.substring(0, 4)),
          Integer.parseInt(utcString.substring(4, 7)), Integer.parseInt(utcString.substring(7, 9)),
          Integer.parseInt(utcString.substring(9, 11)),
          Integer.parseInt(utcString.substring(11, 13)));
    } else if (utcString.length() == 14) {
      utc = new UTCEpoch(Integer.parseInt(utcString.substring(0, 4)),
          Integer.parseInt(utcString.substring(4, 6)), Integer.parseInt(utcString.substring(6, 8)),
          Integer.parseInt(utcString.substring(8, 10)),
          Integer.parseInt(utcString.substring(10, 12)),
          Integer.parseInt(utcString.substring(12, 14)));
    } else {
      throw new RuntimeException("Can't parse time string: " + utcString);
    }

    // convert to TSEpoch
    return TimeAdapter.getInstance().getTSEpochFromUTC(utc);
  }

  public static boolean isAnEpochString(String utcString) {
    return (utcString.matches("\\d{14}") || utcString.matches("\\d{13}"));
  }

  public static String makeString(TSEpoch epoch) {
    UTCEpoch utc = TimeAdapter.getInstance().getUTC(epoch);
    return String.format("%04d%03d%02d%02d%02d", utc.getYear(), utc.getDoy(), utc.getHour(),
        utc.getMin(), (int) Math.floor(utc.getSec()));
  }
}
