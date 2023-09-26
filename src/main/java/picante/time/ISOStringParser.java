package picante.time;

import java.text.ParseException;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import com.google.common.base.CharMatcher;
import com.google.common.base.Splitter;


public class ISOStringParser implements ITimeStringParser {



  private static final Pattern isoPattern = Pattern
      .compile("(\\d\\d\\d\\d)\\-(\\d\\d)\\-(\\d\\d)T(\\d\\d)\\:(\\d\\d)\\:(\\d\\d\\.*\\d*)(Z*)");
  private static final Pattern isoPatternNoPunctuation =
      Pattern.compile("(\\d\\d\\d\\d)(\\d\\d)(\\d\\d)T(\\d\\d)(\\d\\d)(\\d\\d\\.*\\d*)(Z*)");
  private static final Pattern isoDOYPattern =
      Pattern.compile("(\\d\\d\\d\\d)\\-(\\d\\d\\d)T(\\d\\d)\\:(\\d\\d)\\:(\\d\\d\\.*\\d*)(Z*)");
  private static final Pattern isoDOYPatternNoPunctuation =
      Pattern.compile("(\\d\\d\\d\\d)(\\d\\d\\d)T(\\d\\d)(\\d\\d)(\\d\\d\\.*\\d*)(Z*)");
  private static final TimeAdapter ta = TimeAdapter.getInstance();
  private static final CharMatcher genericSeparator = CharMatcher.anyOf(" -:TZ");



  @Override
  public TSEpoch createEpoch(String timeString) {
    return getEpochFromString(timeString);
  }

  private static TSEpoch getUTCFromMatcherDOM(Matcher m) {
    int year = Integer.parseInt(m.group(1));
    int month = Integer.parseInt(m.group(2));
    int dom = Integer.parseInt(m.group(3));
    int hour = Integer.parseInt(m.group(4));
    int min = Integer.parseInt(m.group(5));
    double sec = Double.parseDouble(m.group(6));
    UTCEpoch utc = new UTCEpoch(year, month, dom, hour, min, sec);
    return ta.getTSEpochFromUTC(utc);
  }

  private static TSEpoch getUTCFromMatcherDOY(Matcher md) {
    int year = Integer.parseInt(md.group(1));
    int doy = Integer.parseInt(md.group(2));
    int hour = Integer.parseInt(md.group(3));
    int min = Integer.parseInt(md.group(4));
    double sec = Double.parseDouble(md.group(5));
    UTCEpoch utc = new UTCEpoch(year, doy, hour, min, sec);
    return ta.getTSEpochFromUTC(utc);
  }


  public static TSEpoch getEpochFromString(String timeString) {
    String isoString = timeString.trim();
    Matcher m = isoPattern.matcher(isoString);
    Matcher mnp = isoPatternNoPunctuation.matcher(isoString);
    Matcher md = isoDOYPattern.matcher(isoString);
    Matcher mdnp = isoDOYPatternNoPunctuation.matcher(isoString);

    if (m.matches()) {
      return getUTCFromMatcherDOM(m);
    } else if (mnp.matches()) {
      return getUTCFromMatcherDOM(mnp);
    } else if (md.matches()) {
      return getUTCFromMatcherDOY(md);
    } else if (mdnp.matches()) {
      return getUTCFromMatcherDOY(mdnp);
    } else {
      // Try to just split it
      List<String> elements =
          Splitter.on(genericSeparator).trimResults().omitEmptyStrings().splitToList(isoString);
      if (elements.size() == 6) {
        int year = Integer.parseInt(elements.get(0));
        int month = Integer.parseInt(elements.get(1));
        int dom = Integer.parseInt(elements.get(2));
        int hour = Integer.parseInt(elements.get(3));
        int min = Integer.parseInt(elements.get(4));
        double sec = Double.parseDouble(elements.get(5));
        UTCEpoch utc = new UTCEpoch(year, month, dom, hour, min, sec);
        return ta.getTSEpochFromUTC(utc);
      } else if (elements.size() == 5) {
        int year = Integer.parseInt(elements.get(0));
        int doy = Integer.parseInt(elements.get(1));
        int hour = Integer.parseInt(elements.get(2));
        int min = Integer.parseInt(elements.get(3));
        double sec = Double.parseDouble(elements.get(4));
        UTCEpoch utc = new UTCEpoch(year, doy, hour, min, sec);
        return ta.getTSEpochFromUTC(utc);

      } else if (elements.size() == 1) {
        try {
          return ta.getTSEpochFromUTC(UTCStringParser.getUTC(elements.get(0)));
        } catch (ParseException e) {
          throw new RuntimeException("Can't parse: " + timeString + " as an UTC digits string");
        }
      } else {
        throw new RuntimeException("Can't parse: " + timeString + " as an ISODate string");
      }
    }

  }

  public static void main(String args[]) {
    TimeAdapter ta = TimeAdapter.getInstance();
    String monthDay = "2014-07-14T01:02:03";
    String yearDay = "2014-214T01:02:03";
    String monthDayNS = "2014-7-14-1:02:03";
    String yearDayNS = "2014:214:1:2:3";
    String monthDayNP = "20140714T010203";
    String yearDayNP = "2014214T010203";
    TSEpoch me = ISOStringParser.getEpochFromString(monthDay);
    System.out.println(ta.quickString(me));
    TSEpoch mde = ISOStringParser.getEpochFromString(yearDay);
    System.out.println(ta.quickString(mde));
    TSEpoch mens = ISOStringParser.getEpochFromString(monthDayNS);
    System.out.println(ta.quickString(mens));
    TSEpoch mdens = ISOStringParser.getEpochFromString(yearDayNS);
    System.out.println(ta.quickString(mdens));
    TSEpoch mend = ISOStringParser.getEpochFromString(monthDayNP);
    System.out.println(ta.quickString(mend));
    TSEpoch mdend = ISOStringParser.getEpochFromString(yearDayNP);
    System.out.println(ta.quickString(mdend));


  }

}
