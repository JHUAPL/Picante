package picante.time;

import static org.junit.Assert.assertEquals;

import java.util.LinkedHashMap;
import java.util.Map;
import org.junit.Test;

/**
 * Formats tested are from <a
 * href="https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/spicelib/tparse.html">TPARSE</a>
 * documentation.
 *
 * <p>Examples that TPARSE accepts that this does not:
 *
 * <ul>
 *   <li>1992 11:18:28 3 Jul
 *   <li>13:28:28.128 1992 27 Jun
 *   <li>1992 183// 12 18 19
 *   <li>17:28:01.287 1992-272//
 *   <li>17:28:01.282 272-1994//
 *   <li>2451515.2981 (JD)
 * </ul>
 *
 * @author Hari.Nair@jhuapl.edu
 */
public class UTCStringParserTest {

  @Test
  public void testCreateUTCEpoch() {
    Map<String, UTCEpoch> dates = new LinkedHashMap<>();

    // ISO (T) formats
    dates.put("1987-04-12T16:31:12.814", new UTCEpoch(1987, 102, 16, 31, 12.814));
    dates.put("1987-102T16:31:12.814", new UTCEpoch(1987, 102, 16, 31, 12.814));
    dates.put("1996-12-18T12:28:28", new UTCEpoch(1996, 353, 12, 28, 28.));
    dates.put("1986-01-18T12", new UTCEpoch(1986, 18, 12, 0, 0.));
    dates.put("1986-01-18T12:19", new UTCEpoch(1986, 18, 12, 19, 0.));
    dates.put("1986-01-18T12:19:52.18", new UTCEpoch(1986, 18, 12, 19, 52.18));
    dates.put("1995-18T18:28:12", new UTCEpoch(1995, 18, 18, 28, 12.));
    dates.put("1995-18T", new UTCEpoch(1995, 18, 0, 0, 0.));

    // Calendar formats
    dates.put("1987 APR 12 16:31:12.814", new UTCEpoch(1987, 102, 16, 31, 12.814));
    dates.put("1582 OCT 1 16:31:12.814", new UTCEpoch(1582, 274, 16, 31, 12.814));
    dates.put("Tue Aug 6 11:10:57 1996", new UTCEpoch(1996, 219, 11, 10, 57.));
    dates.put("1 DEC 1997 12:28:29.192", new UTCEpoch(1997, 335, 12, 28, 29.192));
    dates.put("2/3/1996 17:18:12.002", new UTCEpoch(1996, 34, 17, 18, 12.002));
    dates.put("Mar 2 12:18:17.287 1993", new UTCEpoch(1993, 61, 12, 18, 17.287));
    dates.put("1992 11:18:28 3 Jul", new UTCEpoch(1992, 185, 11, 18, 28.));
    dates.put("June 12, 1989 01:21 ", new UTCEpoch(1989, 163, 1, 21, 0.));
    dates.put("1978/3/12 23:28:59.29", new UTCEpoch(1978, 71, 23, 28, 59.29));
    dates.put("17JUN1982 18:28:28 ", new UTCEpoch(1982, 168, 18, 28, 28));
    dates.put("13:28:28.128 1992 27 Jun", new UTCEpoch(1992, 179, 13, 28, 28.128));
    dates.put("1972 27 jun 12:29", new UTCEpoch(1972, 179, 12, 29, 0.));
    dates.put("'93 Jan 23 12:29:47.289", new UTCEpoch(1993, 23, 12, 29, 47.289));
    dates.put("27 Jan 3, 19:12:28.182", new UTCEpoch(2027, 3, 19, 12, 28.182));
    dates.put("23 A.D. APR 4, 18:28:29.29", new UTCEpoch(23, 94, 18, 28, 29.29));
    dates.put("18 B.C. Jun 3, 12:29:28.291", new UTCEpoch(-17, 154, 12, 29, 28.291));
    dates.put("29 Jun  30 12:29:29.298", new UTCEpoch(2029, 181, 12, 29, 29.298));
    dates.put("29 Jun  '30 12:29:29.298", new UTCEpoch(2030, 180, 12, 29, 29.298));

    // Day of year
    dates.put("1986-102 // 16:31:12.814", new UTCEpoch(1986, 102, 16, 31, 12.814));
    dates.put("1997-162::12:18:28.827", new UTCEpoch(1997, 162, 12, 18, 28.827));
    dates.put("162-1996/12:28:28.287   ", new UTCEpoch(1996, 162, 12, 28, 28.287));
    dates.put(" 1993-321/12:28:28.287    ", new UTCEpoch(1993, 321, 12, 28, 28.287));
    dates.put("1992 183// 12 18 19", new UTCEpoch(1992, 183, 12, 18, 19));
    dates.put("17:28:01.287 1992-272//", new UTCEpoch(1992, 272, 17, 28, 1.287));
    dates.put("17:28:01.282 272-1994//", new UTCEpoch(1994, 272, 17, 28, 1.282));
    dates.put("'92-271/ 12:28:30.291", new UTCEpoch(1992, 271, 12, 28, 30.291));
    dates.put("92-182/ 18:28:28.281", new UTCEpoch(1992, 182, 18, 28, 28.281));
    dates.put("182-92/ 12:29:29.192", new UTCEpoch(182, 92, 12, 29, 29.192));
    dates.put("182-'92/ 12:28:29.182", new UTCEpoch(1992, 182, 12, 28, 29.182));

    // Julian date
    dates.put("jd 28272.291", new UTCEpoch(-4635, 111, 18, 59, 02.400));
    dates.put("2451515.2981 (JD)", new UTCEpoch(1999, 336, 19, 9, 15.840));
    dates.put("2451515.2981 JD", new UTCEpoch(1999, 336, 19, 9, 15.840));
    dates.put("JD 2436116.31", new UTCEpoch(1957, 277, 19, 26, 24.));

    //    System.out.printf("%24s %24s\n", "Date String", "UTC");
    for (String date : dates.keySet()) {
      UTCEpoch utc = UTCStringParser.createUTCEpoch(date).createValueRoundedToMillisecs();

      //      System.out.printf("%-24s %24s\n", date, utc.toString());

      assertEquals(utc, dates.get(date).createValueRoundedToMillisecs());
    }
  }
}
