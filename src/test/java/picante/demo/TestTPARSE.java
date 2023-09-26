package picante.demo;

import static org.junit.Assert.*;
import static picante.junit.AssertTools.assertRelativeEquality;

import java.util.List;
import org.junit.Test;
import picante.spice.kernelpool.parser.time.ParseException;
import picante.spice.kernelpool.parser.time.TimeParser;

/**
 * Parses an input time string and returns the number of seconds past the J2000 epoch on a formal
 * calendar.
 *
 * <p>Based on examples shown at <a href=
 * "https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/IDL/icy/cspice_tparse.html">CSPICE_TPARSE</a>
 */
public class TestTPARSE {

  private static final double DELTA = 1e-12;

  @Test
  public void testExample() {
    TimeParser parser = new TimeParser();
    try {
      assertRelativeEquality(-440293207.820000, parser.tparse("1986-01-18T12:19:52.18"), DELTA);
      assertRelativeEquality(-553541492.000000, parser.tparse("17JUN1982 18:28:28"), DELTA);
      assertRelativeEquality(-236820690.818000, parser.tparse("182-'92/ 12:28:29.182"), DELTA);
      assertRelativeEquality(2137710510.291000, parser.tparse("'67-271/ 12:28:30.291"), DELTA);
      assertRelativeEquality(-215265600.000000, parser.tparse("1993 FEB 35"), DELTA);
      assertRelativeEquality(-215265600.000000, parser.tparse("1993 MAR 7"), DELTA);
    } catch (ParseException e) {
      throw new RuntimeException(e);
    }
  }

  /** ISO (T) Formats */
  @Test
  public void testISO() {
    TimeParser parser = new TimeParser();
    try {
      assertRelativeEquality(-9.5815892000000000E+07, parser.tparse("1996-12-18T12:28:28"), DELTA);
      assertRelativeEquality(-4.4029440000000000E+08, parser.tparse("1986-01-18T12"), DELTA);
      assertRelativeEquality(-4.4029326000000000E+08, parser.tparse("1986-01-18T12:19"), DELTA);
      assertRelativeEquality(
          -4.4029320781999999E+08, parser.tparse("1986-01-18T12:19:52.18"), DELTA);
      assertRelativeEquality(
          -4.4029320781999999E+08, parser.tparse("1986-01-18T12:19:52.18Z"), DELTA);
      assertRelativeEquality(-1.5713830800000000E+08, parser.tparse("1995-08T18:28:12"), DELTA);
      assertRelativeEquality(-1.5713830800000000E+08, parser.tparse("1995-08T18:28:12Z"), DELTA);
      assertRelativeEquality(-1.5634080000000000E+08, parser.tparse("1995-18T"), DELTA);
      assertRelativeEquality(-4.3200000000000000E+04, parser.tparse("0000-01-01T"), DELTA);
    } catch (ParseException e) {
      throw new RuntimeException(e);
    }
  }

  /** Calendar formats */
  @Test
  public void testCalendar() {
    TimeParser parser = new TimeParser();
    try {
      assertRelativeEquality(
          -1.0739814300000000E+08, parser.tparse("Tue Aug  6 11:10:57  1996"), DELTA);
      assertRelativeEquality(
          -6.5748690807999998E+07, parser.tparse("1 DEC 1997 12:28:29.192"), DELTA);
      assertRelativeEquality(
          -1.2336010799800000E+08, parser.tparse("2/3/1996 17:18:12.002"), DELTA);
      assertRelativeEquality(
          -2.1565330271300000E+08, parser.tparse("Mar 2 12:18:17.287 1993"), DELTA);
      assertRelativeEquality(-2.3656569200000000E+08, parser.tparse("1992 11:18:28  3 Jul"), DELTA);
      assertRelativeEquality(-3.3311034000000000E+08, parser.tparse("June 12, 1989 01:21"), DELTA);
      assertRelativeEquality(
          -6.8813466071000004E+08, parser.tparse("1978/3/12 23:28:59.29"), DELTA);
      assertRelativeEquality(-5.5354149200000000E+08, parser.tparse("17JUN1982 18:28:28"), DELTA);
      assertRelativeEquality(
          -2.3707629187200001E+08, parser.tparse("13:28:28.128 1992 27 Jun"), DELTA);
      assertRelativeEquality(-8.6823186000000000E+08, parser.tparse("1972 27 jun 12:29"), DELTA);
      assertRelativeEquality(
          -2.1893581271100000E+08, parser.tparse("'93 Jan 23 12:29:47.289"), DELTA);
      assertRelativeEquality(
          8.5227554818200004E+08, parser.tparse("27 Jan 3, 19:12:28.182"), DELTA);
      assertRelativeEquality(
          -6.2379999090709999E+10, parser.tparse("23 A.D. APR 4, 18:28:29.29"), DELTA);
      assertRelativeEquality(
          -6.3637140631709000E+10, parser.tparse("18 B.C. Jun 3, 12:29:28.291"), DELTA);
      assertRelativeEquality(
          9.3078896929799998E+08, parser.tparse("29 Jun  30 12:29:29.298"), DELTA);
      assertRelativeEquality(
          9.6223856929799998E+08, parser.tparse("29 Jun '30 12:29:29.298"), DELTA);
    } catch (ParseException e) {
      throw new RuntimeException(e);
    }
  }

  /** Day of Year formats */
  @Test
  public void testDayOfYear() {
    TimeParser parser = new TimeParser();
    try {
      assertRelativeEquality(
          -8.0696491172999993E+07, parser.tparse("1997-162::12:18:28.827"), DELTA);
      assertRelativeEquality(
          -1.1231829171300000E+08, parser.tparse("162-1996/12:28:28.287"), DELTA);
      assertRelativeEquality(
          -1.9318869171300000E+08, parser.tparse("1993-321/12:28:28.287"), DELTA);
      assertRelativeEquality(-2.3673490100000000E+08, parser.tparse("1992 183// 12:18:19"), DELTA);
      assertRelativeEquality(
          -2.2902671871300000E+08, parser.tparse("17:28:01.287 1992-272//"), DELTA);
      assertRelativeEquality(
          -1.6586831871799999E+08, parser.tparse("17:28:01.282 272-1994//"), DELTA);
      assertRelativeEquality(
          -2.2913108970899999E+08, parser.tparse("'92-271/ 12:28:30.291"), DELTA);
      assertRelativeEquality(-2.3679909171900001E+08, parser.tparse("92-182/ 18:28:28.281"), DELTA);
      assertRelativeEquality(-5.7362599830807999E+10, parser.tparse("182-92/ 12:29:29.192"), DELTA);
      assertRelativeEquality(
          -2.3682069081799999E+08, parser.tparse("182-'92/ 12:28:29.182"), DELTA);
    } catch (ParseException e) {
      throw new RuntimeException(e);
    }
  }

  @Test
  public void testJulianDate() {
    TimeParser parser = new TimeParser();
    try {
      assertRelativeEquality(-2.0937076205759998E+11, parser.tparse("jd 28272.291"), DELTA);
      assertRelativeEquality(-2.5662441600188613E+06, parser.tparse("2451515.2981 (JD)"), DELTA);
      assertRelativeEquality(-2.5662441600188613E+06, parser.tparse("2451515.2981 JD"), DELTA);
    } catch (ParseException e) {
      throw new RuntimeException(e);
    }
  }

  @Test
  public void testLeapYears() {
    TimeParser parser = new TimeParser();
    try {
      assertRelativeEquality(
          -6.3140342400000000E+10, parser.tparse("2 B.C. March 1 12:00:00"), DELTA);
      assertRelativeEquality(
          -6.3108720000000000E+10, parser.tparse("1 B.C. March 1 12:00:00"), DELTA);
      assertRelativeEquality(
          -6.3077184000000000E+10, parser.tparse("1 A.D. March 1 12:00:00"), DELTA);
      assertRelativeEquality(
          -6.3045648000000000E+10, parser.tparse("2 A.D. March 1 12:00:00"), DELTA);
      assertRelativeEquality(
          -6.3014112000000000E+10, parser.tparse("3 A.D. March 1 12:00:00"), DELTA);
      assertRelativeEquality(
          -6.2982489600000000E+10, parser.tparse("4 A.D. March 1 12:00:00"), DELTA);
      assertRelativeEquality(
          -6.2950953600000000E+10, parser.tparse("5 A.D. March 1 12:00:00"), DELTA);
    } catch (ParseException e) {
      throw new RuntimeException(e);
    }
  }

  @Test
  public void testErrors() {
    TimeParser parser = new TimeParser();

    // TimeParser fails on these, same as NAIF
    List<String> utcStrings = List.of("-467-14-25 26:00:75");
    for (String utcString : utcStrings) {
      try {
        parser.tparse(utcString);
        fail();
      } catch (Throwable t) {
        //        System.err.println("Expected error: Cannot parse \"" + utcString + "\"");
        assertTrue(true);
      }
    }
  }
}
