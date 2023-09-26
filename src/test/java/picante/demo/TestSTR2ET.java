package picante.demo;

import static org.junit.Assert.*;
import static picante.junit.AssertTools.assertRelativeEquality;

import java.util.List;
import org.junit.Test;
import picante.time.JulianDate;
import picante.time.TimeConversion;

/**
 * Converts a string representing an epoch to a double precision value representing the number of
 * TDB seconds past the J2000 epoch corresponding to the input epoch.
 *
 * <p>Based on examples shown at <a href=
 * "https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/IDL/icy/cspice_str2et.html">CSPICE_STR2ET</a>
 */
public class TestSTR2ET {

  private static final double DELTA = 1e-12;

  /** ISO (T) Formats */
  @Test
  public void testISO() {
    TimeConversion tc = TimeConversion.createUsingInternalConstants();
    assertRelativeEquality(
        -9.5815829816449523E+07, tc.utcStringToTDB("1996-12-18T12:28:28"), DELTA);
    assertEquals("1996 DEC 18 12:28:28.000", tc.tdbToUTCString(-9.5815829816449523E+07, "C"));
    assertRelativeEquality(-4.4029434481556684E+08, tc.utcStringToTDB("1986-01-18T12"), DELTA);
    assertEquals("1986 JAN 18 12:00:00.000", tc.tdbToUTCString(-4.4029434481556684E+08, "C"));
    assertRelativeEquality(-4.4029320481556648E+08, tc.utcStringToTDB("1986-01-18T12:19"), DELTA);
    assertEquals("1986 JAN 18 12:19:00.000", tc.tdbToUTCString(-4.4029320481556648E+08, "C"));
    assertRelativeEquality(
        -4.4029315263556647E+08, tc.utcStringToTDB("1986-01-18T12:19:52.18"), DELTA);
    assertEquals("1986 JAN 18 12:19:52.180", tc.tdbToUTCString(-4.4029315263556647E+08, "C"));
    assertRelativeEquality(
        -4.4029315263556647E+08, tc.utcStringToTDB("1986-01-18T12:19:52.18Z"), DELTA);
    assertEquals("1986 JAN 18 12:19:52.180", tc.tdbToUTCString(-4.4029315263556647E+08, "C"));
    assertRelativeEquality(-1.5713824681585363E+08, tc.utcStringToTDB("1995-08T18:28:12"), DELTA);
    assertEquals("1995 JAN 08 18:28:12.000", tc.tdbToUTCString(-1.5713824681585363E+08, "C"));
    assertRelativeEquality(-1.5713824681585363E+08, tc.utcStringToTDB("1995-08T18:28:12Z"), DELTA);
    assertEquals("1995 JAN 08 18:28:12.000", tc.tdbToUTCString(-1.5713824681585363E+08, "C"));
    assertRelativeEquality(-1.5634073881559029E+08, tc.utcStringToTDB("1995-18T"), DELTA);
    assertEquals("1995 JAN 18 00:00:00.000", tc.tdbToUTCString(-1.5634073881559029E+08, "C"));
    assertRelativeEquality(-4.3135816087188054E+04, tc.utcStringToTDB("0000-01-01T"), DELTA);
    assertEquals("2000 JAN 01 00:00:00.000", tc.tdbToUTCString(-4.3135816087188054E+04, "C"));
  }

  /** Calendar formats */
  @Test
  public void testCalendar() {
    TimeConversion tc = TimeConversion.createUsingInternalConstants();
    assertRelativeEquality(
        -1.0739808081687510E+08, tc.utcStringToTDB("Tue Aug  6 11:10:57  1996"), DELTA);
    assertEquals("1996 AUG 06 11:10:57.000", tc.tdbToUTCString(-1.0739808081687510E+08, "C"));
    assertRelativeEquality(
        -6.5748627624902718E+07, tc.utcStringToTDB("1 DEC 1997 12:28:29.192"), DELTA);
    assertEquals("1997 DEC 01 12:28:29.192", tc.tdbToUTCString(-6.5748627624902718E+07, "C"));
    assertRelativeEquality(
        -1.2336004581315179E+08, tc.utcStringToTDB("2/3/1996 17:18:12.002"), DELTA);
    assertEquals("1996 FEB 03 17:18:12.002", tc.tdbToUTCString(-1.2336004581315179E+08, "C"));
    assertRelativeEquality(
        -2.1565324352759039E+08, tc.utcStringToTDB("Mar 2 12:18:17.287 1993"), DELTA);
    assertEquals("1993 MAR 02 12:18:17.287", tc.tdbToUTCString(-2.1565324352759039E+08, "C"));
    assertRelativeEquality(
        -2.3656563281596944E+08, tc.utcStringToTDB("1992 11:18:28  3 Jul"), DELTA);
    assertEquals("1992 JUL 03 11:18:28.000", tc.tdbToUTCString(-2.3656563281596944E+08, "C"));
    assertRelativeEquality(
        -3.3311028381537831E+08, tc.utcStringToTDB("June 12, 1989 01:21"), DELTA);
    assertEquals("1989 JUN 12 01:21:00.000", tc.tdbToUTCString(-3.3311028381537831E+08, "C"));
    assertRelativeEquality(
        -6.8813461152445745E+08, tc.utcStringToTDB("1978/3/12 23:28:59.29"), DELTA);
    assertEquals("1978 MAR 12 23:28:59.290", tc.tdbToUTCString(-6.8813461152445745E+08, "C"));
    assertRelativeEquality(-5.5354143981552470E+08, tc.utcStringToTDB("17JUN1982 18:28:28"), DELTA);
    assertEquals("1982 JUN 17 18:28:28.000", tc.tdbToUTCString(-5.5354143981552470E+08, "C"));
    assertRelativeEquality(
        -2.3707623368780425E+08, tc.utcStringToTDB("13:28:28.128 1992 27 Jun"), DELTA);
    assertEquals("1992 JUN 27 13:28:28.128", tc.tdbToUTCString(-2.3707623368780425E+08, "C"));
    assertRelativeEquality(-8.6823181781580842E+08, tc.utcStringToTDB("1972 27 jun 12:29"), DELTA);
    assertEquals("1972 JUN 27 12:29:00.000", tc.tdbToUTCString(-8.6823181781580842E+08, "C"));
    assertRelativeEquality(
        -2.1893575352642342E+08, tc.utcStringToTDB("'93 Jan 23 12:29:47.289"), DELTA);
    assertEquals("1993 JAN 23 12:29:47.289", tc.tdbToUTCString(-2.1893575352642342E+08, "C"));
    assertRelativeEquality(
        8.5227561736599362E+08, tc.utcStringToTDB("27 Jan 3, 19:12:28.182"), DELTA);
    assertEquals("2027 JAN 03 19:12:28.182", tc.tdbToUTCString(8.5227561736599362E+08, "C"));
    assertRelativeEquality(
        -6.2379999049524628E+10, tc.utcStringToTDB("23 A.D. APR 4, 18:28:29.29"), DELTA);
    assertEquals("23 A.D. APR 04 18:28:29.290", tc.tdbToUTCString(-6.2379999049524628E+10, "C"));
    assertRelativeEquality(
        -6.3637140590525070E+10, tc.utcStringToTDB("18 B.C. Jun 3, 12:29:28.291"), DELTA);
    assertEquals("18 B.C. JUN 03 12:29:28.291", tc.tdbToUTCString(-6.3637140590525070E+10, "C"));
    assertRelativeEquality(
        9.3078903848213017E+08, tc.utcStringToTDB("29 Jun  30 12:29:29.298"), DELTA);
    assertEquals("2029 JUN 30 12:29:29.298", tc.tdbToUTCString(9.3078903848213017E+08, "C"));
    assertRelativeEquality(
        9.6223863848216534E+08, tc.utcStringToTDB("29 Jun '30 12:29:29.298"), DELTA);
    assertEquals("2030 JUN 29 12:29:29.298", tc.tdbToUTCString(9.6223863848216534E+08, "C"));
  }

  /** Day of Year formats */
  @Test
  public void testDayOfYear() {
    TimeConversion tc = TimeConversion.createUsingInternalConstants();
    assertRelativeEquality(
        -8.0696428988362223E+07, tc.utcStringToTDB("1997-162::12:18:28.827"), DELTA);
    assertEquals("1997 JUN 11 12:18:28.827", tc.tdbToUTCString(-8.0696428988362223E+07, "C"));
    assertRelativeEquality(
        -1.1231822952834328E+08, tc.utcStringToTDB("162-1996/12:28:28.287"), DELTA);
    assertEquals("1996 JUN 10 12:28:28.287", tc.tdbToUTCString(-1.1231822952834328E+08, "C"));
    assertRelativeEquality(
        -1.9318863153021085E+08, tc.utcStringToTDB("1993-321/12:28:28.287"), DELTA);
    assertEquals("1993 NOV 17 12:28:28.287", tc.tdbToUTCString(-1.9318863153021085E+08, "C"));
    assertRelativeEquality(
        -2.3673484181591457E+08, tc.utcStringToTDB("1992 183// 12:18:19"), DELTA);
    assertEquals("1992 JUL 01 12:18:19.000", tc.tdbToUTCString(-2.3673484181591457E+08, "C"));
    assertRelativeEquality(
        -2.2902665953064784E+08, tc.utcStringToTDB("17:28:01.287 1992-272//"), DELTA);
    assertEquals("1992 SEP 28 17:28:01.287", tc.tdbToUTCString(-2.2902665953064784E+08, "C"));
    assertRelativeEquality(
        -1.6586825753564921E+08, tc.utcStringToTDB("17:28:01.282 272-1994//"), DELTA);
    assertEquals("1994 SEP 29 17:28:01.282", tc.tdbToUTCString(-1.6586825753564921E+08, "C"));
    assertRelativeEquality(
        -2.2913103052664387E+08, tc.utcStringToTDB("'92-271/ 12:28:30.291"), DELTA);
    assertEquals("1992 SEP 27 12:28:30.291", tc.tdbToUTCString(-2.2913103052664387E+08, "C"));
    assertRelativeEquality(
        -2.3679903353489378E+08, tc.utcStringToTDB("92-182/ 18:28:28.281"), DELTA);
    assertEquals("1992 JUN 30 18:28:28.281", tc.tdbToUTCString(-2.3679903353489378E+08, "C"));
    assertRelativeEquality(
        -5.7362599789622559E+10, tc.utcStringToTDB("182-92/ 12:29:29.192"), DELTA);
    assertEquals("182 A.D. APR 02 12:29:29.192", tc.tdbToUTCString(-5.7362599789622559E+10, "C"));
    assertRelativeEquality(
        -2.3682063263388678E+08, tc.utcStringToTDB("182-'92/ 12:28:29.182"), DELTA);
    assertEquals("1992 JUN 30 12:28:29.182", tc.tdbToUTCString(-2.3682063263388678E+08, "C"));
  }

  @Test
  public void testJulianDate() {
    TimeConversion tc = TimeConversion.createUsingInternalConstants();
    assertRelativeEquality(
        -2.0937076201641705E+11,
        tc.utcStringToTDB("jd 28272.291"),
        DELTA); // 4636 B.C. APR 21 18:59:02.400
    assertRelativeEquality(
        -2.5661799769031643E+06,
        tc.utcStringToTDB("2451515.2981 (JD)"),
        DELTA); // 1999 DEC 02 19:09:15.840
    assertRelativeEquality(
        -2.5661799769031643E+06,
        tc.utcStringToTDB("2451515.2981 JD"),
        DELTA); // 1999 DEC 02 19:09:15.840

    assertRelativeEquality(
        -2.0937076201641705E+11,
        tc.julianToTDB(new JulianDate(28272.291)),
        DELTA); // JD 28272.29100
    assertRelativeEquality(28272.291, tc.tdbToJulian(-2.0937076201641705E+11).getDate(), DELTA);
    assertRelativeEquality(
        -2.5661799769031643E+06,
        tc.julianToTDB(new JulianDate(2451515.2981)),
        DELTA); // JD 2451515.298
    assertRelativeEquality(2451515.2981, tc.tdbToJulian(-2.5661799769031643E+06).getDate(), DELTA);
    assertRelativeEquality(
        -2.5661799769031643E+06,
        tc.julianToTDB(new JulianDate(2451515.2981)),
        DELTA); // JD 2451515.298
    assertRelativeEquality(2451515.2981, tc.tdbToJulian(-2.5661799769031643E+06).getDate(), DELTA);
  }

  @Test
  public void testErrors() {
    TimeConversion tc = TimeConversion.createUsingInternalConstants();

    // Picante fails on this, same as NAIF
    List<String> utcStrings = List.of("'98 Jan 12 13:29:29 A.M.");
    for (String utcString : utcStrings) {
      try {
        tc.utcStringToTDB(utcString);
        fail();
      } catch (Throwable t) {
        //        System.err.println("Expected error: Cannot parse \"" + utcString + "\"");
        assertTrue(true);
      }
    }

    // Picante will parse these strings but NAIF will fail
    utcStrings =
        List.of(
            "1997 Jan 32 12:29:29",
            "1997 Feb 29, 12:29:20.0",
            "1992 Mar 12 12:62:20",
            "1993 Mar 18 15:29:60.5");

    for (String utcString : utcStrings) {
      double tdb = tc.utcStringToTDB(utcString);
      System.out.printf(
          "STR2ET will reject %s, but TimeConversion translates to %s\n",
          utcString, tc.tdbToUTCString(tdb, "ISOC"));
      assertTrue(true);
    }

    utcStrings = List.of("2000 Jan 01 00:00:00 PST", "2000 Jan 01 00:00:00 TDB");
    double[] tdbs = {-14335.816077540216, -43200};
    for (int i = 0; i < utcStrings.size(); i++) {
      try {
        assertEquals(tc.utcStringToTDB(utcStrings.get(i)) / tdbs[i], 1., 1e-12);
      } catch (Throwable t) {
        System.out.printf(
            "TimeConversion fails on \"%s\" but STR2ET will accept it.\n", utcStrings.get(i));
        assertTrue(true);
      }
    }
  }

  @Test
  public void testOthers() {
    TimeConversion tc = TimeConversion.createUsingInternalConstants();
    assertEquals(
        tc.utcStringToTDB("1988 June 13, 12:29:48 A.M.") / -3.6456295581541026E+08,
        1.,
        1e-12); // 1988-06-13T00:29:48.000

    assertEquals(
        tc.utcStringToTDB("1988 June 13, 3:29:48 P.M.") / -3.6450895581542671E+08,
        1.,
        1e-12); // 1988-06-13T15:29:48.000
  }

  @Test
  public void testLeapYears() {
    TimeConversion tc = TimeConversion.createUsingInternalConstants();

    assertEquals("396 B.C. MAR 01 12:00:00.000", tc.tdbToUTCString(-7.5573734358814346E+10, "C"));
    assertEquals("396 B.C. 060 // 12:00:00.000", tc.tdbToUTCString(-7.5573734358814346E+10, "D"));
    assertEquals("JD 1576849.00000000", tc.tdbToUTCString(-7.5573734358814346E+10, "J"));
    assertRelativeEquality(
        -7.5573734358814346E+10, tc.utcStringToTDB("     396 B.C. March 1 12:00:00"), DELTA);
    assertEquals("397 B.C. MAR 01 12:00:00.000", tc.tdbToUTCString(-7.5605270358814346E+10, "C"));
    assertEquals("397 B.C. 061 // 12:00:00.000", tc.tdbToUTCString(-7.5605270358814346E+10, "D"));
    assertEquals("JD 1576484.00000000", tc.tdbToUTCString(-7.5605270358814346E+10, "J"));
    assertRelativeEquality(
        -7.5605270358814346E+10, tc.utcStringToTDB("     397 B.C. March 1 12:00:00"), DELTA);
    assertEquals("398 B.C. MAR 01 12:00:00.000", tc.tdbToUTCString(-7.5636892758814346E+10, "C"));
    assertEquals("398 B.C. 060 // 12:00:00.000", tc.tdbToUTCString(-7.5636892758814346E+10, "D"));
    assertEquals("JD 1576118.00000000", tc.tdbToUTCString(-7.5636892758814346E+10, "J"));
    assertRelativeEquality(
        -7.5636892758814346E+10, tc.utcStringToTDB("     398 B.C. March 1 12:00:00"), DELTA);
    assertEquals("399 B.C. MAR 01 12:00:00.000", tc.tdbToUTCString(-7.5668428758814346E+10, "C"));
    assertEquals("399 B.C. 060 // 12:00:00.000", tc.tdbToUTCString(-7.5668428758814346E+10, "D"));
    assertEquals("JD 1575753.00000000", tc.tdbToUTCString(-7.5668428758814346E+10, "J"));
    assertRelativeEquality(
        -7.5668428758814346E+10, tc.utcStringToTDB("     399 B.C. March 1 12:00:00"), DELTA);
    assertEquals("400 B.C. MAR 01 12:00:00.000", tc.tdbToUTCString(-7.5699964758814346E+10, "C"));
    assertEquals("400 B.C. 060 // 12:00:00.000", tc.tdbToUTCString(-7.5699964758814346E+10, "D"));
    assertEquals("JD 1575388.00000000", tc.tdbToUTCString(-7.5699964758814346E+10, "J"));
    assertRelativeEquality(
        -7.5699964758814346E+10, tc.utcStringToTDB("     400 B.C. March 1 12:00:00"), DELTA);
    assertEquals("401 B.C. MAR 01 12:00:00.000", tc.tdbToUTCString(-7.5731500758814362E+10, "C"));
    assertEquals("401 B.C. 061 // 12:00:00.000", tc.tdbToUTCString(-7.5731500758814362E+10, "D"));
    assertEquals("JD 1575023.00000000", tc.tdbToUTCString(-7.5731500758814362E+10, "J"));
    assertRelativeEquality(
        -7.5731500758814362E+10, tc.utcStringToTDB("     401 B.C. March 1 12:00:00"), DELTA);
    assertEquals("402 B.C. MAR 01 12:00:00.000", tc.tdbToUTCString(-7.5763123158814346E+10, "C"));
    assertEquals("402 B.C. 060 // 12:00:00.000", tc.tdbToUTCString(-7.5763123158814346E+10, "D"));
    assertEquals("JD 1574657.00000000", tc.tdbToUTCString(-7.5763123158814346E+10, "J"));

    assertEquals("2 B.C. MAR 01 12:00:00.000", tc.tdbToUTCString(-6.3140342358814346E+10, "C"));
    assertEquals("2 B.C. 060 // 12:00:00.000", tc.tdbToUTCString(-6.3140342358814346E+10, "D"));
    assertEquals("JD 1720754.00000000", tc.tdbToUTCString(-6.3140342358814346E+10, "J"));
    assertRelativeEquality(
        -6.3140342358814346E+10, tc.utcStringToTDB("       2 B.C. March 1 12:00:00"), DELTA);
    assertEquals("1 B.C. MAR 01 12:00:00.000", tc.tdbToUTCString(-6.3108719958814346E+10, "C"));
    assertEquals("1 B.C. 061 // 12:00:00.000", tc.tdbToUTCString(-6.3108719958814346E+10, "D"));
    assertEquals("JD 1721120.00000000", tc.tdbToUTCString(-6.3108719958814346E+10, "J"));
    assertRelativeEquality(
        -6.3108719958814346E+10, tc.utcStringToTDB("       1 B.C. March 1 12:00:00"), DELTA);
    assertEquals("1 A.D. MAR 01 12:00:00.000", tc.tdbToUTCString(-6.3077183958814346E+10, "C"));
    assertEquals("1 A.D. 060 // 12:00:00.000", tc.tdbToUTCString(-6.3077183958814346E+10, "D"));
    assertEquals("JD 1721485.00000000", tc.tdbToUTCString(-6.3077183958814346E+10, "J"));
    assertRelativeEquality(
        -6.3077183958814346E+10, tc.utcStringToTDB("       1 A.D. March 1 12:00:00"), DELTA);
    assertEquals("2 A.D. MAR 01 12:00:00.000", tc.tdbToUTCString(-6.3045647958814346E+10, "C"));
    assertEquals("2 A.D. 060 // 12:00:00.000", tc.tdbToUTCString(-6.3045647958814346E+10, "D"));
    assertEquals("JD 1721850.00000000", tc.tdbToUTCString(-6.3045647958814346E+10, "J"));
    assertRelativeEquality(
        -6.3045647958814346E+10, tc.utcStringToTDB("       2 A.D. March 1 12:00:00"), DELTA);
    assertEquals("3 A.D. MAR 01 12:00:00.000", tc.tdbToUTCString(-6.3014111958814346E+10, "C"));
    assertEquals("3 A.D. 060 // 12:00:00.000", tc.tdbToUTCString(-6.3014111958814346E+10, "D"));
    assertEquals("JD 1722215.00000000", tc.tdbToUTCString(-6.3014111958814346E+10, "J"));
    assertRelativeEquality(
        -6.3014111958814346E+10, tc.utcStringToTDB("       3 A.D. March 1 12:00:00"), DELTA);
    assertEquals("4 A.D. MAR 01 12:00:00.000", tc.tdbToUTCString(-6.2982489558814346E+10, "C"));
    assertEquals("4 A.D. 061 // 12:00:00.000", tc.tdbToUTCString(-6.2982489558814346E+10, "D"));
    assertEquals("JD 1722581.00000000", tc.tdbToUTCString(-6.2982489558814346E+10, "J"));
    assertRelativeEquality(
        -6.2982489558814346E+10, tc.utcStringToTDB("       4 A.D. March 1 12:00:00"), DELTA);
    assertEquals("5 A.D. MAR 01 12:00:00.000", tc.tdbToUTCString(-6.2950953558814346E+10, "C"));
    assertEquals("5 A.D. 060 // 12:00:00.000", tc.tdbToUTCString(-6.2950953558814346E+10, "D"));
    assertEquals("JD 1722946.00000000", tc.tdbToUTCString(-6.2950953558814346E+10, "J"));
    assertRelativeEquality(
        -6.2950953558814346E+10, tc.utcStringToTDB("       5 A.D. March 1 12:00:00"), DELTA);

    assertEquals("396 A.D. MAR 01 12:00:00.000", tc.tdbToUTCString(-5.0612169558814354E+10, "C"));
    assertEquals("396 A.D. 061 // 12:00:00.000", tc.tdbToUTCString(-5.0612169558814354E+10, "D"));
    assertEquals("JD 1865756.00000000", tc.tdbToUTCString(-5.0612169558814354E+10, "J"));
    assertRelativeEquality(
        -5.0612169558814354E+10, tc.utcStringToTDB("     396 A.D. March 1 12:00:00"), DELTA);
    assertEquals("397 A.D. MAR 01 12:00:00.000", tc.tdbToUTCString(-5.0580633558814354E+10, "C"));
    assertEquals("397 A.D. 060 // 12:00:00.000", tc.tdbToUTCString(-5.0580633558814354E+10, "D"));
    assertEquals("JD 1866121.00000000", tc.tdbToUTCString(-5.0580633558814354E+10, "J"));
    assertRelativeEquality(
        -5.0580633558814354E+10, tc.utcStringToTDB("     397 A.D. March 1 12:00:00"), DELTA);
    assertEquals("398 A.D. MAR 01 12:00:00.000", tc.tdbToUTCString(-5.0549097558814354E+10, "C"));
    assertEquals("398 A.D. 060 // 12:00:00.000", tc.tdbToUTCString(-5.0549097558814354E+10, "D"));
    assertEquals("JD 1866486.00000000", tc.tdbToUTCString(-5.0549097558814354E+10, "J"));
    assertRelativeEquality(
        -5.0549097558814354E+10, tc.utcStringToTDB("     398 A.D. March 1 12:00:00"), DELTA);
    assertEquals("399 A.D. MAR 01 12:00:00.000", tc.tdbToUTCString(-5.0517561558814354E+10, "C"));
    assertEquals("399 A.D. 060 // 12:00:00.000", tc.tdbToUTCString(-5.0517561558814354E+10, "D"));
    assertEquals("JD 1866851.00000000", tc.tdbToUTCString(-5.0517561558814354E+10, "J"));
    assertRelativeEquality(
        -5.0517561558814354E+10, tc.utcStringToTDB("     399 A.D. March 1 12:00:00"), DELTA);
    assertEquals("400 A.D. MAR 01 12:00:00.000", tc.tdbToUTCString(-5.0485939158814354E+10, "C"));
    assertEquals("400 A.D. 061 // 12:00:00.000", tc.tdbToUTCString(-5.0485939158814354E+10, "D"));
    assertEquals("JD 1867217.00000000", tc.tdbToUTCString(-5.0485939158814354E+10, "J"));
    assertRelativeEquality(
        -5.0485939158814354E+10, tc.utcStringToTDB("     400 A.D. March 1 12:00:00"), DELTA);
    assertEquals("401 A.D. MAR 01 12:00:00.000", tc.tdbToUTCString(-5.0454403158814354E+10, "C"));
    assertEquals("401 A.D. 060 // 12:00:00.000", tc.tdbToUTCString(-5.0454403158814354E+10, "D"));
    assertEquals("JD 1867582.00000000", tc.tdbToUTCString(-5.0454403158814354E+10, "J"));
    assertRelativeEquality(
        -5.0454403158814354E+10, tc.utcStringToTDB("     401 A.D. March 1 12:00:00"), DELTA);
    assertEquals("402 A.D. MAR 01 12:00:00.000", tc.tdbToUTCString(-5.0422867158814354E+10, "C"));
    assertEquals("402 A.D. 060 // 12:00:00.000", tc.tdbToUTCString(-5.0422867158814354E+10, "D"));
    assertEquals("JD 1867947.00000000", tc.tdbToUTCString(-5.0422867158814354E+10, "J"));
    assertRelativeEquality(
        -5.0422867158814354E+10, tc.utcStringToTDB("     402 A.D. March 1 12:00:00"), DELTA);
  }
}
