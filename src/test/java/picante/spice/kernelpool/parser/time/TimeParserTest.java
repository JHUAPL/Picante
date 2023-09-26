package picante.spice.kernelpool.parser.time;

import junit.framework.TestCase;

public class TimeParserTest extends TestCase {

  /*
   * This should be capable of being 1.0E-14. However, I have uncovered what appears to be a problem
   * with the JVM's floating point implementation:
   * 
   * double e = -43200.0; double f = 43201.001; double g = e+f; g is assigned to 1.0009999999965657,
   * not 1.001.
   */
  private static final double EPSILON = 1.0E-11;

  private TimeParser parser = new TimeParser();

  /*
   * Test method for 'spice.kernelpool.parser.time.TimeParser.parse(String)'
   */
  public void testParse() {

    /*
     * TODO: Additional tests that need to be written... each production in the parser must be
     * exercised directly by this method. There were several, prior to the existence of this junit
     * test that must be explicity exercised. Should come up with a unique test value that checks
     * each field is interpretted properly...
     */

    /*
     * TODO: Each of the tokens should be tested... if possible.
     */


    /*
     * These were the original parser tests implemented in the main method of TimeParser when I
     * first started putting it together.
     */
    test("00-01-01T12:00:01.001", 1.001);
    test("2000-01-01T12:00:01.001", 1.001);
    test("2154.JD", -2.1162738240000E+11);
    test("2000-001/12:0.0", 0.0);
    test("2005-10-07T12:10:01.123", 1.8195900112300E+08);
    test("2000A.D.-001/12:00:00.000", 0.0);
    test("2000B.C.-001/12:00:00.000", -1.2619618560000E+11);
    test("00-1/12:00", 0.0);
    test("01-00AD/12:00", 0.0);
    test("2000-1//12:00", 0.0);

    /*
     * Test the effect of the addition of the <NUMBER> token on the Julian date time strings.
     */
    test("2000JD", -2.1164068800000E+11);
    test("2000.JD", -2.1164068800000E+11);
    test("2000.0JD", -2.1164068800000E+11);
    test("2000.00JD", -2.1164068800000E+11);

    test("JD2000", -2.1164068800000E+11);
    test("JD2000.", -2.1164068800000E+11);
    test("JD2000.0", -2.1164068800000E+11);
    test("JD2000.00", -2.1164068800000E+11);

    /*
     * Test the effect of the addition of the <NUMBER> token on the ISO style time strings.
     */
    test("2000-001T", -43200.0);
    test("2000-001T12", 0.0);
    test("2000-001T12.", 0.0);
    test("2000-001T12.0", 0.0);
    test("2000-001T12:00", 0.0);
    test("2000-001T12:00.", 0.0);
    test("2000-001T12:00.0", 0.0);
    test("2000-001T12:00:00", 0.0);
    test("2000-001T12:00:00.", 0.0);
    test("2000-001T12:00:00.0", 0.0);
    test("2000-001T12:00:00.00", 0.0);

    /*
     * Test the effect of the addition of the <NUMBER> token on DOY style time strings.
     */
    test("2000-001//", -43200.0);
    test("2000-001//12:00", 0.0);
    test("2000-001//12:00.", 0.0);
    test("2000-001//12:00.00", 0.0);
    test("2000-001//12:00:00", 0.0);
    test("2000-001//12:00:00.", 0.0);
    test("2000-001//12:00:00.00", 0.0);

    test("2000 001//", -43200.0);
    test("2000 001//12:00", 0.0);
    test("2000 001//12:00.", 0.0);
    test("2000 001//12:00.00", 0.0);
    test("2000 001//12:00:00", 0.0);
    test("2000 001//12:00:00.", 0.0);
    test("2000 001//12:00:00.00", 0.0);

    test("2000-001/", -43200.0);
    test("2000-001/12:00", 0.0);
    test("2000-001/12:00.", 0.0);
    test("2000-001/12:00.00", 0.0);
    test("2000-001/12:00:00", 0.0);
    test("2000-001/12:00:00.", 0.0);
    test("2000-001/12:00:00.00", 0.0);

    test("001 2000//", -43200.0);
    test("001 2000//12:00", 0.0);
    test("001 2000//12:00.", 0.0);
    test("001 2000//12:00.00", 0.0);
    test("001 2000//12:00:00", 0.0);
    test("001 2000//12:00:00.", 0.0);
    test("001 2000//12:00:00.00", 0.0);


    /*
     * Exercise the hand-generated ParseException for Julian date strings that have era and weekday
     * specifications.
     */
    testParseFailure("200154JD-A.D.");
    testParseFailure("(WED)200154JD");

    /*
     * Exercise the hand-generated ParseException for ISO style time strings that have era and
     * weekday specifications.
     */
    testParseFailure("2005-101T12:00:00 WED");
    testParseFailure("2005AD-101T12:01:10");

    /*
     * Exercise the hand-generated exception whenever two weekday specifications occur in a time
     * string that permits them.
     */
    testParseFailure("WED2005-001/12:00THU");

    /*
     * Now check the hand-generated exception whenever two era specifications are supplied.
     */
    testParseFailure("2005AD-001//12:00BC");
    testParseFailure("2005AD-001//12:00AD");

    /*
     * Test the space delimited numeric patterns.
     */
    test("2000 01 01", -43200.0);
    test("2000 01 01 12", 0.0);
    test("2000 01 01 12.", 0.0);
    test("2000 01 01 12.0", 0.0);
    test("2000 01 01 12 00", 0.0);
    test("2000 01 01 12 00.", 0.0);
    test("2000 01 01 12 00.0", 0.0);
    test("2000 01 01 12 00 00", 0.0);
    test("2000 01 01 12 00 00.", 0.0);
    test("2000 01 01 12 00 00.0", 0.0);

    test("2000 01 01 12:00", 0.0);
    test("2000 01 01 12:00.", 0.0);
    test("2000 01 01 12:00.0", 0.0);
    test("2000 01 01 12:00:00", 0.0);
    test("2000 01 01 12:00:00.", 0.0);
    test("2000 01 01 12:00:00.0", 0.0);

    /*
     * And the Month-Day-Year patterns.
     */
    test("01-01-2000", -43200.0);
    test("01-01-2000 12:00", 0.0);
    test("01-01-2000 12:00.", 0.0);
    test("01-01-2000 12:00.0", 0.0);
    test("01-01-2000 12:00:00", 0.0);
    test("01-01-2000 12:00:00.", 0.0);
    test("01-01-2000 12:00:00.0", 0.0);

    /*
     * Now try some slashed month, day, strict year expansions.
     */
    test("01/01/2000", -43200.0);
    test("01/01/2000 12:00", 0.0);
    test("01/01/2000 12:00.", 0.0);
    test("01/01/2000 12:00.0", 0.0);
    test("01/01/2000 12:00:00", 0.0);
    test("01/01/2000 12:00:00.", 0.0);
    test("01/01/2000 12:00:00.0", 0.0);

    test("01/01/2000/12:00", 0.0);
    test("01/01/2000/12:00.", 0.0);
    test("01/01/2000/12:00.0", 0.0);
    test("01/01/2000/12:00:00", 0.0);
    test("01/01/2000/12:00:00.", 0.0);
    test("01/01/2000/12:00:00.0", 0.0);

    /*
     * And now the "unspecific" year slashed delimited month day strings.
     */
    test("01/01/00", -43200.0);
    test("01/01/00 12:00", 0.0);
    test("01/01/00 12:00.", 0.0);
    test("01/01/00 12:00.0", 0.0);
    test("01/01/00 12:00:00", 0.0);
    test("01/01/00 12:00:00.", 0.0);
    test("01/01/00 12:00:00.0", 0.0);

    /*
     * And the trailing dash separated month, day, year specification.
     */
    test("01-01-2000", -43200.0);
    test("12:00 01-01-2000", 0.0);
    test("12:00. 01-01-2000", 0.0);
    test("12:00.0 01-01-2000", 0.0);
    test("12:00:00 01-01-2000", 0.0);
    test("12:00:00. 01-01-2000", 0.0);
    test("12:00:00.0 01-01-2000", 0.0);

    /*
     * And the trailing slash separated month, day, year specification.
     */
    test("01/01/2000", -43200.0);
    test("12:00 01/01/2000", 0.0);
    test("12:00. 01/01/2000", 0.0);
    test("12:00.0 01/01/2000", 0.0);
    test("12:00:00 01/01/2000", 0.0);
    test("12:00:00. 01/01/2000", 0.0);
    test("12:00:00.0 01/01/2000", 0.0);

    /*
     * And the same, but with integer years.
     */
    test("01/01/00", -43200.0);
    test("12:00 01/01/00", 0.0);
    test("12:00. 01/01/00", 0.0);
    test("12:00.0 01/01/00", 0.0);
    test("12:00:00 01/01/00", 0.0);
    test("12:00:00. 01/01/00", 0.0);
    test("12:00:00.0 01/01/00", 0.0);

    test("00 001//", -43200.0);
    test("00 001//12:00", 0.0);
    test("00 001//12:00.", 0.0);
    test("00 001//12:00.0", 0.0);
    test("00 001//12:00:00", 0.0);
    test("00 001//12:00:00.", 0.0);
    test("00 001//12:00:00.0", 0.0);

    /*
     * Testing the space delimited month day strict year production.
     */
    test("01 01 2000", -43200.0);
    test("01 01 2000 12", 0.0);
    test("01 01 2000 12.", 0.0);
    test("01 01 2000 12.0", 0.0);
    test("01 01 2000 12:00", 0.0);
    test("01 01 2000 12:00.", 0.0);
    test("01 01 2000 12:00.0", 0.0);
    test("01 01 2000 12:00:00", 0.0);
    test("01 01 2000 12:00:00.", 0.0);
    test("01 01 2000 12:00:00.0", 0.0);

    /*
     * Test the strict year, day, named month production.
     */
    test("2000 01 JAN", -43200.0);
    test("2000 01 JAN.", -43200.0);
    test("2000 01 JANU", -43200.0);
    test("2000 01 JANU.", -43200.0);
    test("2000 01 JANUA", -43200.0);
    test("2000 01 JANUA.", -43200.0);
    test("2000 01 JANUAR", -43200.0);
    test("2000 01 JANUAR.", -43200.0);
    test("2000 01 JANUARY", -43200.0);
    test("2000 01 JANUARY.", -43200.0);
    test("2000 01 JANUARY.12", 0.0);
    test("2000 01 JANUARY 12", 0.0);
    test("2000 01 JANUARY 12.", 0.0);
    test("2000 01 JANUARY 12.0", 0.0);
    test("2000 01 JANUARY 12:00", 0.0);
    test("2000 01 JANUARY 12:00.", 0.0);
    test("2000 01 JANUARY 12:00.0", 0.0);
    test("2000 01 JANUARY 12:00:00", 0.0);
    test("2000 01 JANUARY 12:00:00.", 0.0);
    test("2000 01 JANUARY 12:00:00.0", 0.0);

    /*
     * Test the strict year, named month, day production.
     */
    test("2000 JAN 01", -43200.0);
    test("2000 JAN. 01", -43200.0);
    test("2000 JANU 01", -43200.0);
    test("2000 JANU. 01", -43200.0);
    test("2000 JANUA 01", -43200.0);
    test("2000 JANUA. 01", -43200.0);
    test("2000 JANUAR 01", -43200.0);
    test("2000 JANUAR. 01", -43200.0);
    test("2000 JANUARY 01", -43200.0);
    test("2000 JANUARY. 01", -43200.0);
    test("2000 JANUARY.01 12", 0.0);
    test("2000 JANUARY 01 12", 0.0);
    test("2000 JANUARY 01 12.", 0.0);
    test("2000 JANUARY 01 12.0", 0.0);
    test("2000 JANUARY 01 12:00", 0.0);
    test("2000 JANUARY 01 12:00.", 0.0);
    test("2000 JANUARY 01 12:00.0", 0.0);
    test("2000 JANUARY 01 12:00:00", 0.0);
    test("2000 JANUARY 01 12:00:00.", 0.0);
    test("2000 JANUARY 01 12:00:00.0", 0.0);

    /*
     * Test the space delimited integer (year), day, named month.
     */
    test("00 01 JAN.", -43200.0);
    test("00 01 JAN. 12", 0.0);
    test("00 01 JAN. 12.", 0.0);
    test("00 01 JAN. 12.0", 0.0);
    test("00 01 JAN. 12:00", 0.0);
    test("00 01 JAN. 12:00.", 0.0);
    test("00 01 JAN. 12:00.0", 0.0);
    test("00 01 JAN. 12:00:00", 0.0);
    test("00 01 JAN. 12:00:00.", 0.0);
    test("00 01 JAN. 12:00:00.0", 0.0);
    test("00 01 JAN. 12 00", 0.0);
    test("00 01 JAN. 12 00.", 0.0);
    test("00 01 JAN. 12 00.0", 0.0);
    test("00 01 JAN. 12 00 00", 0.0);
    test("00 01 JAN. 12 00 00.", 0.0);
    test("00 01 JAN. 12 00 00.0", 0.0);

    /*
     * Space delimited day, named month, strict year.
     */
    test("01 JANUARY 2000", -43200.0);
    test("01 JANUARY 2000 12", 0.0);
    test("01 JANUARY 2000 12.", 0.0);
    test("01 JANUARY 2000 12.0", 0.0);
    test("01 JANUARY 2000 12:00", 0.0);
    test("01 JANUARY 2000 12:00.", 0.0);
    test("01 JANUARY 2000 12:00.0", 0.0);
    test("01 JANUARY 2000 12:00:00", 0.0);
    test("01 JANUARY 2000 12:00:00.", 0.0);
    test("01 JANUARY 2000 12:00:00.0", 0.0);

    test("01 (WEDNESDAY) JAN 2000 12:00", 0.0);
    test("01 (  WEDNESDAY   ) JAN 2000 12:00", 0.0);

    /*
     * Space delimited integer (year), named month, day.
     */
    test("00 JAN 01", -43200.0);
    test("00 JAN 01 12", 0.0);
    test("00 JAN 01 12.", 0.0);
    test("00 JAN 01 12.0", 0.0);
    test("00 JAN 01 12:00", 0.0);
    test("00 JAN 01 12:00.", 0.0);
    test("00 JAN 01 12:00.0", 0.0);
    test("00 JAN 01 12:00:00", 0.0);
    test("00 JAN 01 12:00:00.", 0.0);
    test("00 JAN 01 12:00:00.0", 0.0);
    test("00 JAN 01 12 00", 0.0);
    test("00 JAN 01 12 00.", 0.0);
    test("00 JAN 01 12 00.0", 0.0);
    test("00 JAN 01 12 00 00", 0.0);
    test("00 JAN 01 12 00 00.", 0.0);
    test("00 JAN 01 12 00 00.0", 0.0);

    /*
     * Named month, day, strict year.
     */
    test("JANUARY 01 2000", -43200.0);
    test("JANUARY 01 2000 12", 0.0);
    test("JANUARY 01 2000 12.", 0.0);
    test("JANUARY 01 2000 12.0", 0.0);
    test("JANUARY 01 2000 12:00", 0.0);
    test("JANUARY 01 2000 12:00.", 0.0);
    test("JANUARY 01 2000 12:00.0", 0.0);
    test("JANUARY 01 2000 12:00:00", 0.0);
    test("JANUARY 01 2000 12:00:00.", 0.0);
    test("JANUARY 01 2000 12:00:00.0", 0.0);

    /*
     * Space delimited named month, day, integer (year)
     */
    test("JAN 01 00", -43200.0);
    test("JAN 01 00 12", 0.0);
    test("JAN 01 00 12.", 0.0);
    test("JAN 01 00 12.0", 0.0);
    test("JAN 01 00 12:00", 0.0);
    test("JAN 01 00 12:00.", 0.0);
    test("JAN 01 00 12:00.0", 0.0);
    test("JAN 01 00 12:00:00", 0.0);
    test("JAN 01 00 12:00:00.", 0.0);
    test("JAN 01 00 12:00:00.0", 0.0);
    test("JAN 01 00 12 00", 0.0);
    test("JAN 01 00 12 00.", 0.0);
    test("JAN 01 00 12 00.0", 0.0);
    test("JAN 01 00 12 00 00", 0.0);
    test("JAN 01 00 12 00 00.", 0.0);
    test("JAN 01 00 12 00 00.0", 0.0);

    /*
     * Space delimited day, named month, time spec (hour required) and strict year.
     */
    test("01 JAN 12:00 2000", 0.0);
    test("01 JAN 12:00. 2000", 0.0);
    test("01 JAN 12:00.0 2000", 0.0);
    test("01 JAN 12:00:00 2000", 0.0);
    test("01 JAN 12:00:00. 2000", 0.0);
    test("01 JAN 12:00:00.0 2000", 0.0);

    /*
     * Space delimited named month, day, time spec (hour required) and strict year.
     */
    test("JANU 01 12:00 2000", 0.0);
    test("JANU 01 12:00. 2000", 0.0);
    test("JANU 01 12:00.0 2000", 0.0);
    test("JANU 01 12:00:00 2000", 0.0);
    test("JANU 01 12:00:00. 2000", 0.0);
    test("JANU 01 12:00:00.0 2000", 0.0);

    /*
     * Trailing day, named month and strict year... space delimited.
     */
    test("12:00 01 JANUARY 2000", 0.0);
    test("12:00. 01 JANUARY 2000", 0.0);
    test("12:00.0 01 JANUARY 2000", 0.0);
    test("12:00:00 01 JANUARY 2000", 0.0);
    test("12:00:00. 01 JANUARY 2000", 0.0);
    test("12:00:00.0 01 JANUARY 2000", 0.0);

    /*
     * Trailing named month, day and strict year... space delimited.
     */
    test("12:00 JANUA 01 2000", 0.0);
    test("12:00. JANUA 01 2000", 0.0);
    test("12:00.0 JANUA 01 2000", 0.0);
    test("12:00:00 JANUA 01 2000", 0.0);
    test("12:00:00. JANUA 01 2000", 0.0);
    test("12:00:00.0 JANUA 01 2000", 0.0);

    /*
     * Year, named month, decimal day.
     */
    test("2000 JAN 01", -43200.0);
    test("2000 JAN 01.", -43200.0);
    test("2000 JAN 01.5", 0.0);
    test("00 JAN 01", -43200.0);
    test("00 JAN 01.", -43200.0);
    test("00 JAN 01.5", 0.0);

    /*
     * Year, decimal day, named month.
     */
    test("2000 01 JAN", -43200.0);
    test("2000 01. JAN", -43200.0);
    test("2000 01.5 JAN", 0.0);
    test("00 01 JAN", -43200.0);
    test("00 01. JAN", -43200.0);
    test("00 01.5 JAN", 0.0);

    /*
     * Named month, decimal day, year.
     */
    test("JAN. 01 2000", -43200.0);
    test("JAN. 01. 2000", -43200.0);
    test("JAN. 01.5 2000", 0.0);
    test("JAN. 01 00", -43200.0);
    test("JAN. 01. 00", -43200.0);
    test("JAN. 01.5 00", 0.0);

    /*
     * Decimal day, named month, strict year.
     */
    test("01 JAN 2000", -43200.0);
    test("01. JAN 2000", -43200.0);
    test("01.5 JAN 2000", 0.0);

    /*
     * Month, decimal day, strict year.
     */
    test("01 01 '00", -43200.0);
    test("01 01. '00", -43200.0);
    test("01 01.5 '00", 0.0);

    /*
     * Strict year, month, decimal day.
     */
    test("00(AD) 01 01", -43200.0);
    test("00(AD) 01 01.", -43200.0);
    test("00(AD) 01 01.5", 0.0);

    /*
     * Try using dashes instead of spaces.
     */
    test("2000-JAN-01-12:00:00", 0.0);
    test("2000/001//12:00", 0.0);

  }

  private void test(String time, double expected) {
    try {
      assertEquals(expected, parser.parse(time), EPSILON);
    } catch (ParseException pe) {

      System.out.println(pe.getMessage());
      fail("Unexpected ParseException was thrown.");

    }
  }

  @SuppressWarnings("unused")
  private void testTokenizeFailure(String time) {
    try {
      parser.parse(time);
      fail("Expected TokenMgrError was not thrown.");
    } catch (TokenMgrError tme) {
      /*
       * Just catch the exception, that's sufficient.
       */
    } catch (ParseException pe) {
      fail("Expected TokenMgrError was not thrown, but a "
          + "ParseException was generated instead.");
    }
  }

  private void testParseFailure(String time) {
    try {
      parser.parse(time);
      fail("Expected ParseException was not thrown.");
    } catch (ParseException pe) {
      /*
       * Just catch the exception, that's good enough for now.
       */
    }
  }

}
