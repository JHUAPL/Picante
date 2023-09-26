/**
 * Author : vandejd1 Created : Nov 17, 2010
 * 
 * Copyright (C) 2010 The Johns Hopkins University Applied Physics Laboratory (JHU/APL) All rights
 * reserved
 */
package picante.time;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

/**
 * 
 * @author vandejd1
 */
public class UTCEpochTest {
  @Test
  public void testCreateValueRoundedToMicrosZ1() {
    UTCEpoch utc = new UTCEpoch(2000, 366, 23, 59, 59.99999949).createValueRoundedToMicrosecs();
    assertEquals("Year", 2000, utc.getYear());
    assertEquals("Doy", 366, utc.getDoy());
    assertEquals("Hr", 23, utc.getHour());
    assertEquals("Min", 59, utc.getMin());
    double tolerance = 1.e-9;
    assertEquals("Sec", 59.999999, utc.getSec(), tolerance);
  }

  @Test
  public void testCreateValueRoundedToMicrosZ2() {
    UTCEpoch utc = new UTCEpoch(2000, 366, 23, 59, 59.99999951).createValueRoundedToMicrosecs();
    assertEquals("Year", 2001, utc.getYear());
    assertEquals("Doy", 1, utc.getDoy());
    assertEquals("Hr", 0, utc.getHour());
    assertEquals("Min", 0, utc.getMin());
    double tolerance = 1.e-12;
    assertEquals("Sec", 0, utc.getSec(), tolerance);
  }

  @Test
  public void testCreateValueRoundedToMicrosZ3() {
    UTCEpoch utc = new UTCEpoch(2005, 296, 15, 43, 58.12345678).createValueRoundedToMicrosecs();
    assertEquals("Year", 2005, utc.getYear());
    assertEquals("Doy", 296, utc.getDoy());
    assertEquals("Hr", 15, utc.getHour());
    assertEquals("Min", 43, utc.getMin());
    double tolerance = 1.e-12;
    assertEquals("Sec", 58.123457, utc.getSec(), tolerance);
  }

  @Test
  public void testCreateValueRoundedToMicrosZ4() {
    UTCEpoch utc = new UTCEpoch(2030, 9, 22, 18, 12.1239996).createValueRoundedToMicrosecs();
    assertEquals("Year", 2030, utc.getYear());
    assertEquals("Doy", 9, utc.getDoy());
    assertEquals("Hr", 22, utc.getHour());
    assertEquals("Min", 18, utc.getMin());
    double tolerance = 1.e-12;
    assertEquals("Sec", 12.124, utc.getSec(), tolerance);
  }

  @Test
  public void testCreateValueRoundedToMicrosZ5() {
    UTCEpoch utc = new UTCEpoch(2030, 9, 22, 18, 12.9999997).createValueRoundedToMicrosecs();
    assertEquals("Year", 2030, utc.getYear());
    assertEquals("Doy", 9, utc.getDoy());
    assertEquals("Hr", 22, utc.getHour());
    assertEquals("Min", 18, utc.getMin());
    double tolerance = 1.e-12;
    assertEquals("Sec", 13.0, utc.getSec(), tolerance);
  }

  @Test
  public void testCreateValueRoundedToMicrosZ6() {
    UTCEpoch utc = new UTCEpoch(2005, 296, 15, 43, 59.9999998).createValueRoundedToMicrosecs();
    assertEquals("Year", 2005, utc.getYear());
    assertEquals("Doy", 296, utc.getDoy());
    assertEquals("Hr", 15, utc.getHour());
    assertEquals("Min", 44, utc.getMin());
    double tolerance = 1.e-12;
    assertEquals("Sec", 0, utc.getSec(), tolerance);
  }

  @Test
  public void testCreateValueRoundedToMillisecsZ1() {
    UTCEpoch utc = new UTCEpoch(2000, 366, 23, 59, 59.99949).createValueRoundedToMillisecs();
    assertEquals("Year", 2000, utc.getYear());
    assertEquals("Doy", 366, utc.getDoy());
    assertEquals("Hr", 23, utc.getHour());
    assertEquals("Min", 59, utc.getMin());
    double tolerance = 1.e-9;
    assertEquals("Sec", 59.999, utc.getSec(), tolerance);
  }

  @Test
  public void testCreateValueRoundedToMillisecsZ2() {
    UTCEpoch utc = new UTCEpoch(2000, 366, 23, 59, 59.99951).createValueRoundedToMillisecs();
    assertEquals("Year", 2001, utc.getYear());
    assertEquals("Doy", 1, utc.getDoy());
    assertEquals("Hr", 0, utc.getHour());
    assertEquals("Min", 0, utc.getMin());
    double tolerance = 1.e-12;
    assertEquals("Sec", 0, utc.getSec(), tolerance);
  }

  @Test
  public void testToString1() {
    UTCEpoch utc = new UTCEpoch(2000, 366, 23, 59, 59.99949);
    assertEquals("UTC to string with rounding to millisecs", "2000-366T23:59:59.999",
        utc.toString());
  }

  @Test
  public void testToString2() {
    UTCEpoch utc = new UTCEpoch(2000, 366, 23, 59, 59.99951);
    assertEquals("UTC to string with rounding to millisecs", "2001-001T00:00:00.000",
        utc.toString());
  }

  private void testNextDay(int year1, int doy1, int year2Actual, int doy2Actual) {
    UTCEpoch nextUtc = UTCEpoch.getNextDay(year1, doy1);
    UTCEpoch nextUtcActual = new UTCEpoch(year2Actual, doy2Actual, 0, 0, 0);
    checkUtcSameness(nextUtc, nextUtcActual);
  }

  @Test
  public void testNextDay() {
    testNextDay(2000, 364, 2000, 365);
    testNextDay(2000, 365, 2000, 366);
    testNextDay(2001, 366, 2002, 1);
    testNextDay(1999, 31, 1999, 32);
    testNextDay(2008, 365, 2008, 366);
    testNextDay(2008, 366, 2009, 1);
    testNextDay(2007, 365, 2008, 1);
  }

  @Test
  public void testPrevDay() {
    testPrevDay(2000, 365, 2000, 364);
    testPrevDay(2001, 1, 2000, 366);
    testPrevDay(1999, 32, 1999, 31);
    testPrevDay(2008, 366, 2008, 365);
    testPrevDay(2009, 1, 2008, 366);
    testPrevDay(2008, 1, 2007, 365);
  }

  private void testPrevDay(int year1, int doy1, int year2Actual, int doy2Actual) {
    UTCEpoch nextUtc = UTCEpoch.getPreviousDay(year1, doy1);
    UTCEpoch nextUtcActual = new UTCEpoch(year2Actual, doy2Actual, 0, 0, 0);
    checkUtcSameness(nextUtc, nextUtcActual);
  }

  private void testPreviousMonth(int year1, int month1, int year2Actual, int month2Actual) {
    UTCEpoch nextUtc = UTCEpoch.getPreviousMonth(year1, month1);
    UTCEpoch nextUtcActual = new UTCEpoch(year2Actual, month2Actual, 1, 0, 0, 0);

    checkUtcSameness(nextUtc, nextUtcActual);
  }

  private void testNextMonth(int year1, int month1, int year2Actual, int month2Actual) {
    UTCEpoch nextUtc = UTCEpoch.getNextMonth(year1, month1);
    UTCEpoch nextUtcActual = new UTCEpoch(year2Actual, month2Actual, 1, 0, 0, 0);

    checkUtcSameness(nextUtc, nextUtcActual);
  }

  @Test
  public void testPreviousMonth() {
    testPreviousMonth(2000, 11, 2000, 10);
    testPreviousMonth(2000, 12, 2000, 11);
    testPreviousMonth(2001, 12, 2001, 11);
    testPreviousMonth(1999, 1, 1998, 12);
    testPreviousMonth(2008, 2, 2008, 1);
    testPreviousMonth(2008, 1, 2007, 12);
    testPreviousMonth(2007, 1, 2006, 12);

    // I guess this is allowed
    testPreviousMonth(2000, 13, 2000, 12);
    testPreviousMonth(2000, 0, 1999, 12);
    testPreviousMonth(2000, -1, 1999, 12);
    testPreviousMonth(2000, -2, 1999, 12);
  }

  @Test
  public void testNextMonth() {
    testNextMonth(2000, 11, 2000, 12);
    testNextMonth(2000, 12, 2001, 1);
    testNextMonth(2001, 12, 2002, 1);
    testNextMonth(1999, 1, 1999, 2);
    testNextMonth(2008, 11, 2008, 12);
    testNextMonth(2008, 12, 2009, 1);
    testNextMonth(2007, 12, 2008, 1);

    // I guess this is allowed
    testNextMonth(2000, 13, 2001, 1);
    testNextMonth(2000, 0, 2000, 1);
    testNextMonth(2000, -1, 2000, 0);
    testNextMonth(2000, -2, 2000, -1);
  }

  @Test
  public void testZeroEpoch() {
    TimeSystems timeSystems = TimeSystems.createUsingInternalConstants();
    TimeSystem<Double> etTimeSys = timeSystems.getTDB();
    TimeSystem<UTCEpoch> utcTimeSys = timeSystems.getUTC();
    TSEpoch et0 = etTimeSys.getTSEpoch(0.0);
    UTCEpoch utcZero = utcTimeSys.getTime(et0);
    checkUtcSameness(new UTCEpoch(2000, 1, 11, 58, 55.816), utcZero);
  }

  private void checkUtcSameness(UTCEpoch u1, UTCEpoch u2) {
    assertEquals("Year", u1.getYear(), u2.getYear());
    assertEquals("Doy", u1.getDoy(), u2.getDoy());
    assertEquals("Hr", u1.getHour(), u2.getHour());
    assertEquals("Min", u2.getMin(), u2.getMin());
    double delta = 1.e-10;
    assertEquals("Sec", u2.getSec(), u2.getSec(), delta);

  }


  @Test
  public void testCreateValueRoundedToMillisecs0() {
    UTCEpoch utc = new UTCEpoch(2000, 366, 23, 59, 59.99949).createValueRoundedToMillisecs();

    assertEquals("Year", utc.getYear(), 2000);
    assertEquals("Doy", utc.getDoy(), 366);
    assertEquals("Hr", utc.getHour(), 23);
    assertEquals("Min", utc.getMin(), 59);
    assertEquals("Sec", utc.getSec(), 59.999, 1.e-16);
  }

  @Test
  public void testCreateValueRoundedToMillisecs1() {
    UTCEpoch utc = new UTCEpoch(2000, 366, 23, 59, 59.99951).createValueRoundedToMillisecs();

    assertEquals("Year", utc.getYear(), 2001);
    assertEquals("Doy", utc.getDoy(), 1);
    assertEquals("Hr", utc.getHour(), 0);
    assertEquals("Min", utc.getMin(), 0);
    assertEquals("Sec", utc.getSec(), 0, 1.e-16);
  }

  @Test
  public void testCreateValueRoundedToMillisecsA1() {
    testOneMillisecRoundedTime(new UTCEpoch(2000, 366, 23, 59, 59.99949));
  }

  @Test
  public void testCreateValueRoundedToMillisecsA2() {
    testOneMillisecRoundedTime(new UTCEpoch(2000, 366, 23, 59, 59.99951));
  }

  @Test
  public void testCreateValueRoundedToMillisecsB1() {
    testOneMillisecRoundedTime(new UTCEpoch(1999, 365, 23, 59, 59.99949));
  }

  @Test
  public void testCreateValueRoundedToMillisecsB2() {
    testOneMillisecRoundedTime(new UTCEpoch(1999, 365, 23, 59, 59.99951));
  }

  @Test
  public void testCreateValueRoundedToMillisecsC() {
    testOneMillisecRoundedTime(new UTCEpoch(1995, 362, 23, 59, 59.99951));
  }

  @Test
  public void testCreateValueRoundedToMillisecsD1() {
    testOneMillisecRoundedTime(new UTCEpoch(2010, 77, 3, 31, 9.999));
  }

  @Test
  public void testCreateValueRoundedToMillisecsD2() {
    testOneMillisecRoundedTime(new UTCEpoch(2010, 77, 3, 31, 9.9996));
  }



  private void testOneMillisecRoundedTime(UTCEpoch utc) {
    // This method does the rouding in a different way and compares it to the rounding
    // done in the UTCEpoch class.

    double dsec = utc.getSec();
    int secsInIntegerMillis = (int) (dsec * 1000.0 + 0.5);
    int intSecs = (int) (secsInIntegerMillis / 1000.0);
    int rawMillis = secsInIntegerMillis - intSecs * 1000;

    int yr;
    int doy;
    int hr;
    int min;
    int sec;
    int millis;
    if (intSecs != (int) dsec) {
      // the rounding bumped up the seconds by one.
      // So larger fields could also roll over.
      // Compute the start of the next second, and then the millis are 0.
      TimeSystems timeSystems = TimeSystems.createUsingInternalConstants();
      TimeSystem<UTCEpoch> utcTimeSys = timeSystems.getUTC();

      UTCEpoch oneSecondLater = utcTimeSys.add(new UTCEpoch(utc.getYear(), utc.getDoy(),
          utc.getHour(), utc.getMin(), (int) utc.getSec()), 1.0);
      yr = oneSecondLater.getYear();
      doy = oneSecondLater.getDoy();
      hr = oneSecondLater.getHour();
      min = oneSecondLater.getMin();
      sec = (int) oneSecondLater.getSec();
      millis = 0;
    } else {
      yr = utc.getYear();
      doy = utc.getDoy();
      hr = utc.getHour();
      min = utc.getMin();
      sec = intSecs;
      millis = rawMillis;
    }
    int[] vals = utc.roundToMilliseconds();
    int oyr = vals[0];
    int odoy = vals[1];
    int ohr = vals[2];
    int omin = vals[3];
    int osec = vals[4];
    int omillis = vals[5];

    assertEquals("Year", yr, oyr);
    assertEquals("Doy", doy, odoy);
    assertEquals("Hr", hr, ohr);
    assertEquals("Min", min, omin);
    assertEquals("Sec", sec, osec);
    assertEquals("Millis", millis, omillis);
  }

  @Test
  public void testCreateValueRoundedToSeconds0() {
    UTCEpoch utc = new UTCEpoch(2000, 366, 23, 59, 59.99949).createValueRoundedToSeconds();

    assertEquals("Year", utc.getYear(), 2001);
    assertEquals("Doy", utc.getDoy(), 1);
    assertEquals("Hr", utc.getHour(), 0);
    assertEquals("Min", utc.getMin(), 0);
    assertEquals("Sec", utc.getSec(), 0, 1.e-16);
  }


  @Test
  public void testCreateValueRoundedToSecondsA1() {
    testOneSecondsRoundedTime(new UTCEpoch(2010, 77, 3, 31, 9.9996));
    testOneSecondsRoundedTime(new UTCEpoch(1995, 362, 23, 59, 59.99949));
    testOneSecondsRoundedTime(new UTCEpoch(1995, 362, 23, 59, 59.99951));
  }

  private void testOneSecondsRoundedTime(UTCEpoch utc) {
    UTCEpoch rndUtc = utc.createValueRoundedToSeconds();
    // This method does the rouding in a different way and compares it to the rounding
    // done in the UTCEpoch class.

    double dsec = utc.getSec();
    int intSecs = (int) (dsec + 0.5);

    int deltaSecs = 0;
    if (intSecs != (int) dsec) {
      // the rounding bumped up the seconds by one.
      // So larger fields could also roll over.
      // Compute the start of the next second, and then the millis are 0.
      deltaSecs = 1;
    }
    TimeSystems timeSystems = TimeSystems.createUsingInternalConstants();
    TimeSystem<UTCEpoch> utcTimeSys = timeSystems.getUTC();
    UTCEpoch altUtc = utcTimeSys.add(
        new UTCEpoch(utc.getYear(), utc.getDoy(), utc.getHour(), utc.getMin(), (int) utc.getSec()),
        deltaSecs);

    assertEquals("Year", rndUtc.getYear(), altUtc.getYear());
    assertEquals("Doy", rndUtc.getDoy(), altUtc.getDoy());
    assertEquals("Hr", rndUtc.getHour(), altUtc.getHour());
    assertEquals("Min", rndUtc.getMin(), altUtc.getMin());
    assertEquals("Sec", rndUtc.getSec(), altUtc.getSec(), 1.e-16);
  }



  @Test
  public void testCreateValueRoundedToMinutes1() {
    UTCEpoch utcRounded1 = new UTCEpoch(2001, 1, 23, 59, 45.0).createValueRoundedToMinutes();
    UTCEpoch utcActual1 = new UTCEpoch(2001, 2, 0, 0, 0);
    testEquality(utcRounded1, utcActual1);

    UTCEpoch utcRounded2 = new UTCEpoch(2004, 366, 23, 59, 45.0).createValueRoundedToMinutes();
    UTCEpoch utcActual2 = new UTCEpoch(2005, 1, 0, 0, 0);
    testEquality(utcRounded2, utcActual2);

    UTCEpoch utcRounded3 = new UTCEpoch(2013, 181, 2, 33, 5.0).createValueRoundedToMinutes();
    UTCEpoch utcActual3 = new UTCEpoch(2013, 181, 2, 33, 0);
    testEquality(utcRounded3, utcActual3);
  }

  private void testEquality(UTCEpoch u1, UTCEpoch u2) {
    assertEquals("Year", u1.getYear(), u2.getYear());
    assertEquals("Doy", u1.getDoy(), u2.getDoy());
    assertEquals("Hr", u1.getHour(), u2.getHour());
    assertEquals("Min", u1.getMin(), u2.getMin());
    assertEquals("Sec", u1.getSec(), u2.getSec(), 1.e-16);
  }

  @Test
  public void testAsFractionalDoy() {

    assertEquals(1.0, new UTCEpoch(2014, 1, 0, 0, 0.0).getAsFractionalDoy(), 0.0);
    assertEquals(1.0 + 12.345 / (24 * 3600),
        new UTCEpoch(2014, 1, 0, 0, 12.345).getAsFractionalDoy(), 0.0);
    assertEquals(1.0 + 58. / (24 * 60), new UTCEpoch(2014, 1, 0, 58, 0.0).getAsFractionalDoy(),
        0.0);
    assertEquals(1.0 + 3. / 24, new UTCEpoch(2014, 1, 3, 0, 0.0).getAsFractionalDoy(), 0.0);
    assertEquals(356, new UTCEpoch(2014, 356, 0, 0, 0.0).getAsFractionalDoy(), 0.0);
    assertEquals(356 + 3. / 24 + 58. / (24 * 60) + 12.345 / (24 * 3600),
        new UTCEpoch(2014, 356, 3, 58, 12.345).getAsFractionalDoy(), 0.0);
  }

}
