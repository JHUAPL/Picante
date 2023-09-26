package picante.time;

import static org.junit.Assert.assertEquals;
import org.junit.Before;
import org.junit.Test;

public class ISOStringParserTest {

  private TSEpoch targetEpoch;

  @Before
  public void setUp() {
    UTCEpoch utc = new UTCEpoch(2014, 7, 14, 1, 2, 3);
    targetEpoch = TimeAdapter.getInstance().getTSEpochFromUTC(utc);
  }


  @Test
  public void testMonthDay() {
    String monthDay = "2014-07-14T01:02:03";
    TSEpoch testEpoch = ISOStringParser.getEpochFromString(monthDay);
    assertEquals(targetEpoch, testEpoch);
  }


  @Test
  public void testYearDay() {
    String yearDay = "2014-195T01:02:03";
    TSEpoch testEpoch = ISOStringParser.getEpochFromString(yearDay);
    assertEquals(targetEpoch, testEpoch);
  }

  @Test
  public void testMonthDayNonStandard() {
    String monthDayNS = "2014-7-14-1:02:03";
    TSEpoch testEpoch = ISOStringParser.getEpochFromString(monthDayNS);
    assertEquals(targetEpoch, testEpoch);
  }


  @Test
  public void testYearDayNonStandard() {
    String yearDayNS = "2014:195:1:2:3";
    TSEpoch testEpoch = ISOStringParser.getEpochFromString(yearDayNS);
    assertEquals(targetEpoch, testEpoch);
  }



}
