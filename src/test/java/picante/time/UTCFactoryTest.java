package picante.time;

import static org.junit.Assert.assertEquals;
import static picante.junit.AssertTools.assertRelativeEquality;
import org.junit.Before;
import org.junit.Test;

/**
 * expected values are from web geo calc
 * 
 * @author stephgk1
 *
 */
public class UTCFactoryTest {

  private UTCFactory utcFactory;

  private static double TOL = 3.0E-13;

  @Before
  public void setUp() throws Exception {

    TimeSystems.Builder ts = TimeSystems.builder();

    ts.populateDefaultsFromInternalPropertiesFile();
    ts.buildLeapsecondsTable();

    utcFactory = new UTCFactory(ts.getDayTable(), ts.getTaiTable(), ts.getDelta_t_a(), ts.getEb(),
        ts.getK(), ts.getM0(), ts.getM1());
  }

  @Test
  public void testGetTDBUTCEpoch() {

    UTCEpoch testEpoch = new UTCEpoch(2009, 11, 24, 14, 21, 33.436);
    double tdb = utcFactory.getTDB(testEpoch);
    assertRelativeEquality(312344559.618930, tdb, TOL);

    testEpoch = new UTCEpoch(2100, 3, 1, 3, 18, 21.327000);
    tdb = utcFactory.getTDB(testEpoch);
    assertRelativeEquality(3160826370.512400, tdb, TOL);

    testEpoch = new UTCEpoch(100, 3, 1, 3, 18, 21.327000);
    tdb = utcFactory.getTDB(testEpoch);
    assertRelativeEquality(-5.99530776575E+10, tdb, TOL);

    testEpoch = new UTCEpoch(2015, 7, 2, 18, 1, 52.327124);
    tdb = utcFactory.getTDB(testEpoch);
    assertRelativeEquality(489132180.511200, tdb, TOL);

    testEpoch = new UTCEpoch(23, 4, 4, 18, 28, 29.29);
    tdb = utcFactory.getTDB(testEpoch);
    assertRelativeEquality(-6.2379999049524628E+10, tdb, TOL);

    // 1 AD
    testEpoch = new UTCEpoch(1, 6, 3, 0, 0, 0.);
    tdb = utcFactory.getTDB(testEpoch);
    assertRelativeEquality(-63069105558.816071, tdb, TOL);

    // 1 BC
    testEpoch = new UTCEpoch(0, 6, 3, 0, 0, 0.);
    tdb = utcFactory.getTDB(testEpoch);
    assertRelativeEquality(-63100641558.816078, tdb, TOL);

    // this is 18 BC
    testEpoch = new UTCEpoch(-17, 6, 3, 12, 29, 28.291);
    tdb = utcFactory.getTDB(testEpoch);
    assertRelativeEquality(-6.3637140590525070E+10, tdb, TOL);
  }

  @Test
  public void testGetETIntIntIntIntDouble() {

    double tdb = utcFactory.getTDB(2009, 328, 14, 21, 33.436);
    assertRelativeEquality(312344559.618930, tdb, TOL);

    tdb = utcFactory.getTDB(2100, 60, 3, 18, 21.327000);
    assertRelativeEquality(3160826370.512400, tdb, TOL);

    tdb = utcFactory.getTDB(100, 60, 3, 18, 21.327000);
    assertRelativeEquality(-5.99530776575E+10, tdb, TOL);

    tdb = utcFactory.getTDB(2015, 183, 18, 1, 52.327124);
    assertRelativeEquality(489132180.511200, tdb, TOL);
  }

  @Test
  public void testGetUTCfromTDB() {

    double testTDB = 312344559.618930;
    UTCEpoch epoch = utcFactory.getUTCfromTDB(testTDB);
    assertEquals(2009, epoch.getYear());
    assertEquals(11, epoch.getMonth());
    assertEquals(24, epoch.getDom());
    assertEquals(14, epoch.getHour());
    assertEquals(21, epoch.getMin());
    assertEquals(33.435996, epoch.getSec(), 1.0E-4);

    testTDB = 3160826370.512400;
    epoch = utcFactory.getUTCfromTDB(testTDB);
    assertEquals(2100, epoch.getYear());
    assertEquals(3, epoch.getMonth());
    assertEquals(1, epoch.getDom());
    assertEquals(3, epoch.getHour());
    assertEquals(18, epoch.getMin());
    assertEquals(21.327000, epoch.getSec(), 1.0E-4);

    testTDB = -5.99530776575E+10;
    epoch = utcFactory.getUTCfromTDB(testTDB);
    assertEquals(100, epoch.getYear());
    assertEquals(3, epoch.getMonth());
    assertEquals(1, epoch.getDom());
    assertEquals(3, epoch.getHour());
    assertEquals(18, epoch.getMin());
    assertEquals(21.327000, epoch.getSec(), 1.0E-1);

    testTDB = 489132180.511200;
    epoch = utcFactory.getUTCfromTDB(testTDB);
    assertEquals(2015, epoch.getYear());
    assertEquals(7, epoch.getMonth());
    assertEquals(2, epoch.getDom());
    assertEquals(18, epoch.getHour());
    assertEquals(1, epoch.getMin());
    assertEquals(52.327124, epoch.getSec(), 1.0E-4);

    testTDB = -6.2379999049524628E+10;
    epoch = utcFactory.getUTCfromTDB(testTDB);
    assertEquals(23, epoch.getYear());
    assertEquals(4, epoch.getMonth());
    assertEquals(4, epoch.getDom());
    assertEquals(18, epoch.getHour());
    assertEquals(28, epoch.getMin());
    assertEquals(29.29, epoch.getSec(), 1.0E-4);

    // this is 18 B.C.  Year is -17, when counting from 1.
    testTDB = -6.3637140590525070E+10;
    epoch = utcFactory.getUTCfromTDB(testTDB);
    assertEquals(-17, epoch.getYear());
    assertEquals(6, epoch.getMonth());
    assertEquals(3, epoch.getDom());
    assertEquals(12, epoch.getHour());
    assertEquals(29, epoch.getMin());
    assertEquals(28.291, epoch.getSec(), 1.0E-4);
  }

  @Test
  public void testGetTAIUTCEpoch() {

    UTCEpoch testEpoch = new UTCEpoch(2009, 11, 24, 14, 21, 33.436);
    double tai = utcFactory.getTAI(testEpoch);
    assertRelativeEquality(312344559.620000 - 32.184, tai, TOL);

    testEpoch = new UTCEpoch(2100, 3, 1, 3, 18, 21.327000);
    tai = utcFactory.getTAI(testEpoch);
    assertRelativeEquality(3160826370.511000 - 32.184, tai, TOL);

    testEpoch = new UTCEpoch(100, 3, 1, 3, 18, 21.327000);
    tai = utcFactory.getTAI(testEpoch);
    assertRelativeEquality(-5.99530776575E+10 - 32.184, tai, TOL);

    testEpoch = new UTCEpoch(2015, 7, 2, 18, 1, 52.327124);
    tai = utcFactory.getTAI(testEpoch);
    assertRelativeEquality(489132180.511120 - 32.184, tai, TOL);
  }

  @Test
  public void testGetTAIIntIntIntIntDouble() {

    double tai = utcFactory.getTAI(2009, 328, 14, 21, 33.436);
    assertRelativeEquality(312344559.620000 - 32.184, tai, TOL);

    tai = utcFactory.getTAI(2100, 60, 3, 18, 21.327000);
    assertRelativeEquality(3160826370.511000 - 32.184, tai, TOL);

    tai = utcFactory.getTAI(100, 60, 3, 18, 21.327000);
    assertRelativeEquality(-5.99530776575E+10 - 32.184, tai, TOL);

    tai = utcFactory.getTAI(2015, 183, 18, 1, 52.327124);
    assertRelativeEquality(489132180.511120 - 32.184, tai, TOL);
  }

  @Test
  public void testGetUTCfromTAI() {

    double testTAI = 312344559.620000 - 32.184;
    UTCEpoch epoch = utcFactory.getUTCfromTAI(testTAI);
    assertEquals(2009, epoch.getYear());
    assertEquals(11, epoch.getMonth());
    assertEquals(24, epoch.getDom());
    assertEquals(14, epoch.getHour());
    assertEquals(21, epoch.getMin());
    assertEquals(33.435996, epoch.getSec(), 1.0E-5);

    testTAI = 3160826370.511000 - 32.184;
    epoch = utcFactory.getUTCfromTAI(testTAI);
    assertEquals(2100, epoch.getYear());
    assertEquals(3, epoch.getMonth());
    assertEquals(1, epoch.getDom());
    assertEquals(3, epoch.getHour());
    assertEquals(18, epoch.getMin());
    assertEquals(21.327000, epoch.getSec(), 1.0E-5);

    testTAI = -5.99530776575E+10 - 32.184;
    epoch = utcFactory.getUTCfromTAI(testTAI);
    assertEquals(100, epoch.getYear());
    assertEquals(3, epoch.getMonth());
    assertEquals(1, epoch.getDom());
    assertEquals(3, epoch.getHour());
    assertEquals(18, epoch.getMin());
    assertEquals(21.327000, epoch.getSec(), 1.0E-1);

    testTAI = 489132180.511120 - 32.184;
    epoch = utcFactory.getUTCfromTAI(testTAI);
    assertEquals(2015, epoch.getYear());
    assertEquals(7, epoch.getMonth());
    assertEquals(2, epoch.getDom());
    assertEquals(18, epoch.getHour());
    assertEquals(1, epoch.getMin());
    assertEquals(52.327124, epoch.getSec(), 1.0E-5);
  }


  //
  // @Test
  // public void testDaysPast0001() {
  // fail("Not yet implemented");
  // }
  //
  // @Test
  // public void testConvertTDTToET() {
  // fail("Not yet implemented");
  // }
  //
  // @Test
  // public void testConvertETToTDT() {
  // fail("Not yet implemented");
  // }
  //
  // @Ignore
  // @Test
  // public void testConvertTAItoET() {
  // fail("Not yet implemented");
  // }
  //
  // @Ignore
  // @Test
  // public void testConvertETtoTAI() {
  // fail("Not yet implemented");
  // }
  //
  // @Ignore
  // @Test
  // public void testConvertTAIToUTC() {
  // fail("Not yet implemented");
  // }
  //
  // @Ignore
  // @Test
  // public void testConvertUTCToTAI() {
  // fail("Not yet implemented");
  // }
  //
  // @Ignore
  // @Test
  // public void testIsValidUTC() {
  // fail("Not yet implemented");
  // }
  //
  // @Ignore
  // @Test
  // public void testGetTAITableIndex() {
  // fail("Not yet implemented");
  // }
  //
  // @Test
  // public void testGetDayTableIndex() {
  // fail("Not yet implemented");
  // }

}
