/*
 * Filename: LocalTimeToRadiansConversionsTest.java Author : vandejd1 Created : Aug 28, 2008 3:38:54
 * PM
 * 
 * Copyright (C) 2008 The Johns Hopkins University Applied Physics Laboratory (JHU/APL) All rights
 * reserved
 */
package picante.units;

import org.junit.Assert;
import org.junit.Test;

/**
 * 
 * @author vandejd1
 */
public class LocalTimeToRadiansConversionsTest {
  private static final double accuracy = 1.e-14;

  @Test
  public void testConvertLongitudeInRadiansToLocaltimeInFractionalHoursA() {
    double longitudeDeg = 10;
    double ltimeActual = longitudeDeg / 360. * 24 + 12;

    doLongToLocTimeTest(longitudeDeg, ltimeActual);
  }

  @Test
  public void testConvertLongitudeInRadiansToLocaltimeInFractionalHoursB() {
    double longitudeDeg = 0;
    double ltimeActual = 12;

    doLongToLocTimeTest(longitudeDeg, ltimeActual);
  }

  @Test
  public void testConvertLongitudeInRadiansToLocaltimeInFractionalHoursC() {
    double longitudeDeg = 90;
    double ltimeActual = 18;

    doLongToLocTimeTest(longitudeDeg, ltimeActual);
  }

  @Test
  public void testConvertLongitudeInRadiansToLocaltimeInFractionalHoursD() {
    double longitudeDeg = 276;
    double ltimeActual = 6 + 1. / 60. * 24.;

    doLongToLocTimeTest(longitudeDeg, ltimeActual);
  }

  @Test
  public void testConvertLongitudeInRadiansToLocaltimeInFractionalHoursE() {
    double longitudeDeg = 0;
    double ltimeActual = 12;

    doLongToLocTimeTest(longitudeDeg, ltimeActual);
  }

  @Test
  public void testConvertLongitudeInRadiansToLocaltimeInFractionalHoursF() {
    double longitudeDeg = 180;
    double ltimeActual = 0;

    doLongToLocTimeTest(longitudeDeg, ltimeActual);
  }

  private void doLongToLocTimeTest(double longitudeDeg, double ltimeActual) {
    double longitudeRad = Math.toRadians(longitudeDeg);
    double ltimeComputed = LocalTimeToRadiansConversions.radiansToLocaltime(longitudeRad);
    Assert.assertEquals("Longitude of " + longitudeDeg, ltimeActual, ltimeComputed, accuracy);
  }

  @Test
  public void testConvertLocaltimeInFractionalHoursToLongitudeInRadiansA() {
    double longitudeDeg = 0;
    double ltime = 12;

    doLocTimeToLonTest(ltime, longitudeDeg);
  }

  @Test
  public void testConvertLocaltimeInFractionalHoursToLongitudeInRadiansB() {
    double longitudeDeg = 180 - 360;
    double ltime = 0;

    doLocTimeToLonTest(ltime, longitudeDeg);
  }

  @Test
  public void testConvertLocaltimeInFractionalHoursToLongitudeInRadiansC() {
    double longitudeDeg = 276 - 360;
    double ltime = 6 + 1. / 60. * 24.;

    doLocTimeToLonTest(ltime, longitudeDeg);
  }

  @Test
  public void testConvertLocaltimeInFractionalHoursToLongitudeInRadiansD() {
    double longitudeDeg = 90;
    double ltime = 18;

    doLocTimeToLonTest(ltime, longitudeDeg);
  }


  /**
   * longitude must be from -180 to 180
   * 
   * @param localTimeInHrs
   * @param longitudeInDegreesActual
   */
  private void doLocTimeToLonTest(double localTimeInHrs, double longitudeInDegreesActual) {
    new LocalTimeToRadiansConversions();
    double longitudeComputedInRadians =
        LocalTimeToRadiansConversions.localtimeHoursToRadians(localTimeInHrs);
    double longitudeInDegreesComputed = Math.toDegrees(longitudeComputedInRadians);
    Assert.assertEquals("Localtime of " + localTimeInHrs, longitudeInDegreesActual,
        longitudeInDegreesComputed, accuracy);
  }


}
