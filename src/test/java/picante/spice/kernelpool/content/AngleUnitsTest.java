package picante.spice.kernelpool.content;

import static org.junit.Assert.assertEquals;
import static picante.junit.AssertTools.assertRelativeEquality;
import java.util.Set;
import org.junit.Test;
import com.google.common.collect.Sets;
import picante.junit.AssertTools;

public class AngleUnitsTest {

  private final static double TOLERANCE = 1e-14;

  @Test
  public void testAcceptableUnitsSet() {
    Set<String> expected = Sets.newHashSet();
    for (AngleUnits converter : AngleUnits.values()) {
      expected.add(converter.name());
    }
    assertEquals(expected, AngleUnits.ACCEPTABLE_ANGLE_UNITS);
  }

  @Test
  public void testRadiansConvertToRadians() {
    AssertTools.assertEqualDouble(1.0, AngleUnits.RADIANS.convertToRadians(1.0));
    AssertTools.assertEqualDouble(1e6, AngleUnits.RADIANS.convertToRadians(1e6));
  }

  @Test
  public void testDegreesConvertToRadians() {
    assertRelativeEquality(1.74532925199432955E-002, AngleUnits.DEGREES.convertToRadians(1.0),
        TOLERANCE);
    assertRelativeEquality(1e6 * 1.74532925199432955E-002, AngleUnits.DEGREES.convertToRadians(1e6),
        TOLERANCE);
  }

  @Test
  public void testArcminutesConvertToRadians() {
    assertRelativeEquality(2.90888208665721580E-004, AngleUnits.ARCMINUTES.convertToRadians(1.0),
        TOLERANCE);
    assertRelativeEquality(1e6 * 2.90888208665721580E-004,
        AngleUnits.ARCMINUTES.convertToRadians(1e6), TOLERANCE);
  }

  @Test
  public void testArcsecondsConvertToRadians() {
    assertRelativeEquality(4.84813681109535984E-006, AngleUnits.ARCSECONDS.convertToRadians(1.0),
        TOLERANCE);
    assertRelativeEquality(1e6 * 4.84813681109535984E-006,
        AngleUnits.ARCSECONDS.convertToRadians(1e6), TOLERANCE);

  }

  @Test
  public void testHourangleConvertToRadians() {
    assertRelativeEquality(0.26179938779914941, AngleUnits.HOURANGLE.convertToRadians(1.0),
        TOLERANCE);
    assertRelativeEquality(1e6 * 0.26179938779914941, AngleUnits.HOURANGLE.convertToRadians(1e6),
        TOLERANCE);

  }

  @Test
  public void testMinuteangleConvertToRadians() {
    assertRelativeEquality(4.36332312998582387E-003, AngleUnits.MINUTEANGLE.convertToRadians(1.0),
        TOLERANCE);
    assertRelativeEquality(1e6 * 4.36332312998582387E-003,
        AngleUnits.MINUTEANGLE.convertToRadians(1e6), TOLERANCE);

  }

  @Test
  public void testSecondangleConvertToRadians() {
    assertRelativeEquality(7.27220521664303951E-005, AngleUnits.SECONDANGLE.convertToRadians(1.0),
        TOLERANCE);
    assertRelativeEquality(1e6 * 7.27220521664303951E-005,
        AngleUnits.SECONDANGLE.convertToRadians(1e6), TOLERANCE);

  }

}
