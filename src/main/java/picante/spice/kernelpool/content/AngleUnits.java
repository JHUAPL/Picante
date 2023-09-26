package picante.spice.kernelpool.content;

import java.util.Arrays;

import com.google.common.base.Function;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Iterators;

/**
 * Simple enumeration to handle all of the supported angular units by the TKFRAME ANGLES
 * specification.
 * <p>
 * The acceptable units are defined in the SPICE routine CONVRT.
 * </p>
 */
enum AngleUnits {

  RADIANS(1.0), DEGREES(Math.toRadians(1.0)),

  /**
   * Standard conversion from arcminutes, 1/60th of a degree.
   */
  ARCMINUTES(Math.toRadians(1.0) / 60.0),

  /**
   * Standard conversion from arcseconds, 1/3600th of a degree.
   */
  ARCSECONDS(Math.toRadians(1.0) / 3600.0),

  /**
   * Standard conversion from an angle measured in hours, typically for right ascension in
   * astronomical applications. One hour is 15 degrees.
   */
  HOURANGLE(15.0 * Math.toRadians(1.0)),

  /**
   * Continuing the time analogy, this is an angle measured in a minute or 1/60th of 15 degrees.
   */
  MINUTEANGLE(15.0 * Math.toRadians(1.0) / 60.0),

  /**
   * And lastly, an angle measured in a second, or 1/3600th of 15 degrees.
   */
  SECONDANGLE(15.0 * Math.toRadians(1.0) / 3600.0);

  private final double radiansPerUnit;

  /**
   * Set used to capture all of the string based names for each element of the enumeration.
   */
  static final ImmutableSet<String> ACCEPTABLE_ANGLE_UNITS = ImmutableSet.copyOf(
      Iterators.transform(Arrays.asList(values()).iterator(), new Function<AngleUnits, String>() {
        @Override
        public String apply(AngleUnits input) {
          return input.name();
        }
      }));

  /**
   * Constructs the instance from the conversion factor
   * 
   * @param radiansPerUnit multiplier to convert from the units to radians
   */
  private AngleUnits(double radiansPerUnit) {
    this.radiansPerUnit = radiansPerUnit;
  }

  /**
   * Converts the value from the associated units into radians.
   * 
   * @param value the value to convert
   * 
   * @return the value in radians
   */
  double convertToRadians(double value) {
    return radiansPerUnit * value;
  }

}
