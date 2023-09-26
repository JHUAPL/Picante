package picante.spice.kernelpool.content;

import java.util.HashMap;
import java.util.Map;

/**
 * Simple enumeration that provides a link between SPICE string inputs and AngleUnits
 */
public enum DynamicUnits {


  RADIANS("RADIANS", AngleUnits.RADIANS), DEGREES("DEGREES",
      AngleUnits.DEGREES), ARCSECONDS("ARCSECONDS", AngleUnits.ARCSECONDS);

  private final String string;
  private final AngleUnits units;

  @Override
  public String toString() {
    return string;
  }

  /**
   * Converts the value from the associated units into radians.
   * 
   * @param value the value to convert
   * 
   * @return the value in radians
   */
  public double convertToRadians(double value) {
    return units.convertToRadians(value);
  }

  private DynamicUnits(String string, AngleUnits units) {
    this.string = string;
    this.units = units;
  }

  static Map<String, DynamicUnits> mapStringToUnit = new HashMap<>();
  static {
    for (DynamicUnits unit : DynamicUnits.values()) {
      mapStringToUnit.put(unit.toString(), unit);
    }
  }

  public static DynamicUnits fromString(String string) {
    return mapStringToUnit.get(string);
  }
}
