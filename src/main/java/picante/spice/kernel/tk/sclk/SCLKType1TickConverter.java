package picante.spice.kernel.tk.sclk;

import java.util.List;

import com.google.common.annotations.VisibleForTesting;

/**
 * Implementation of the {@link TickConverter} interface for the type 1 SCLK.
 */
class SCLKType1TickConverter implements TickConverter {

  /**
   * The ID code for the clock to which this converter is tied.
   */
  private final int clockID;

  /**
   * The number of ticks represented by a single count of each field
   */
  private final double[] ticksPerField;

  /**
   * The field moduli
   */
  private final double[] fieldModuli;

  /**
   * The offset for each field of the clock. Valid clock readings must have values greater than or
   * equal to this offset.
   */
  private final double[] offsets;

  /**
   * Creates a type 1 tick converter for use with the type 1 SCLK.
   * 
   * @param moduli the moduli counts for each clock field
   * @param offsets the offsets for each field
   * 
   * @throws SCLKInstantiationException if moduli or offsets are inappropriate for use by this
   *         implementation
   */
  SCLKType1TickConverter(int clockID, List<Double> moduli, List<Double> offsets)
      throws SCLKInstantiationException {
    this.clockID = clockID;
    if (moduli.size() != offsets.size()) {
      throw new SCLKInstantiationException(clockID,
          "The offset and moduli lists must contain precisely the " + "same number of elements.");
    }

    this.fieldModuli = new double[moduli.size()];
    int i = 0;
    for (double modulus : moduli) {
      this.fieldModuli[i] = modulus;
      i++;
    }

    this.ticksPerField = createTicksPerField(moduli);
    this.offsets = createOffsets(offsets);
  }

  /**
   * Creates the ticks per field array by multiplying up the moduli.
   * 
   * @param moduli the moduli from most significant field to least.
   * 
   * @return a newly created double array that contains the ticks per each field of the clock
   */
  @VisibleForTesting
  static double[] createTicksPerField(List<Double> moduli) {
    double[] result = new double[moduli.size()];

    /*
     * By definition the last field of the clock is a single tick.
     */
    result[result.length - 1] = 1.0;

    /*
     * Bootstrap the other values performing the appropriate multiplication.
     */
    for (int i = result.length - 2; i >= 0; i--) {
      result[i] = result[i + 1] * (moduli.get(i + 1));
    }

    return result;
  }

  /**
   * Converts the list of integers into an array of doubles.
   * 
   * @param offsets the list of integral offsets.
   * 
   * @return an array of doubles containing the converted values.
   */
  @VisibleForTesting
  static double[] createOffsets(List<Double> offsets) {
    double[] result = new double[offsets.size()];

    for (int i = 0; i < offsets.size(); i++) {
      result[i] = (offsets.get(i));
    }

    return result;
  }

  /**
   * Determines if the supplied SCLK has a valid list of clock fields for this clock.
   * 
   * @param sclk the sclk to test
   * 
   * @return true if the SCLK value is valid for this converter, false otherwise
   */
  @Override
  public boolean isValidClock(SCLK sclk) {

    if (sclk.getNumberOfFields() > ticksPerField.length) {
      return false;
    }

    for (int i = 0; i < sclk.getNumberOfFields(); i++) {
      if ((sclk.getField(i) - offsets[i]) < 0) {
        return false;
      }
    }

    return true;

  }

  /**
   * Converts the SCLK reading, sans partition, into a count of ticks past the initial clock
   * reading.
   * 
   * @param sclk the SCLK to convert to ticks
   * 
   * @return the number of ticks since the first reading of the clock
   */
  @Override
  public double convertToTicks(SCLK sclk) {

    if (sclk.getNumberOfFields() > ticksPerField.length) {
      throw new SCLKEvaluationException(clockID,
          "Unable to convert SCLK: [" + sclk + "] as it possesses " + sclk.getNumberOfFields()
              + " fields which exceeds the maximum amount permitted for this clock: "
              + ticksPerField.length);
    }

    double result = 0.0;

    for (int i = 0; i < sclk.getNumberOfFields(); i++) {
      result += removeOffset(sclk, i) * ticksPerField[i];
    }

    return result;
  }

  /**
   * Removes the offset to the field of interest, throwing an exception if the field value is out of
   * bounds.
   * 
   * @param sclk the SCLK value to apply the offset
   * @param fieldIndex the index of a particular field in which to apply the offset
   * 
   * @return the field value minus the offset
   * 
   * @throws SCLKEvaluationException if the field is out of range, i.e. less than offset.
   */
  @VisibleForTesting
  double removeOffset(SCLK sclk, int fieldIndex) {

    double result = sclk.getField(fieldIndex) - offsets[fieldIndex];

    if (result < 0) {
      throw new SCLKEvaluationException(clockID,
          "The field at index: " + fieldIndex + " in SCLK [" + sclk + "] is invalid.");
    }

    return result;

  }

  /**
   * Convert the supplied tick counts into an SCLK buffer, without adjusting the partition value.
   * 
   * @param ticks the raw ticks to convert
   * @param buffer the buffer to capture the fields
   * 
   * @return a reference to buffer for convenience
   */
  @Override
  public SCLK convertToSCLK(double ticks, SCLK buffer) {

    long[] fields = new long[ticksPerField.length];

    /*
     * The supplied value for ticks must be positive.
     */
    if (ticks < 0) {
      throw new SCLKEvaluationException(clockID, "Attempt to convert ticks: " + ticks
          + " to an SCLK value has failed.  Ticks must always be positive.");
    }

    double remainder = Math.round(ticks);

    /*
     * This expression emulates what is happening in the SPICE code that performs this
     * conversion.
     */
    for (int i = 0; i < ticksPerField.length - 1; i++) {
      fields[i] = (long) (((long) (remainder / ticksPerField[i])) + offsets[i]);
      remainder %= ticksPerField[i];
    }

    fields[fields.length - 1] = (long) (remainder + offsets[fields.length - 1]);

    buffer.setFields(fields);

    return buffer;
  }

  int getNumberOfFields() {
    return ticksPerField.length;
  }

  double getFieldModulus(int index) {
    return fieldModuli[index];
  }

  double getTicksPerField(int index) {
    return ticksPerField[index];
  }

  double getOffset(int index) {
    return offsets[index];
  }
}
