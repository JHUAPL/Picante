package picante.spice.kernel.tk.pck;

public interface TimeConverter {

  /**
   * TimeConverter that simply returns the supplied et, performing no conversion.
   */
  public static final TimeConverter NO_CONVERSION = new TimeConverter() {
    @Override
    public double computeEvaluationTime(double et) {
      return et;
    }
  };

  /**
   * Convert the standard ephemeris time into the appropriate time base for the polynomial
   * evaluation.
   * 
   * @param et TDB seconds past J2000 of interest.
   * 
   * @return a count of seconds since the polynomial reference time
   */
  public double computeEvaluationTime(double et);

}
