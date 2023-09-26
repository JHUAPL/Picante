package picante.data.list;

/**
 * Simple expression of the gauged retrieval method.
 * <p>
 * This interface exists purely to allow functionality from the {@link Indexable} and
 * {@link Retrievable} inheritance trees to share code.
 * </p>
 * 
 */
public interface Gauged {

  /**
   * Retrieve the gauge (time) associated with a record at a particular index.
   * 
   * @param index the index of interest
   * 
   * @return the double capturing the measured value against which records are recorded, typically
   *         time.
   * 
   * @throws IndexOutOfBoundsException if the index lies outside the range of acceptable values
   *         supporting the instance
   */
  double getGauge(int index);

}
