package picante.data.list;

/**
 * Basic interface for describing a list of indexed records that are retrieved with a mutable buffer
 * pattern.
 * 
 * @param <R> the type of the record required by the implementation
 */
public interface Retrievable<R> {

  /**
   * Retrieve a particular record from the list
   * 
   * @param index the index of interest
   * 
   * @param buffer the record buffer to receive the results
   * 
   * @return a reference to buffer for convenience
   * 
   * @throws IndexOutOfBoundsException if index lies outside the acceptable range supported by the
   *         instance: [0, {@link Sizeable#size()}-1].
   */
  R get(int index, R buffer);

  /**
   * Get the size of the list
   * 
   * @return the number of records in the list
   */
  public int size();
  
}
