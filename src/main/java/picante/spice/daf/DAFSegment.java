package picante.spice.daf;

/**
 * Double precision array file (DAF) segment interface.
 * <p>
 * This interface defines the methods necessary to access the contents of a double precision array
 * file's data content. Double precision array file, or DAF, segments consist of four data
 * components:
 * </p>
 * <ul>
 * <li>A terse character description of the segment referred to as the segment's name.</li>
 * <li>An array of doubles containing meta data that describes the content or intended usage of the
 * segment.</li>
 * <li>An array of integers containing meta data that describes the content or intended usage of the
 * segment.</li>
 * <li>The data content of the segment, an array of doubles.</li>
 * </ul>
 * <p>
 * The specifics of how the interface accesses the data content is left up to the implementation.
 * </p>
 */
public interface DAFSegment {

  /**
   * Fetch the segment name.
   * 
   * @return a <code>String</code> containing the segment name.
   * 
   * @throws DAFAccessException if retrieving the name of the segment fails for any reason
   */
  public String getName();

  /**
   * Fetch an element of the double precision metadata.
   * 
   * @param index index of the requested element, must be between 0 and getND()-1 inclusive.
   * 
   * @return the requested metadata element
   * 
   * @throws IndexOutOfBoundsException if the requested index lies outside the supported range of
   *         the metadata
   * 
   * @throws DAFAccessException if retrieving the metadata content fails for any reason other than
   *         issues with the requested index
   */
  public double getDoubleComponent(int index);

  /**
   * Fetch an element of the integer metadata.
   * 
   * @param index the index of the requested element, must be between 0 and getNI()-1 inclusive.
   * 
   * @return the requested metadata element
   * 
   * @throws IndexOutOfBoundsException if the requested index lies outside the supported range of
   *         the metadata
   * 
   * @throws DAFAccessException if retrieving the metadata content fails for any reason other than
   *         issues with the requested index
   */
  public int getIntComponent(int index);

  /**
   * Retrieves the number of double precision metadata elements
   * 
   * @return the number of elements
   * 
   * @throws DAFAccessException if retrieving the number of doubles in the metadata fails for any
   *         reason
   */
  public int getND();

  /**
   * Retrieves the number of integer metadata elements
   * 
   * @return the number of elements
   * 
   * @throws DAFAccessException if retrieving the number of integers in the metadata fails for any
   *         reason
   */
  public int getNI();

  /**
   * Copies a sequence of elements from the data array of the segment into the supplied buffer
   * 
   * @param index the starting index, must be between 0 and getLength()-1 inclusive.
   * @param buffer a double precision array to receive the results
   * @param offset the offset in buffer at which to begin placing values from the segment
   * @param length the number of elements to retrieve from the array captured in the segment. (index
   *        + length) must not exceed getLength().
   * 
   * @throws IndexOutOfBoundsException if the requested index range lies outside the range supported
   *         by the DAF
   * 
   * @throws DAFAccessException if retrieving the requested data fails for any reason other than
   *         indexing issues
   */
  public void get(int index, double[] buffer, int offset, int length);

  /**
   * Retrieves the number of elements in the double precision array contained within this segment
   * 
   * @return the number of elements
   * 
   * @throws DAFAccessException if retrieving the number of data elements in the segment fails for
   *         any reason
   */
  public int getLength();

}
