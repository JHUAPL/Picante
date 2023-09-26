package picante.spice.daf;

/**
 * NAIF double precision array file interface.
 * <p>
 * This interface defines the methods necessary to access the contents of a NAIF double precision
 * array file. These files conceptually are an ordered list of <code>DAFSegment</code>s and meta
 * data describing this list. Specifically:
 * </p>
 * <ul>
 * <li>The DAF ID word, typically an 8 character string that identifies the type of the DAF
 * file.</li>
 * <li>An internal filename, that serves a similar purpose to an actual filename</li>
 * <li>A list of reserved records, a block of bytes that live outside the DAF system, but are
 * present for usage in particular types of DAF</li>
 * <li>An ordered list of segments, conceptually containing integer and double precision meta data
 * as well as an ordered list of double precision values
 * </ul>
 * <p>
 * The specifics of how the interface access the data content is left up to the implementation. Each
 * method that access data or metadata may throw the listed runtime exceptions.
 * </p>
 * <p>
 * Note: NAIF's reference implementation provides access to the data content via a doubly linked
 * list. This interface hides the details of this implementation.
 * </p>
 * <p>
 * The number of metadata elements for a standard NAIF DAF file are constrained in the following
 * manner: (Note: ND refers to the number of double precision components and NI refers to the number
 * of integer components.)
 * </p>
 * <ul>
 * <li>0 &lt;= ND &lt;= 124</li>
 * <li>0 &lt;= NI &lt;= 248</li>
 * <li>ND + (NI+1)/2 &lt;= 125</li>
 * </ul>
 * <p>
 * For those familiar with NAIF's implementation of DAF, there are two additional integer metadata
 * components describing the address range in the file to locate the actual segment data content. As
 * this interface is an attempt to abstract the details of the implementation, these addresses are
 * hidden from the user entirely by the interface.
 * </p>
 */
public interface DAF {

  /**
   * Retrieves the ID word of the DAF.
   * 
   * @return a <code>String</code> containing the ID word present in the DAF
   * 
   * @throws DAFAccessException if retrieving the ID from the DAF fails for any reason
   */
  public String getID();

  /**
   * Retrieves the internally stored file name in the DAF
   * 
   * @return a <code>String</code> containing the internal filename
   * 
   * @throws DAFAccessException if retrieving the internal name of the DAF fails for any reason
   */
  public String getName();

  /**
   * Retrieves the entire block of reserved records from the DAF.
   * 
   * @return a newly created array of bytes containing the contents of the reserved records in the
   *         DAF
   * 
   * @throws DAFAccessException if the reserved record content retrieval fails for any reason
   */
  public byte[] getReservedRecords();

  /**
   * Retrieves the size of the DAF.
   * 
   * @return the number of segments present in the DAF
   * 
   * @throws DAFAccessException if retrieving the number of segments in the DAF fails for any reason
   */
  public int getSize();

  /**
   * Retrieves a <code>DAFSegment</code> from the DAF.
   * 
   * @param index the index of the segment of interest, must be between 0 and getSize()-1 inclusive
   * 
   * @return the segment of interest
   * 
   * @throws IndexOutOfBoundsException if the requested index lies outside the supported range of
   *         the DAF
   * 
   * @throws DAFAccessException if retrieving the requested segment fails for any reason other than
   *         an invalid index argument
   */
  public DAFSegment getSegment(int index);
}
