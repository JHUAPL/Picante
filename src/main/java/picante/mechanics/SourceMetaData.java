package picante.mechanics;

/**
 * Interface describing common methods associated with meta data for a frame or ephemeris source.
 */
public interface SourceMetaData {

  /**
   * Retrieves the name of the source.
   * 
   * @return A <code>String</code> containing the name: filename, database name, etc. of a
   *         particular source.
   */
  public String getName();

}
