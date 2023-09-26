package picante.mechanics;

/**
 * Interface defining an ephemeris ID code, used to reference specific frames in the mechanics
 * package.
 * <p>
 * TODO: Mention the equals() comparision method vs. ==, as this will be important. TODO: Mention
 * the immutability requirement that could orphan entries in a hash table
 * </p>
 */
public interface EphemerisID {

  /**
   * Obtain the name of the object associated with this ID.
   * 
   * @return the name
   */
  public String getName();

}
