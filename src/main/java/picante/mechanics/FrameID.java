package picante.mechanics;

/**
 * Interface defining a frame ID code, used to reference specific frames in the mechanics package.
 * <p>
 * TODO: Mention the equals() comparison method, vs. ==, as this will be important.
 * </p>
 */
public interface FrameID {

  /**
   * Obtain the name of the frame associated with this ID.
   * 
   * @return a string with the canonical representation of the name of the frame associated with
   *         this ID.
   */
  public String getName();

  /**
   * Determines if the frame identified by this code is an inertial frame or not.
   * <p>
   * <b>Note:</b> This is just an optimization hint, indicating to the frame system that, because
   * the frame is inertial, certain shortcuts with regards to computation of states can be taken.
   * So, it is possible that an inertial frame code <b>might</b> return <code>false</code>.
   * </p>
   * 
   * @return true, if it is inertial, false otherwise
   */
  public boolean isInertial();
}
