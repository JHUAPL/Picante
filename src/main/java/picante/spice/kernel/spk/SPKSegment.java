package picante.spice.kernel.spk;

import picante.math.vectorspace.VectorIJK;
import picante.mechanics.Coverage;
import picante.mechanics.StateVector;

/**
 * Interface describing the elemental ephemeris unit in an SPK file.
 */
public interface SPKSegment {

  /**
   * Returns the name assigned to the segment.
   * 
   * @return a string containing the name
   */
  public String getName();

  /**
   * Returns the NAIF integer ID code for the target object.
   * 
   * @return target object ID.
   */
  public int getTargetID();

  /**
   * Returns the NAIF integer ID code for the observer object.
   * 
   * @return observer object ID
   */
  public int getObserverID();

  /**
   * Returns the NAIF integer frame ID code for the frame in which the target to observer
   * relationship is specified by the segment.
   * 
   * @return frame ID
   */
  public int getFrameID();

  /**
   * Returns the SPK integer type code for the segment.
   * 
   * @return type ID
   */
  public int getType();

  /**
   * Retrieves the coverage of the segment.
   * 
   * @return a reference to the coverage for the instance
   */
  public Coverage getCoverage();

  /**
   * Computes the position of the target relative to the observer in the native frame specified by
   * the segment.
   * 
   * @param time ephemeris seconds past J2000 to evaluate the position at
   * 
   * @param buffer a <code>VectorIJK</code> to receive the results
   * 
   * @return a reference to buffer for convenience
   */
  public VectorIJK getPosition(double time, VectorIJK buffer);

  /**
   * Computes the state of the target relative to the observer in the native frame specified by the
   * segment.
   * 
   * @param time ephemeris seconds past J2000 to evaluate the position at
   * 
   * @param buffer a <code>StateVector</code> to receive the results
   * 
   * @return a reference to buffer for convenience
   */
  public StateVector getState(double time, StateVector buffer);

}
