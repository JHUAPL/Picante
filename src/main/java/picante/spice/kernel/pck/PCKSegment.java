package picante.spice.kernel.pck;

import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.Coverage;
import picante.mechanics.StateTransform;

/**
 * Interface describing the elemental rotation unit in a PCK file.
 */
public interface PCKSegment {

  /**
   * Returns the name assigned to the segment.
   * 
   * @return a string containing the name
   */
  public String getName();

  /**
   * Returns the NAIF integer ID code for the body frame.
   * 
   * @return body frame ID.
   */
  public int getBodyFrameID();

  /**
   * Returns the NAIF integer ID code for the reference frame.
   * 
   * @return reference frame ID.
   */
  public int getReferenceFrameID();

  /**
   * Returns the PCK integer type code for the segment.
   * 
   * @return type ID
   */
  public int getType();

  /**
   * Retrieves the coverage of the segment.
   * 
   * @return a reference to the coverage for the instance.
   */
  public Coverage getCoverage();

  /**
   * Computes the rotation of the body frame relative to the reference frame specified by the
   * segment.
   * 
   * @param time ephemeris seconds past J2000 to evaluate the rotation at
   * 
   * @param buffer a <code>RotationMatrixIJK</code> to receive the results
   * 
   * @return reference to buffer for convenience
   */
  public RotationMatrixIJK getTransform(double time, RotationMatrixIJK buffer);

  /**
   * Computes the state transform of the body frame relative to the reference frame specified by the
   * segment.
   * 
   * @param time ephemeris seconds past J2000 to evaluate the transform at
   * 
   * @param buffer a {@link RotationMatrixIJK} to receive the results
   * 
   * @return reference to buffer for convenience
   */
  public StateTransform getTransform(double time, StateTransform buffer);

}
