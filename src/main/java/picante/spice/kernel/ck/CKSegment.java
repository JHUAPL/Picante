package picante.spice.kernel.ck;

import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.Coverage;

/**
 * Interface describing the elemental attitude unit in a CK file.
 */
public interface CKSegment {

  /**
   * Returns the name assigned to the segment.
   * 
   * @return a string containing the name
   */
  public String getName();

  /**
   * Returns the NAIF integer ID code for the instrument frame.
   * 
   * @return the ID of the frame to which the segment transforms vectors
   */
  public int getInstrumentID();

  /**
   * Returns the NAIF integer ID code for the reference frame.
   * 
   * @return the ID of the frame from which the segment transforms vectors
   */
  public int getReferenceID();

  /**
   * A boolean indicating whether the segment includes angular velocity information.
   * 
   * @return true if the segment has angular velocity, false otherwise.
   */
  public boolean hasAngularVelocity();

  /**
   * Returns the CK integer type code for the segment.
   * 
   * @return type ID
   */
  public int getType();

  /**
   * Retrieves the first valid encoded SCLK for the segment.
   * 
   * @return a double containing the encoded SCLK
   */
  public double getInitialEncodedSCLK();

  /**
   * Retrieves the last valid encoded SCLK for the segment.
   * 
   * @return a double containing the encoded SCLK
   */
  public double getFinalEncodedSCLK();

  /**
   * Retrieves the coverage associated with the C-kernel.
   * 
   * @return an implementation of the {@link Coverage} interface that operates on encoded SCLK
   *         associated with the CK segment. <b>This is not ephemeris time as the
   *         <code>Coverage</code> interface would usually provide.</b>
   */
  public CKCoverage getCoverage();

  /**
   * Computes the rotation from the frame specified by the reference ID to the frame specified by
   * the instrument ID at the requested encoded SCLK time.
   * 
   * @param encodedSCLK a valid encoded SCLK at which the rotation is requested
   * @param buffer a <code>RotationMatrixIJK</code> to capture the results
   * 
   * @return a reference to buffer for convenience
   */
  public RotationMatrixIJK getTransform(double encodedSCLK, RotationMatrixIJK buffer);
}
