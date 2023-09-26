package picante.spice.kernel.ck;

import picante.math.vectorspace.VectorIJK;

/**
 * An extension of the <code>CKSegment</code> interface that includes a method to compute the
 * angular rate in addition to the rotation.
 */
public interface CKSegmentWithAV extends CKSegment {

  /**
   * Computes the right-handed angular rate vector, in radians per second, expressed in the frame
   * specified by the reference ID at the requested encoded SCLK.
   * 
   * @param encodedSCLK a valid encoded SCLK at which the rotation is requested
   * @param buffer a <code>VectorIJK</code> to capture the results
   * 
   * @return a reference to buffer for convenience
   */
  public VectorIJK getAngularRate(double encodedSCLK, VectorIJK buffer);

}
