package picante.spice.adapters;

import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.FrameID;
import picante.mechanics.StateTransform;
import picante.mechanics.StateTransformFunction;
import picante.mechanics.rotations.MatrixWrapper;
import picante.mechanics.rotations.WrapperWithRate;
import picante.spice.kernel.ck.CKSegmentWithAV;
import picante.spice.kernel.tk.sclk.EncodedSCLKConverter;

/**
 * Extension of the {@link CKSegmentAdapter} class that implements the
 * <code>StateTransformFunction</code> for CK segments that possess angular velocity.
 */
class CKSegmentWithAVAdapter extends CKSegmentAdapter implements StateTransformFunction {

  /**
   * The SCLK converter to be used in time conversion.
   */
  private final EncodedSCLKConverter converter;

  /**
   * The segment to adapt.
   */
  private final CKSegmentWithAV segment;

  /**
   * A rotation matrix buffer used for creating the state transform from the angular velocity and
   * rotation natively supported by the C-kernel.
   */
  private final RotationMatrixIJK rBuffer = new RotationMatrixIJK();

  /**
   * The actual implementation of the <code>DifferentiableRotation</code> interface to perform the
   * state transformation.
   */
  private final WrapperWithRate<MatrixWrapper> transformer =
      new WrapperWithRate<MatrixWrapper>(new MatrixWrapper(rBuffer));

  /**
   * Creates an adapted from the supplied ID codes, SCLK converter, and segment.
   * 
   * @param fromID the FrameID from which vectors are transformed
   * @param toID the FrameID to which vectors are transformed
   * @param converter the SCLK converter connecting back to TDB
   * @param segment the segment
   */
  CKSegmentWithAVAdapter(FrameID fromID, FrameID toID, EncodedSCLKConverter converter,
      CKSegmentWithAV segment) {
    super(fromID, toID, converter, segment);
    this.converter = converter;
    this.segment = segment;
  }

  @Override
  public StateTransform getStateTransform(double time, StateTransform buffer) {
    buffer = buffer == null ? new StateTransform() : buffer;
    /*
     * This is a bit convoluted, as the transformer class is to be used to produce the state
     * transform of interest. Access control to the rotation matrix is intentionally circumvented in
     * the name of performance. It is safe, as these details never escape this private class.
     */
    time = converter.convertToEncodedSclk(time);
    segment.getTransform(time, rBuffer);
    segment.getAngularRate(time, transformer.getRate());
    transformer.getTransform(buffer);

    /*
     * Invert the resultant state transformation, since the C-kernel provides a state transform that
     * takes vectors from the toID frame (reference) to the fromID frame (instrument).
     */
    return buffer.invert();
  }
}
