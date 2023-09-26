package picante.spice.adapters;

import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.Coverage;
import picante.mechanics.FrameID;
import picante.mechanics.FrameTransformFunction;
import picante.spice.kernel.ck.CKSegment;
import picante.spice.kernel.tk.sclk.EncodedSCLKConverter;

/**
 * Adapts a CK segment to the <code>FrameTransformFunction</code> interface.
 * 
 * @see CKSegmentWithAVAdapter
 */
class CKSegmentAdapter implements FrameTransformFunction {

  /**
   * The frame ID associated with the from side of the transformation.
   */
  private final FrameID fromID;

  /**
   * The frame ID associated with the to side of the transformation.
   */
  private final FrameID toID;

  /**
   * A reference to the actual CK segment being adapted.
   */
  private final CKSegment segment;

  /**
   * The encoded SCLK converter used to move between TDB and the SCLK based time system native to
   * the CK data.
   */
  private final EncodedSCLKConverter converter;

  /**
   * An adapted coverage object that presents the C-kernel segment's coverage in a TDB fashion.
   */
  private final TdbCoverage coverage;

  /**
   * Creates an adapter from the supplied ID codes, SCLK converter, and segment.
   * 
   * @param fromID the FrameID from which vectors are transformed
   * @param toID the FrameID to which vectors are transformed
   * @param converter the SCLK converter connecting back to TDB
   * @param segment the segment
   */
  protected CKSegmentAdapter(FrameID fromID, FrameID toID, EncodedSCLKConverter converter,
      CKSegment segment) {
    this.fromID = fromID;
    this.toID = toID;
    this.segment = segment;
    this.converter = converter;
    this.coverage = new TdbCoverage(segment.getCoverage(), this.converter);
  }

  @Override
  public Coverage getCoverage() {
    return coverage;
  }

  @Override
  public FrameID getFromID() {
    return fromID;
  }

  @Override
  public FrameID getToID() {
    return toID;
  }

  @Override
  public RotationMatrixIJK getTransform(double time, RotationMatrixIJK buffer) {

    buffer = buffer == null ? new RotationMatrixIJK() : buffer;

    segment.getTransform(converter.convertToEncodedSclk(time), buffer);

    /*
     * Invert the resultant transformation, since the C-kernel provides a transform that takes
     * vectors from the toID frame (reference) to the fromID frame (instrument).
     */
    return buffer.transpose();
  }

}
