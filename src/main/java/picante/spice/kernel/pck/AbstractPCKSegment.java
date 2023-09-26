package picante.spice.kernel.pck;

import picante.mechanics.Coverage;
import picante.mechanics.Coverages;

/**
 * Abstract class providing many of the common meta data focused features of the PCK segment
 * classes.
 */
abstract class AbstractPCKSegment implements PCKSegment {

  private final Coverage coverage;
  private final int bodyFrameID;
  private final int referenceFrameID;
  private final String name;

  /**
   * Constructs an abstract PCK segment from the supplied meta data.
   * 
   * @param name the name of the PCK segment
   * @param bodyFrameID the integer ID code of the body frame this segment describes. This is the
   *        &quot;to&quot; frame of the segment transformation.
   * @param referenceFrameID the integer ID code of the reference frame to which the rotation is
   *        relative; i.e. the &quot;from&quot; frame.
   * @param startET seconds past J2000.0 in TDB indicating the start of the segment's applicability.
   * @param finalET seconds past J2000.0 in TDB indicating the end of the segment's applicability.
   */
  public AbstractPCKSegment(String name, int bodyFrameID, int referenceFrameID, double startET,
      double finalET) {
    this.name = name;
    this.coverage = Coverages.create(startET, finalET);
    this.bodyFrameID = bodyFrameID;
    this.referenceFrameID = referenceFrameID;
  }

  @Override
  public int getBodyFrameID() {
    return bodyFrameID;
  }

  @Override
  public Coverage getCoverage() {
    return coverage;
  }

  @Override
  public String getName() {
    return name;
  }

  @Override
  public int getReferenceFrameID() {
    return referenceFrameID;
  }

  @Override
  public abstract int getType();

}
