package picante.spice.kernel.spk;

import picante.mechanics.Coverage;
import picante.mechanics.Coverages;

/**
 * Abstract class providing many of the common meta data focused features of the SPK segment
 * classes.
 */
abstract class AbstractSPKSegment implements SPKSegment {

  private final Coverage coverage;
  private final int frameID;
  private final int targetID;
  private final int observerID;
  private final String name;

  /**
   * Constructs an abstract SPK segment from the supplied meta data.
   * 
   * @param name the name of the SPK segment
   * @param targetID the integer ID code of the target, the head of the ephemeris vector
   * @param observerID the integer ID code of the observer, the tail of the ephemeris vector
   * @param frameID the integer ID code of the frame in which the ephemeris vector is expressed
   * @param startET seconds past J2000.0 in TDB indicating the start of the segment's applicability
   * @param finalET seconds past J2000.0 in TDB indicating the end of the segment's applicability
   */
  public AbstractSPKSegment(String name, int targetID, int observerID, int frameID, double startET,
      double finalET) {
    this.name = name;
    this.coverage = Coverages.create(startET, finalET);
    this.frameID = frameID;
    this.observerID = observerID;
    this.targetID = targetID;
  }

  @Override
  public String getName() {
    return name;
  }

  @Override
  public Coverage getCoverage() {
    return coverage;
  }

  @Override
  public int getFrameID() {
    return frameID;
  }

  @Override
  public int getObserverID() {
    return observerID;
  }

  @Override
  public int getTargetID() {
    return targetID;
  }

  @Override
  public abstract int getType();

}
