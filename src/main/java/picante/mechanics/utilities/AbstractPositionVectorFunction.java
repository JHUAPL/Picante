package picante.mechanics.utilities;

import picante.mechanics.Coverage;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;
import picante.mechanics.PositionVectorFunction;

/**
 * Abstract implementation of the {@link PositionVectorFunction} interface that provides coverage
 * and ID access via references provided to the constructor.
 */
public abstract class AbstractPositionVectorFunction implements PositionVectorFunction {

  private final EphemerisID targetID;
  private final EphemerisID observerID;
  private final FrameID frameID;
  private final Coverage coverage;

  /**
   * Creates the abstract function capturing references to the supplied IDs and coverage.
   * 
   * @param targetID the ephemeris ID for the target object
   * @param observerID the ephemeris ID for the observer object
   * @param frameID the frame ID for the frame in which this vector function is expressed
   * @param coverage the coverage over which this vector function can be evaluated
   */
  public AbstractPositionVectorFunction(EphemerisID targetID, EphemerisID observerID,
      FrameID frameID, Coverage coverage) {
    super();
    this.targetID = targetID;
    this.observerID = observerID;
    this.frameID = frameID;
    this.coverage = coverage;
  }

  @Override
  public EphemerisID getObserverID() {
    return observerID;
  }

  @Override
  public EphemerisID getTargetID() {
    return targetID;
  }

  @Override
  public FrameID getFrameID() {
    return frameID;
  }

  @Override
  public Coverage getCoverage() {
    return coverage;
  }

}
