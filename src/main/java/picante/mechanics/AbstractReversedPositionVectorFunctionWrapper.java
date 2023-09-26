package picante.mechanics;

/**
 * Provides a basic wrapper or view of an existing position vector function, but inverts the sense
 * of the observer and target ID. This is particularly useful for implementing negated vector
 * functions.
 */
abstract class AbstractReversedPositionVectorFunctionWrapper implements PositionVectorFunction {

  private final PositionVectorFunction function;

  /**
   * Creates a delegating wrapper, with the to and from IDs reversed.
   * 
   * @param function the function to delegate coverage and IDs to
   */
  public AbstractReversedPositionVectorFunctionWrapper(PositionVectorFunction function) {
    this.function = function;
  }

  @Override
  public EphemerisID getObserverID() {
    return function.getTargetID();
  }

  @Override
  public EphemerisID getTargetID() {
    return function.getObserverID();
  }

  @Override
  public FrameID getFrameID() {
    return function.getFrameID();
  }

  @Override
  public Coverage getCoverage() {
    return function.getCoverage();
  }

}
