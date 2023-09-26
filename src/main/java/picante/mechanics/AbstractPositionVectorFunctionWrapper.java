package picante.mechanics;

/**
 * Provides a basic wrapper or view of an existing position vector function, but leaves the vector
 * evaluation method unspecified.
 */
abstract class AbstractPositionVectorFunctionWrapper implements PositionVectorFunction {

  private final PositionVectorFunction function;

  /**
   * Constructs the delegation for coverage and IDs.
   * 
   * @param function the function to delegate to
   */
  public AbstractPositionVectorFunctionWrapper(PositionVectorFunction function) {
    this.function = function;
  }

  @Override
  public EphemerisID getObserverID() {
    return function.getObserverID();
  }

  @Override
  public EphemerisID getTargetID() {
    return function.getTargetID();
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
