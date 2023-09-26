package picante.mechanics;

/**
 * Trivial extension of the {@link AbstractReversedPositionVectorFunctionWrapper} that incorporates
 * the {@link StateVectorFunction} interface
 */
abstract class AbstractReversedStateVectorFunctionWrapper
    extends AbstractReversedPositionVectorFunctionWrapper implements StateVectorFunction {

  /**
   * Creates the delegate wrapper with to and from IDs reversed.
   * 
   * @param function the function to delegate to
   */
  public AbstractReversedStateVectorFunctionWrapper(PositionVectorFunction function) {
    super(function);
  }

}
