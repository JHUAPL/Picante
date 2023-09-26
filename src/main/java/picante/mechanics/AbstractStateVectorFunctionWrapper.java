package picante.mechanics;

/**
 * Trivial extension of the {@link AbstractPositionVectorFunctionWrapper} that incorporates the
 * {@link StateVectorFunction} interface.
 */
abstract class AbstractStateVectorFunctionWrapper extends AbstractPositionVectorFunctionWrapper
    implements StateVectorFunction {

  /**
   * Constructs the delegating view.
   * 
   * @param function the position vector function to delegate for coverage and various IDs.
   */
  public AbstractStateVectorFunctionWrapper(PositionVectorFunction function) {
    super(function);
  }

}
