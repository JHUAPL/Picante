package picante.mechanics.providers.aberrated;

import static com.google.common.base.Preconditions.checkNotNull;
import picante.mechanics.StateVector;
import picante.mechanics.StateVectorFunction;

class GeometricStateVectorFunction extends GeometricPositionVectorFunction
    implements AberratedStateVectorFunction {

  private final StateVectorFunction delegate;

  GeometricStateVectorFunction(StateVectorFunction delegate) {
    super(delegate);
    this.delegate = checkNotNull(delegate);
  }

  @Override
  public StateVector getState(double time, StateVector buffer) {
    return delegate.getState(time, buffer);
  }

  /**
   * {@inheritDoc}
   * 
   * @throws UnsupportedOperationException as geometric states are unable to provide light time
   *         derivatives
   */
  @Override
  public double getLightTimeDerivative(@SuppressWarnings("unused") double time) {
    throw new UnsupportedOperationException(
        "Geometric states can not provide light time or its derivative.");
  }
}
