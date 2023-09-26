package picante.mechanics.utilities;

import picante.mechanics.StateVector;
import picante.mechanics.StateVectorFunction;

/**
 * adapts an existing {@link StateVectorFunction} by rescaling the length of the resulting position
 * and velocity vectors
 * 
 * @author vandejd1
 */
@Deprecated
public class ScaledStateVectorFunction extends ScaledPositionVectorFunction
    implements StateVectorFunction {
  final StateVectorFunction srcFunc;
  final double scaleFactor;

  public ScaledStateVectorFunction(StateVectorFunction srcFunc, double scaleFactor) {
    super(srcFunc, scaleFactor);
    this.srcFunc = srcFunc;
    this.scaleFactor = scaleFactor;
  }

  @Override
  public StateVector getState(double time, StateVector buffer) {
    srcFunc.getState(time, buffer);
    buffer.getVelocity().scale(scaleFactor);
    buffer.getPosition().scale(scaleFactor);
    return buffer;
  }

}
