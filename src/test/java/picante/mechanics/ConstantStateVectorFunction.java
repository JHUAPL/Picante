package picante.mechanics;

/**
 * Simple implementation of the <code>StateVectorFunction</code> interface that supplies a constant
 * state vector over all valid times.
 */
public class ConstantStateVectorFunction extends ConstantPositionVectorFunction
    implements StateVectorFunction {

  private final StateVector state = new StateVector();

  public ConstantStateVectorFunction(EphemerisTestCodes targetID, EphemerisTestCodes observerID,
      FrameTestCodes frameID, Coverage coverage, UnwritableStateVector state) {
    super(targetID, observerID, frameID, coverage, state.position);
    this.state.setTo(state);
  }

  @Override
  public StateVector getState(double time, StateVector buffer) {
    if (!getCoverage().contains(time)) {
      throw new EphemerisEvaluationException();
    }

    return buffer.setTo(state);
  }

}
