package picante.spice.kernel.spk;

import picante.math.vectorspace.VectorIJK;
import picante.mechanics.StateVector;
import picante.mechanics.UnwritableStateVector;

class SPKType5Evaluator {

  private final TwoBodyPropagator forward = new TwoBodyPropagator();
  private final TwoBodyPropagator backward = new TwoBodyPropagator();

  private boolean useBoth = true;
  private final StateVector forwardState = new StateVector();
  private final StateVector backwardState = new StateVector();
  private final VectorIJK vector = new VectorIJK();

  void configure(double gm, double leftTime, UnwritableStateVector leftState, double rightTime,
      UnwritableStateVector rightState) {
    useBoth = leftTime != rightTime;
    forward.configure(gm, leftTime, leftState);
    if (useBoth) {
      backward.configure(gm, rightTime, rightState);
    }
  }

  StateVector evaluate(double time, StateVector buffer) {

    if (!useBoth) {
      return forward.getState(time, buffer);
    }

    forward.getState(time, forwardState);
    backward.getState(time, backwardState);

    double numer = time - forward.getT0();
    double denom = backward.getT0() - forward.getT0();
    double arg = numer * Math.PI / denom;
    double dargdt = Math.PI / denom;

    double w = 0.5 + 0.5 * Math.cos(arg);
    double dwdt = -0.5 * Math.sin(arg) * dargdt;

    StateVector.combine(w, forwardState, 1.0 - w, backwardState, buffer);
    VectorIJK.combine(dwdt, forwardState.getPosition(), -dwdt, backwardState.getPosition(), vector);
    VectorIJK.add(vector, buffer.getVelocity(), buffer.getVelocity());
    return buffer;
  }

}


