package picante.mechanics.providers.aberrated;

import static com.google.common.base.Preconditions.checkState;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.StateVector;
import picante.mechanics.StateVectorFunction;
import picante.mechanics.UnwritableStateVector;

class InertialLightTimeStateVectorFunction extends InertialLightTimePositionVectorFunction
    implements AberratedStateVectorFunction {

  private final double TOL = 1.0E-10;

  private final StateVectorFunction observer;
  private final StateVectorFunction target;

  InertialLightTimeStateVectorFunction(StateVectorFunction observer, StateVectorFunction target,
      AberrationCorrection correction, int numberOfIterations) {
    super(observer, target, correction, numberOfIterations);
    this.observer = observer;
    this.target = target;
  }

  @Override
  public StateVector getState(double time, StateVector buffer) {

    buffer = buffer == null ? new StateVector() : buffer;

    StateVector observer = new StateVector();
    StateVector target = new StateVector();

    /*
     * Compute the position of the observer at time relative to the inertial reference, the target
     * at the light time adjusted position relative to the inertial reference, and the target at the
     * light time relative to the observer at time. Note: this will only partially populate the
     * velocity components of buffer, as it neglects the light time derivative.
     */
    computeVectors(time, observer, target, buffer);

    /*
     * Compute the light time derivative, and correct the velocity components of buffer.
     */
    double dlt = computeLightTimeDerviative(target, buffer);
    VectorIJK.combine(1.0 + lightTimeSign * dlt, target.getVelocity(), -1.0, observer.getVelocity(),
        buffer.getVelocity());

    return buffer;
  }

  /**
   * Computes the observer relative to the reference point at time, the target relative to the
   * reference point at time adjusted for light time, and the target relative to the observer
   * without adjusting the velocity terms to account for the light time derivative.
   */
  private void computeVectors(double time, StateVector observer, StateVector target,
      StateVector buffer) {
    /*
     * Look up the position of the observer relative to the inertial reference. This vector remains
     * a constant during the computation, as time is specified at the observer.
     */
    this.observer.getState(time, observer);

    /*
     * And fetch the position of the target relative to the inertial reference. Subtract the
     * observer's position from it.
     */
    this.target.getState(time, target);
    StateVector.subtract(target, observer, buffer);

    double lightTime = buffer.getLength() / AberrationCorrection.SPEED_OF_LIGHT;

    /*
     * We've completed one iteration already, perform any additional iterations requested.
     */
    for (int i = 0; i < numberOfIterations; i++) {
      this.target.getState(time + lightTimeSign * lightTime, target);
      StateVector.subtract(target, observer, buffer);
      lightTime = buffer.getLength() / AberrationCorrection.SPEED_OF_LIGHT;
    }

  }

  /**
   * Compute the derivative of the light time with respect to time.
   * 
   * <p>
   * Derive the formula for this quantity for the reception case. Let:
   * <ul>
   * <li>POBS be the position of the observer relative to the inertial reference:
   * observer.getPosition()</li>
   * <li>VOBS be the velocity of the observer relative to the inertial reference:
   * observer.getVelocity()</li>
   * <li>PTARG be the position of the target relative to the inertial reference:
   * target.getPosition()</li>
   * <li>VTARG be the velocity of the target relative to the inertial reference:
   * target.getVelocity()</li>
   * <li>S be the sign of the light time correction. S is negative for the reception case.</li>
   * </ul>
   * </p>
   * <p>
   * The light-time corrected position of the target relative to the observer at observation time
   * ET, given the one-way light time LT is:
   * 
   * <pre>
   * PTARG(ET + S * LT) - POBS(ET)
   * </pre>
   * 
   * </p>
   * <p>
   * The light-time corrected velocity of the target relative to the observer at observation time ET
   * is:
   * 
   * <pre>
   * VTARG(ET + S * LT) * (1 + S * d(LT) / d(ET)) - VOBS(ET)
   * </pre>
   * 
   * </p>
   * <p>
   * We need to compute dLT/dt. Below, we use the facts that, for a time-dependent vector X(t),
   * 
   * <pre>
   *     ||X||     = <X,X> ** (1/2)
   * 
   *   d(||X||)/dt = (1/2)<X,X>**(-1/2) * 2 * <X,dX/dt>
   * 
   *               = <X,X>**(-1/2) *  <X,dX/dt>
   * 
   *               = <X,dX/dt> / ||X||
   * </pre>
   * 
   * Newtonian light time equation:
   * 
   * <pre>
   *        LT     =   (1/c) * || PTARG(ET+S*LT) - POBS(ET)||
   * </pre>
   * 
   * Differentiate both sides:
   * 
   * <pre>
   *        dLT/dt =   (1/c) * ( 1 / || PTARG(ET+S*LT) - POBS(ET) || )
   * 
   *                  * < PTARG(ET+S*LT) - POBS(ET),
   *                      VTARG(ET+S*LT)*(1+S*d(LT)/d(ET)) - VOBS(ET) >
   * 
   *               = (1/c) * ( 1 / || PTARG(ET+S*LT) - POBS(ET) || )
   * 
   *                 * (  < PTARG(ET+S*LT) - POBS(ET),
   *                        VTARG(ET+S*LT) - VOBS(ET) >
   *                        
   *                   +  < PTARG(ET+S*LT) - POBS(ET),
   *                        VTARG(ET+S*LT)           > * (S*d(LT)/d(ET))  )
   * </pre>
   * 
   * </p>
   * <p>
   * Let:
   * 
   * <pre>
   *  A =   (1/c) * ( 1 / || PTARG(ET+S*LT) - POBS(ET) || )
   * 
   *  B =   < PTARG(ET+S*LT) - POBS(ET), VTARG(ET+S*LT) - VOBS(ET) >
   * 
   *  C =   < PTARG(ET+S*LT) - POBS(ET), VTARG(ET+S*LT) >
   * </pre>
   * 
   * Then
   * 
   * <pre>
   *  d(LT)/d(ET) =  A * ( B  +  C * S*d(LT)/d(ET) )
   * </pre>
   * 
   * which implies
   * 
   * <pre>
   *  d(LT)/d(ET) =  A*B / ( 1 - S*C*A )
   * </pre>
   * 
   * </p>
   * 
   * @param target the state of the target relative to an inertial reference point in an inertial
   *        frame
   * @param buffer the state of the target relative to the observer expressed in the same inertial
   *        frame
   * @return the light time derivative
   */
  private double computeLightTimeDerviative(UnwritableStateVector target,
      UnwritableStateVector buffer) {

    double a = 1.0 / (AberrationCorrection.SPEED_OF_LIGHT * buffer.getLength());

    double b = buffer.getPosition().getDot(buffer.getVelocity());

    double c = buffer.getPosition().getDot(target.getVelocity());

    checkState(lightTimeSign * c * a <= 1.0 - TOL,
        "Target range rate maximum is approaching the speed of"
            + " light. Unable to estimate light time derivative.");

    return (a * b) / (1.0 - lightTimeSign * c * a);

  }

  @Override
  public double getLightTimeDerivative(double time) {

    StateVector target = new StateVector();
    StateVector buffer = new StateVector();

    /*
     * Compute the position of the observer at time relative to the inertial reference, the target
     * at the light time adjusted position relative to the inertial reference, and the target at the
     * light time relative to the observer at time. Note: this will only partially populate the
     * velocity components of buffer, as it neglects the light time derivative.
     */
    computeVectors(time, new StateVector(), target, buffer);

    /*
     * Compute the light time derivative, and correct the velocity components of buffer.
     */
    return computeLightTimeDerviative(target, buffer);

  }

}
