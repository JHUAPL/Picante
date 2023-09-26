package picante.roots;

import static com.google.common.base.Preconditions.checkArgument;

/**
 * Collection of methods creating and operating on {@link Stepper} implementations.
 */
public class Steppers {

  private Steppers() {}

  /**
   * Creates a {@link Stepper} implementation that always returns a constant size.
   * 
   * @param stepSize the size of the step to return
   * 
   * @return the implementation of {@link Stepper} that always returns stepSize
   */
  public static Stepper createConstant(final double stepSize) {
    checkArgument(stepSize > 0, "Constant step size must be strictly positive. Was %s", stepSize);

    return new Stepper() {

      @Override
      public double step(@SuppressWarnings("unused") double x) {
        return stepSize;
      }

    };
  }
}
