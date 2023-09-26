package picante.roots;

import org.apache.commons.math3.analysis.solvers.BrentSolver;

import com.google.common.base.Supplier;

/**
 * Package private implementation of a supplier of {@link BrentSolver}s given the specified relative
 * and absolute accuracies.
 * <p>
 * At some point in the future, we'll break the direct dependency on the commons-math API by
 * inserting our own solver interface and providing the necessary adapters. For now, as this is
 * literally an implementation detail it remains as is.
 * </p>
 */
class BrentSolverSupplier implements Supplier<BrentSolver> {

  private final double absoluteAccuracy;
  private final double relativeAccuracy;

  /**
   * Creates a new {@link BrentSolver} supplier.
   * 
   * @param relativeAccuracy the relative accuracy
   * @param absoluteAccuracy the absolute accuracy
   * 
   * @see BrentSolver#BrentSolver(double, double)
   */
  BrentSolverSupplier(double relativeAccuracy, double absoluteAccuracy) {
    super();
    this.absoluteAccuracy = absoluteAccuracy;
    this.relativeAccuracy = relativeAccuracy;
  }

  @Override
  public BrentSolver get() {
    return new BrentSolver(relativeAccuracy, absoluteAccuracy);
  }

}
