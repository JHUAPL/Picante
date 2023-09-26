package picante.math.cones;

import static com.google.common.base.Preconditions.checkArgument;
import static picante.units.FundamentalPhysicalConstants.TWOPI;
import picante.designpatterns.Builder;

class ProjectionBuilders {

  private ProjectionBuilders() {}

  /**
   * Creates a simple cylindrical projection that covers the entire body in longitude.
   * 
   * <pre>
   *         +========================================================+
   *     ----|---+------------------------------------------------+---|----   Math.PI/2.0
   *         |   |                                                |   |
   *         +========================================================+
   *         |---|                                                | 
   *           ^ |    +---------------------|------x              |    +---
   *           | |    |                     |       i+1           |    |
   *  overPole | |    |--- splitTolerance --|                     |    |
   *   BorderPad |    |                           on other        |    |
   *             |    x                            branch         |    x
   *             |     i                                          |
   *     --------+------------------------------------------------+--------  -Math.PI/2.0
   *       
   *       lowerBranchValue                      lowerBranchValue + (2 * Math.PI)
   * </pre>
   * 
   * <p>
   * When the difference between two successive points in a {@link PathPlotter} exceeds the
   * splitTolerance then the map projection will consider them to be connected across the branch
   * cut. Typically this value should be something slightly larger than half the difference between
   * the lowerBranchValue and upperBranchValue for the projection to work properly.
   * </p>
   * <p>
   * Note: the current implementation does not validate or otherwise alter the supplied latitude (y
   * coordinate) values. These are unnecessary to manipulate in the projection. It does support
   * paths that pass through the pole or that contain it, so long as they are simply connected.
   * Paths that are not simply connected may work or may not. Further, each closed region should lie
   * in a single hemisphere.
   * </p>
   * 
   * @param lowerBranchValue the value representing the lower longitude of the branch cut in radians
   * @param splitTolerance the value of the splitTolerance expressed in radians
   * @param overPoleBorderPad the minimum amount of &quot;closure&quot; padding added to paths that
   *        enclose either pole specified in radians.
   * 
   * @return the newly constructed map projection
   * 
   * @throws IllegalArgumentException if lowerBranchLongitude lies outside the range [-2.0*Math.PI,
   *         0.0]
   */
  static class SimpleCylindrical implements Builder<Projection, RuntimeException> {

    private double lowerBranchLongitude = -Math.PI;
    private double splitTolerance = Math.PI - 0.001;
    private double overPoleBorderPad = Math.toRadians(20.0);

    private SimpleCylindrical() {}

    public SimpleCylindrical withLowerBranch(double longitudeInRadians) {
      checkArgument(longitudeInRadians >= -TWOPI);
      checkArgument(longitudeInRadians <= 0.0);
      this.lowerBranchLongitude = longitudeInRadians;
      return this;
    }

    public SimpleCylindrical withSplit(double toleranceInRadians) {
      checkArgument(toleranceInRadians > 0);
      checkArgument(toleranceInRadians < TWOPI);
      this.splitTolerance = toleranceInRadians;
      return this;
    }

    public SimpleCylindrical withOverPoleBorder(double paddingInRadians) {
      checkArgument(overPoleBorderPad > 0);
      this.overPoleBorderPad = paddingInRadians;
      return this;
    }

    @Override
    public Projection build() throws RuntimeException {
      return new picante.math.cones.SimpleCylindrical(lowerBranchLongitude, splitTolerance, overPoleBorderPad);
    }

  }

  public static SimpleCylindrical simpleCylindrical() {
    return new SimpleCylindrical();
  }

}
