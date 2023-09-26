package picante.math.cones;

abstract class DerivativeProjection extends AbstractProjection {

  private final Projection origin;

  DerivativeProjection(Projection originalProjection) {
    super();
    this.origin = originalProjection;
  }

  /**
   * {@inheritDoc}
   * 
   * Recursively unwrap the underlying projection wrappers. This is safe, because there is no way
   * outside of the {@link Projection#flip()},
   * {@link Projection#clip(picante.math.intervals.UnwritableInterval, picante.math.intervals.UnwritableInterval)},
   * or {@link Projection#rotate(picante.math.vectorspace.UnwritableRotationMatrixIJK) methods
   * that allow these to be created.
   */
  @Override
  public Class<? extends Projection> getOriginalClass() {
    return origin.getOriginalClass();
  }

}
