package picante.math.coords;

import static com.google.common.base.Preconditions.checkNotNull;
import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.UnwritableVectorIJK;

/**
 * This is a helper class intended to encapsulate the two state methods on the
 * {@link CoordConverter} interface. Since these methods are really just leveraging a
 * {@link Transformation}, they need not be reimplimented every time.
 * 
 * Particularly, this class is meant to ensure the Thread safety of implementations of
 * {@link CoordConverter} in a consistent way. It is important that the incoming
 * {@link Transformation} is thread safe(preferably stateless).
 * 
 * TODO The other two methods should also be accomplished leveraging some other class, this will
 * then ensure the thread safety of those two methods as well. Also, this returns the templated
 * classes {@link WritableState} and {@link State}, not the concrete classes, so these methods will
 * need to be casted if you want the real state.
 * 
 * @author G.K.Stephens
 * 
 * @param <C> a {@link AbstractVector} type
 */
abstract class AbstractCoordConverter<C extends AbstractVector> implements CoordConverter<C> {

  private final Transformation<C> jacobian;

  /**
   * 
   * @param jacobian the incoming Jacobian must be thread safe for this class to be thread safe.
   *        Implementations contained in this package are assumed to be thread safe.
   */
  public AbstractCoordConverter(Transformation<C> jacobian) {
    this.jacobian = checkNotNull(jacobian);
  }

  // we know that C extends UC
  @Override
  public State<C> toCoordinate(State<UnwritableVectorIJK> cartesian) {

    MatrixIJK matrix = new MatrixIJK();

    C position = toCoordinate(cartesian.getPosition());

    jacobian.getInverseTransformation(position, matrix);

    C velocity = jacobian.mxv(matrix, cartesian.getVelocity());

    return construct(position, velocity);
  }

  abstract State<C> construct(C position, C velocity);

  @Override
  public State<UnwritableVectorIJK> toCartesian(State<C> coordinate) {

    MatrixIJK matrix = new MatrixIJK();

    UnwritableVectorIJK position = toCartesian(coordinate.getPosition());

    jacobian.getTransformation(coordinate.getPosition(), matrix);

    UnwritableVectorIJK velocity = jacobian.mxv(matrix, coordinate.getVelocity());

    return new CartesianState(position, velocity);
  }

}
