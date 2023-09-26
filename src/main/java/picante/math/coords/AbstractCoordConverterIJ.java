package picante.math.coords;

import static com.google.common.base.Preconditions.checkNotNull;
import picante.math.vectorspace.MatrixIJ;
import picante.math.vectorspace.UnwritableVectorIJ;

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
 * @param <C> a {@link AbstractVectorIJ} type
 */
abstract class AbstractCoordConverterIJ<C extends AbstractVectorIJ> implements CoordConverterIJ<C> {

  private final TransformationIJ<C> jacobian;

  /**
   * 
   * @param jacobian the incoming Jacobian must be thread safe for this class to be thread safe.
   *        Implementations contained in this package are assumed to be thread safe.
   */
  public AbstractCoordConverterIJ(TransformationIJ<C> jacobian) {
    this.jacobian = checkNotNull(jacobian);
  }

  // we know that C extends UC
  @Override
  public State<C> toCoordinate(State<UnwritableVectorIJ> cartesian) {

    MatrixIJ matrix = new MatrixIJ();

    C position = toCoordinate(cartesian.getPosition());

    jacobian.getInverseTransformation(position, matrix);

    C velocity = jacobian.mxv(matrix, cartesian.getVelocity());

    return construct(position, velocity);
  }

  abstract State<C> construct(C position, C velocity);

  @Override
  public State<UnwritableVectorIJ> toCartesian(State<C> coordinate) {

    MatrixIJ matrix = new MatrixIJ();

    UnwritableVectorIJ position = toCartesian(coordinate.getPosition());

    jacobian.getTransformation(coordinate.getPosition(), matrix);

    UnwritableVectorIJ velocity = jacobian.mxv(matrix, coordinate.getVelocity());

    return new CartesianStateIJ(position, velocity);
  }

}
