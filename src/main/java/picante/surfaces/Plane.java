package picante.surfaces;

import picante.designpatterns.Writable;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

/**
 * Writable subclass of the unwritable 3D plane that completes the implementation of the weak
 * immutability design pattern.
 * <p>
 * This class contains the mutator methods necessary to set or alter the internals of the parent
 * classes fields.
 * </p>
 */
public class Plane extends UnwritablePlane implements Writable<UnwritablePlane, Plane> {

  /**
   * Creates the default plane, whose normal is {@link VectorIJK#K} and contains the origin.
   */
  public Plane() {
    super(VectorIJK.K, 0.0);
  }

  /**
   * Copy constructor.
   * 
   * @param plane creates a new plane, copying the contents of the supplied plane.
   */
  public Plane(UnwritablePlane plane) {
    super(plane);
  }

  /**
   * Constructs a plane from the supplied normal vector and constant.
   * <p>
   * A plane can be defined by the expression:
   * 
   * <pre>
   *    &lt; x, normal &gt; = constant
   * </pre>
   * 
   * where &lt;,&gt; denotes the inner product, and x an arbitrary point in the plane.
   * </p>
   * 
   * @param normal the normal vector
   * @param constant the constant
   * 
   * @throws IllegalArgumentException if normal is zero length.
   */
  public Plane(UnwritableVectorIJK normal, double constant) {
    super(normal, constant);
  }

  /**
   * Constructs a plane from the supplied point and two spanning vectors.
   * <p>
   * A plane can be defined by the expression:
   * 
   * <pre>
   * point + s * u + t * v
   * </pre>
   * 
   * where s and t are real numbers, u and v are the linearly independent spanning vectors, and
   * point is an arbitrary point in the plane of interest.
   * </p>
   * 
   * @param point an arbitrary point in the plane
   * @param u one of the two linearly independent spanning vectors
   * @param v the other of the spanning vectors
   * 
   * @throws IllegalArgumentException if u and v are parallel
   */
  public Plane(UnwritableVectorIJK point, UnwritableVectorIJK u, UnwritableVectorIJK v) {
    super(point, u, v);
  }

  /**
   * Constructs a plane from the supplied normal vector and point in the plane.
   * <p>
   * A plane can be defined by the expression:
   * 
   * <pre>
   *    &lt; x - point, normal &gt; = 0
   * </pre>
   * 
   * where &lt;,&gt; denotes the inner product, and x an arbitrary point in the plane.
   * </p>
   * 
   * @param normal the normal vector
   * @param point a point in the plane
   * 
   * @throws IllegalArgumentException if normal is zero length
   */
  public Plane(UnwritableVectorIJK normal, UnwritableVectorIJK point) {
    super(normal, point);
  }

  /**
   * Configures the instance to the value of the supplied plane.
   * 
   * @param plane the plane to mutate the instance to
   * 
   * @return a reference to the instance for convenience
   */
  @Override
  public Plane setTo(UnwritablePlane plane) {
    super.setTo(plane);
    return this;
  }

  /**
   * Sets the contents of the plane to that of a normal vector and constant.
   * <p>
   * A plane can be defined by the expression:
   * 
   * <pre>
   *    &lt; x, normal &gt; = constant
   * </pre>
   * 
   * where &lt;,&gt; denotes the inner product, and x an arbitrary point in the plane.
   * </p>
   * 
   * @param normal the normal vector
   * @param constant the constant
   * 
   * @return a reference to the instance for convenience
   * 
   * @throws IllegalArgumentException if normal is zero length
   */
  @Override
  public Plane setTo(UnwritableVectorIJK normal, double constant) {
    super.setTo(normal, constant);
    return this;
  }

  /**
   * Sets the contents of the plane to that of the supplied normal vector and point.
   * <p>
   * A plane can be defined by the expression:
   * 
   * <pre>
   *    &lt; x - point, normal &gt; = 0
   * </pre>
   * 
   * where &lt;,&gt; denotes the inner product, and x an arbitrary point in the plane.
   * </p>
   * 
   * @param normal the normal vector
   * @param point a point in the plane
   * 
   * @returns a reference to the instance for convenience
   * 
   * @throws IllegalArgumentException if normal is zero length
   */
  @Override
  public Plane setTo(UnwritableVectorIJK normal, UnwritableVectorIJK point) {
    super.setTo(normal, point);
    return this;
  }

  /**
   * Sets the contents of the plane from the supplied point and two spanning vectors.
   * <p>
   * A plane can be defined by the expression:
   * 
   * <pre>
   * point + s * u + t * v
   * </pre>
   * 
   * where s and t are real numbers, u and v are the linearly independent spanning vectors, and
   * point is an arbitrary point in the plane of interest.
   * </p>
   * 
   * @param point an arbitrary point in the plane
   * @param u one of the two linearly independent spanning vectors
   * @param v the other of the spanning vectors
   * 
   * @returns a reference to the instance for convenience
   * 
   * @throws IllegalArgumentException if u and v are parallel
   */
  @Override
  public Plane setTo(UnwritableVectorIJK point, UnwritableVectorIJK u, UnwritableVectorIJK v) {
    super.setTo(point, u, v);
    return this;
  }

}
