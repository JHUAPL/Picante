package picante.surfaces;

import static com.google.common.base.Preconditions.checkArgument;
import com.google.common.base.MoreObjects;
import picante.math.vectorspace.MatrixIJ;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJ;
import picante.math.vectorspace.VectorIJK;
import picante.surfaces.Ellipse.Type;
import picante.surfaces.Ellipsoid.PointAndDistance;
import picante.units.FundamentalPhysicalConstants;

/**
 * Unmodifiable parent class of the mutable ellipse class.
 * <p>
 * This class handles degeneracy, in that it directly supports ellipses that are collapsed to a line
 * segment (1D) or point (0D).
 * </p>
 */
public class UnwritableEllipse {

  /**
   * Field referencing the sub-type of ellipse.
   */
  Ellipse.Type type;

  private final VectorIJK theCenter;
  private final VectorIJK theSmajor;
  private final VectorIJK theSminor;

  /*
   * Unmodifiable views exposed to the sub-class.
   */
  final UnwritableVectorIJK center;
  final UnwritableVectorIJK smajor;
  final UnwritableVectorIJK sminor;

  /*
   * final double
   */
  final static double ORTHOGONALITY_TOL = 0.001;

  /**
   * Creates the default ellipse.
   * <p>
   * This constructor is only to be utilized internally, and is exposed at the package level only to
   * allow the writable sub-class to access it for performance reasons. (The alternate constructor
   * is expensive to invoke.)
   * </p>
   */
  UnwritableEllipse() {
    this.theCenter = new VectorIJK();
    this.theSmajor = new VectorIJK(VectorIJK.I);
    this.theSminor = new VectorIJK(VectorIJK.J);
    this.center = this.theCenter;
    this.smajor = this.theSmajor;
    this.sminor = this.theSminor;
    this.type = Type.ELLIPSE;
  }

  public static UnwritableEllipse copyOf(UnwritableEllipse ellipse) {
    if (ellipse.getClass().equals(UnwritableEllipse.class)) {
      return ellipse;
    }
    UnwritableEllipse result = new UnwritableEllipse();
    result.setTo(ellipse);
    return result;
  }

  /**
   * 
   * Creates an unwritable ellipse from the center and two generating vectors.
   * 
   * @param center the vector to the center of the ellipse
   * @param u a generating vector, may be zero.
   * @param v another generating vector, v may be parallel to u.
   * @return
   */
  public static UnwritableEllipse createWithGeneratingVectors(UnwritableVectorIJK center,
      UnwritableVectorIJK u, UnwritableVectorIJK v) {
    UnwritableEllipse ellipse = new UnwritableEllipse();
    ellipse.setToGenerating(center, u, v);
    return ellipse;
  }

  /**
   * Creates an unwritable ellipse from the center, semiMajor, and semiMinor. Checks if the
   * semiMajor and semiMinor are orthogonal.
   * 
   * @param center the center vector
   * @param semiMajor major axis
   * @param semiMinor must be an orthogonal vector to semiMajor
   * @return
   */
  public static UnwritableEllipse create(UnwritableVectorIJK center, UnwritableVectorIJK semiMajor,
      UnwritableVectorIJK semiMinor) {
    UnwritableEllipse ellipse = new UnwritableEllipse();
    ellipse.setTo(center, semiMajor, semiMinor);
    ellipse.type = ellipse.determineType();
    return ellipse;
  }

  public static UnwritableEllipse create(UnwritableEllipse ellipse) {
    UnwritableEllipse newEllipse = new UnwritableEllipse();
    newEllipse.setTo(ellipse);
    return newEllipse;
  }



  /**
   * Retrieves the center vector.
   * 
   * @param buffer the buffer to receive the center.
   * 
   * @return a reference to buffer for convenience.
   */
  public VectorIJK getCenter(VectorIJK buffer) {
    return buffer.setTo(theCenter);
  }

  /**
   * Creates a copy of the center vector.
   * 
   * @return a newly created vector containing the center of the ellipse
   */
  public VectorIJK getCenter() {
    return getCenter(new VectorIJK());
  }

  /**
   * Retrieves the semi-major axis vector.
   * 
   * @param buffer the buffer to receive the semi-major axis.
   * 
   * @return a reference to buffer for convenience
   */
  public VectorIJK getSemiMajorAxis(VectorIJK buffer) {
    return buffer.setTo(theSmajor);
  }

  /**
   * Creates a copy of the semi-major axis.
   * 
   * @return a newly created vector containing the semi-major axis of the ellipse
   */
  public VectorIJK getSemiMajorAxis() {
    return getSemiMajorAxis(new VectorIJK());
  }

  /**
   * Retrieves the semi-minor axis vector.
   * 
   * @param buffer the buffer to receive the semi-minor axis.
   * 
   * @return a reference to buffer for convenience
   */
  public VectorIJK getSemiMinorAxis(VectorIJK buffer) {
    return buffer.setTo(theSminor);
  }

  /**
   * Creates a copy of the semi-minor axis.
   * 
   * @return a newly created vector containing the semi-minor axis of the ellipse
   */
  public VectorIJK getSemiMinorAxis() {
    return getSemiMinorAxis(new VectorIJK());
  }

  /**
   * Get the type of the ellipse.
   * 
   * @return the associated type enumeration
   */
  public Type getType() {
    return this.type;
  }

  /**
   * Is the ellipse degenerate, not fully 2D?
   * 
   * @return true if degenerate, false otherwise
   */
  public boolean isDegenerate() {
    return this.type.isDegenerate();
  }

  /**
   * Is the ellipse precisely a circle?
   * 
   * @return true if the semi-major and semi-minor axis
   */
  public boolean isCircular() {
    if (this.type.equals(Type.POINT)) {
      return true;
    }
    return smajor.getLength() == sminor.getLength();
  }

  /**
   * Is the ellipse contained completely within the plane?
   * <p>
   * This method exists to allow for checking if infinite intersections between the ellipse and the
   * plane exist. Invoking
   * {@link UnwritableEllipse#intersect(UnwritablePlane, VectorIJK, VectorIJK)} in this case will
   * result in the generation of a runtime exception.
   * </p>
   * 
   * @param plane the plane to test for containment
   * 
   * @return true if ellipse is contained completely within plane, false otherwise
   */
  public boolean isContainedWithin(UnwritablePlane plane) {
    return this.type.isContainedWithin(this, plane);
  }

  /**
   * Does the ellipse intersect the plane?
   * <p>
   * This method exists to allow for checking ahead of time if the ellipse intersects the plane.
   * Invoking {@link UnwritableEllipse#intersect(UnwritablePlane, VectorIJK, VectorIJK)} will result
   * in a runtime exception if no exceptions exist.
   * </p>
   * 
   * @param plane the plane to test for intersection
   * 
   * @return true if ellipse intersects plane, false otherwise
   */
  public boolean intersects(UnwritablePlane plane) {
    return this.type.intersects(this, plane);
  }

  /**
   * Compute the intersection of the ellipse with a plane.
   * 
   * @param plane the plane with which to compute the intersection
   * @param bufferOne a buffer to capture the first point of intersection
   * @param bufferTwo another buffer to capture the second point of intersection
   * 
   * @throws IllegalArgumentException if not intersection occurs, or if the two vector buffers are
   *         not sufficient to capture all possible intersection points. In general the latter case
   *         is triggered if the ellipse is anything but a point, and entirely contained within the
   *         plane.
   */
  public void intersect(UnwritablePlane plane, VectorIJK bufferOne, VectorIJK bufferTwo) {
    this.type.intersect(this, plane, bufferOne, bufferTwo);
  }

  public PointAndDistance computeNearPoint(VectorIJK position) {
    return this.type.computeNearPoint(this, position);
  }

  public Plane getPlane() {
    return getPlane(new Plane());
  }

  public Plane getPlane(Plane buffer) {
    checkArgument(this.type.equals(Ellipse.Type.ELLIPSE),
        "Only non-degenerate ellipses can be turned into a plane.");
    buffer.setTo(this.center, this.smajor, this.sminor);
    return buffer;
  }

  UnwritableEllipse setTo(UnwritableVectorIJK center, UnwritableVectorIJK semiMajor,
      UnwritableVectorIJK semiMinor) {
    double orthogonality =
        Math.abs(semiMajor.getSeparation(semiMinor) - FundamentalPhysicalConstants.HALFPI);
    checkArgument(orthogonality < ORTHOGONALITY_TOL,
        "SemiMajor and SemiMinor axes must be under the orthogonal tolerance: " + orthogonality);
    this.theCenter.setTo(center);
    this.theSmajor.setTo(semiMajor);
    this.theSminor.setTo(semiMinor);
    this.type = determineType();
    return this;
  }

  /**
   * Package private method used to configure the internals of the ellipse to the center and
   * supplied generating vectors.
   * 
   * @param center the center of the ellipse
   * @param u a generating vector (possibly VectorIJK.ZERO)
   * @param v another generating vector (possibly VectorIJK.ZERO, or parallel to u)
   * 
   * @return a reference to the instance
   */
  UnwritableEllipse setToGenerating(UnwritableVectorIJK center, UnwritableVectorIJK u,
      UnwritableVectorIJK v) {
    this.theCenter.setTo(center);

    MatrixIJ matrix = new MatrixIJ();
    VectorIJ values = new VectorIJ();
    VectorIJK tmp = new VectorIJK();

    double scale = Math.max(u.getLength(), v.getLength());

    /*
     * This is an exceptional case representing a single point in space.
     */
    if (scale == 0.0) {
      theSmajor.setTo(VectorIJK.ZERO);
      theSminor.setTo(VectorIJK.ZERO);
      this.type = Type.POINT;
      return this;
    }

    theSmajor.setTo(u).scale(1.0 / scale);
    theSminor.setTo(v).scale(1.0 / scale);

    double offDiagonals = theSmajor.getDot(theSminor);
    matrix.setTo(theSmajor.getDot(theSmajor), offDiagonals, offDiagonals,
        theSminor.getDot(theSminor));

    /*
     * Diagonalize matrix.
     */
    MatrixIJ.diagonalizeSymmetricMatrix(matrix, values, matrix);

    /*
     * Compute the semi-axes.
     */
    int major, minor;
    if (Math.abs(values.getI()) >= Math.abs(values.getJ())) {

      /*
       * The first eigenvector corresponds to the semi-major axes.
       */
      major = 0;
      minor = 1;

    } else {

      /*
       * The second eigenvector corresponds to the semi-major axis.
       */
      major = 1;
      minor = 0;
    }

    VectorIJK.combine(matrix.get(0, major), theSmajor, matrix.get(1, major), theSminor, tmp);
    VectorIJK.combine(matrix.get(0, minor), theSmajor, matrix.get(1, minor), theSminor, theSminor);
    theSmajor.setTo(tmp);

    /*
     * Restore the scaling.
     */
    theSmajor.scale(scale);
    theSminor.scale(scale);

    this.type = determineType();

    return this;
  }

  /**
   * Simple method that encapsulates the type selection logic by interrogating the existing fields
   * on the class.
   * 
   * @return the appropriate instance of type.
   */
  private Type determineType() {

    if (sminor.equals(VectorIJK.ZERO)) {

      /*
       * We have a degenerate case, determine which flavor:
       */
      if (smajor.equals(VectorIJK.ZERO)) {
        return Type.POINT;
      }
      return Type.LINE_SEGMENT;

    }

    return Type.ELLIPSE;
  }

  /**
   * Package private method to safely configure the instance to that of another ellipse.
   * 
   * @param ellipse the ellipse to copy
   * 
   * @return a reference to the instance
   */
  UnwritableEllipse setTo(UnwritableEllipse ellipse) {
    this.theCenter.setTo(ellipse.theCenter);
    this.theSmajor.setTo(ellipse.theSmajor);
    this.theSminor.setTo(ellipse.theSminor);
    this.type = ellipse.type;
    return this;
  }

  UnwritableEllipse rotate(UnwritableRotationMatrixIJK rotation) {
    rotation.mxv(theCenter, theCenter);
    rotation.mxv(theSmajor, theSmajor);
    rotation.mxv(theSminor, theSminor);
    return this;
  }

  UnwritableEllipse offset(UnwritableVectorIJK offset) {
    VectorIJK.add(theCenter, offset, theCenter);
    return this;
  }

  void scale(double scale) {
    checkArgument(scale >= 0.0, "Only positive scalings may be applied to an ellipse.");
    this.theCenter.scale(scale);
    this.theSmajor.scale(scale);
    this.theSminor.scale(scale);
    this.type = determineType();
  }

  @Override
  public String toString() {
    return MoreObjects.toStringHelper(getClass()).add("center", this.center)
        .add("smajor", this.smajor).add("sminor", this.sminor).toString();
  }

  @Override
  public final int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((center == null) ? 0 : center.hashCode());
    result = prime * result + ((smajor == null) ? 0 : smajor.hashCode());
    result = prime * result + ((sminor == null) ? 0 : sminor.hashCode());
    result = prime * result + ((type == null) ? 0 : type.hashCode());
    return result;
  }

  @Override
  public final boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (!(obj instanceof UnwritableEllipse)) {
      return false;
    }
    UnwritableEllipse other = (UnwritableEllipse) obj;
    if (center == null) {
      if (other.center != null) {
        return false;
      }
    } else if (!center.equals(other.center)) {
      return false;
    }
    if (smajor == null) {
      if (other.smajor != null) {
        return false;
      }
    } else if (!smajor.equals(other.smajor)) {
      return false;
    }
    if (sminor == null) {
      if (other.sminor != null) {
        return false;
      }
    } else if (!sminor.equals(other.sminor)) {
      return false;
    }
    if (type != other.type) {
      return false;
    }
    return true;
  }

}
