package picante.math.cones;

import picante.math.intervals.UnwritableInterval;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

/**
 * An implementation of a {@link Cone} that has an elliptical shape.
 * 
 * The Cone has a vertex, an interior point, and two "generating vectors". These vectors are
 * orthogonal to each other and to the interior point, and will be used in the calculation of edge
 * vectors. For any parameter theta in [0, 2*PI], the edge vector is:
 * 
 * boresight + cos(theta) * axis1 + sin(theta) * axis2
 * 
 * @author osheacm1
 *
 */
class EllipticalCone implements Cone {

  private final UnwritableVectorIJK vertex;
  private final UnwritableVectorIJK boresight;
  private UnwritableVectorIJK axis1;
  private UnwritableVectorIJK axis2;

  /**
   * Construct an EllipticalCone from the vertex, boresight, and two orthogonal axes
   * 
   * @param vertex the vector to the vertex of the cone
   * @param boresight the boresight of the instrument, or the axis of the cone - pointed down the
   *        middle of the cone
   * @param axis1 one "generating vector" - scaled by the tangent of a cone half angle
   * @param axis2 the second "generating vector" - scaled by the tangent of the cone's other half
   *        angle
   */
  EllipticalCone(UnwritableVectorIJK vertex, UnwritableVectorIJK boresight,
      UnwritableVectorIJK axis1, UnwritableVectorIJK axis2) {
    super();
    this.vertex = vertex;
    this.boresight = boresight;
    this.axis1 = axis1;
    this.axis2 = axis2;
  }

  @Override
  public UnwritableVectorIJK getVertex() {
    return vertex;
  }

  @Override
  public UnwritableVectorIJK getEdge(double theta) {
    VectorIJK edge =
        VectorIJK.combine(1.0, boresight, Math.cos(theta), axis1, Math.sin(theta), axis2);

    return edge.createUnitized();
  }

  @Override
  public UnwritableInterval getParameterDomain() {
    return new UnwritableInterval(0, 2 * Math.PI);
  }

  @Override
  public UnwritableVectorIJK getInteriorPoint() {
    return boresight;
  }

}
