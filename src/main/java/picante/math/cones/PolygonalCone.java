package picante.math.cones;

import java.util.ArrayList;
import java.util.List;
import picante.math.intervals.UnwritableInterval;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

/**
 * A Polygonal Cone. Parameterize from 0-n in order to generate the n sides, in that order.
 * getEdge(0) will return corner 0, getEdge(1) will return corner 1...
 *
 * TODO This needs testing. I have not had the chance to perform integration tests.
 * 
 * @author poffert1
 *
 */

public class PolygonalCone implements Cone {
  private final UnwritableVectorIJK vertex;
  private final UnwritableVectorIJK interiorPoint;
  private final int n;
  private final List<UnwritableVectorIJK> corners;


  PolygonalCone(UnwritableVectorIJK vertex, UnwritableVectorIJK interiorPt,
      List<UnwritableVectorIJK> pts3d) {
    this.vertex = vertex;
    this.interiorPoint = interiorPt;
    this.n = pts3d.size();

    List<UnwritableVectorIJK> corners = new ArrayList<>(pts3d);
    corners.add(corners.get(0));
    corners.add(corners.get(1)); // I am adding two vectors to the end. This is to handle the case
                                 // where the end of the domain is entered (4)
    this.corners = corners;
  }

  @Override
  public UnwritableVectorIJK getVertex() {
    return this.vertex;
  }

  @Override
  public UnwritableVectorIJK getEdge(double parameter) {
    int idx0 = (int) parameter;
    int idx1 = idx0 + 1;
    double amt = parameter - idx0;

    UnwritableVectorIJK vector0 = corners.get(idx0);
    UnwritableVectorIJK vector1 = corners.get(idx1);

    return VectorIJK.combine(1 - amt, vector0, amt, vector1);
  }

  @Override
  public UnwritableInterval getParameterDomain() {
    return new UnwritableInterval(0, n);
  }

  @Override
  public UnwritableVectorIJK getInteriorPoint() {
    return this.interiorPoint;
  }

  /**
   * Only return the original list of n corners
   * 
   * @return
   */
  public List<UnwritableVectorIJK> getCorners() {
    return corners.subList(0, n);
  }

}
