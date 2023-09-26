package picante.surfaces;

import static com.google.common.base.Preconditions.checkArgument;
import static java.lang.Math.abs;
import static java.lang.Math.max;
import static java.lang.Math.min;
import static java.lang.Math.signum;
import static java.lang.Math.sqrt;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import com.google.common.base.Predicate;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import picante.math.vectorspace.UnwritableVectorIJ;
import picante.math.vectorspace.VectorIJ;

class Circle {

  private final double rr;

  Circle(double radius) {
    super();
    this.rr = radius * radius;
  }

  boolean isSimple() {
    return true;
  }

  boolean isClosed() {
    return true;
  }

  boolean isJordan() {
    return isSimple() && isClosed();
  }

  /**
   * Computes the first intersection of the r
   * 
   * @param source
   * @param ray
   * @param buffer
   * @return
   */
  public List<VectorIJ> computeIntersectionsLineSegment(UnwritableVectorIJ point1,
      UnwritableVectorIJ point2) {

    List<VectorIJ> points = computeIntersectionsLine(point1, point2);

    final double minX = min(point1.getI(), point2.getI());
    final double maxX = max(point1.getI(), point2.getI());

    final double minY = min(point1.getJ(), point2.getJ());
    final double maxY = max(point1.getJ(), point2.getJ());

    Predicate<VectorIJ> onSegment = new Predicate<VectorIJ>() {

      @Override
      public boolean apply(VectorIJ point) {

        double x = point.getI();
        double y = point.getJ();

        return x >= minX && x <= maxX && y >= minY && y <= maxY;

      }
    };

    return Lists.newArrayList(Iterables.filter(points, onSegment));
  }

  /**
   * Computes the first intersection of the r
   * 
   * @param source
   * @param ray
   * @param buffer
   * @return
   */
  public List<VectorIJ> computeIntersectionsRay(final UnwritableVectorIJ source,
      final UnwritableVectorIJ rayFromSource) {

    List<VectorIJ> points = computeIntersectionsLine(source, VectorIJ.add(source, rayFromSource));

    Predicate<VectorIJ> alongRay = new Predicate<VectorIJ>() {

      @Override
      public boolean apply(VectorIJ point) {

        VectorIJ pointRay = VectorIJ.subtract(point, source);

        return pointRay.getDot(rayFromSource) > 0;
      }
    };

    return Lists.newArrayList(Iterables.filter(points, alongRay));
  }

  /**
   * Computes the first intersection of the r
   * 
   * @param source
   * @param ray
   * @param buffer
   * @return
   */
  public VectorIJ computeFirstIntersectionRay(final UnwritableVectorIJ source,
      UnwritableVectorIJ rayFromSource, VectorIJ buffer) {

    List<VectorIJ> points = computeIntersectionsRay(source, rayFromSource);

    if (points.size() == 0) {
      throw new NoIntersectionException();
    }

    Collections.sort(points, new Comparator<VectorIJ>() {

      @Override
      public int compare(VectorIJ o1, VectorIJ o2) {

        double dx1 = o1.getI() - source.getI();
        double dy1 = o1.getJ() - source.getJ();

        double d1 = sqrt(dx1 * dx1 + dy1 * dy1);

        double dx2 = o2.getI() - source.getI();
        double dy2 = o2.getJ() - source.getJ();

        double d2 = sqrt(dx2 * dx2 + dy2 * dy2);

        return Double.compare(d1, d2);
      }
    });

    return buffer.setTo(points.get(0));
  }

  /**
   * Computes all the intersections along an (infinite) line defined by two points and the curve.
   * 
   * @param point1 the first point defining the line
   * @param point2 the second point defining the line
   * @return a {@link List} of all the intersections.
   */
  public List<VectorIJ> computeIntersectionsLine(UnwritableVectorIJ point1,
      UnwritableVectorIJ point2) {

    checkArgument(!point1.equals(point2),
        "Both points are the same, cannot form a line from one point");

    List<VectorIJ> vects = Lists.newArrayList();

    double x1 = point1.getI();
    double y1 = point1.getJ();

    double x2 = point2.getI();
    double y2 = point2.getJ();

    double dx = x2 - x1;
    double dy = y2 - y1;

    double dr = sqrt(dx * dx + dy * dy);

    double drdr = dr * dr;

    double det = x1 * y2 - x2 * y1;

    double delta = rr * drdr - det * det;

    if (delta < 0) {
    }
    // TODO should there be some tolerance on this, it is unlikely to ever
    // be exactly zero
    else if (delta == 0) {

      double xi1 = (det * dy + signumStar(dy) * dx * sqrt(rr * drdr - det * det)) / (drdr);

      double yi1 = (-det * dx + abs(dy) * sqrt(rr * drdr - det * det)) / (drdr);

      vects.add(new VectorIJ(xi1, yi1));
    } else {

      double rad = sqrt(rr * drdr - det * det);

      double xi1 = (det * dy + signumStar(dy) * dx * rad) / (drdr);
      double yi1 = (-det * dx + abs(dy) * sqrt(rr * drdr - det * det)) / (drdr);

      double xi2 = (det * dy - signumStar(dy) * dx * rad) / (drdr);
      double yi2 = (-det * dx - abs(dy) * rad) / (drdr);

      vects.add(new VectorIJ(xi1, yi1));
      vects.add(new VectorIJ(xi2, yi2));
    }
    return vects;
  }

  /**
   * 
   * @param surfacePoint
   * @param buffer
   * @return
   */
  public VectorIJ computeGradient(UnwritableVectorIJ surfacePoint, VectorIJ buffer) {
    return buffer.setTo(2 * surfacePoint.getI(), 2 * surfacePoint.getJ());
  }

  public static double signumStar(double value) {
    if (value == 0) {
      return 1.0;
    }
    return signum(value);
  }

}
