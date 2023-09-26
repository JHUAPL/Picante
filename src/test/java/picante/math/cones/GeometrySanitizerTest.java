package picante.math.cones;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.junit.Before;
import org.junit.Test;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.LineString;
import org.locationtech.jts.geom.LinearRing;
import org.locationtech.jts.geom.MultiLineString;
import org.locationtech.jts.geom.MultiPoint;
import org.locationtech.jts.geom.MultiPolygon;
import org.locationtech.jts.geom.Point;
import org.locationtech.jts.geom.Polygon;
import org.locationtech.jts.geom.PrecisionModel;
import org.locationtech.jts.geom.impl.PackedCoordinateSequenceFactory;
import com.google.common.collect.ImmutableList;

public class GeometrySanitizerTest {

  private GeometryFactory geometryFactory;
  private GeometrySanitizer sanitizer;

  @Before
  public void setUp() throws Exception {
    this.geometryFactory = new GeometryFactory(new PrecisionModel(PrecisionModel.FLOATING), 0,
        PackedCoordinateSequenceFactory.DOUBLE_FACTORY);
    this.sanitizer = new GeometrySanitizer(geometryFactory);
  }

  @Test
  public void testEmptyList() {
    Geometry geometry = sanitizer.apply(ImmutableList.of());
    assertTrue(geometry.isEmpty());
  }

  Point createPoint(double x, double y) {
    return geometryFactory.createPoint(new Coordinate(x, y));
  }

  @Test
  public void testSinglePointList() {
    Geometry geometry = sanitizer.apply(ImmutableList.of(createPoint(0, 0)));
    assertEquals(Point.class, geometry.getClass());
    assertEquals(1, geometry.getCoordinates().length);
    assertEquals(new Coordinate(0, 0), geometry.getCoordinates()[0]);
  }

  @Test
  public void testMultiPointList() {
    ImmutableList<Point> points =
        ImmutableList.of(createPoint(0, 0), createPoint(1, 1), createPoint(2, 2));
    Geometry geometry = sanitizer.apply(points);
    assertEquals(MultiPoint.class, geometry.getClass());
    assertEquals(3, geometry.getNumGeometries());
    for (int i = 0; i < 3; i++) {
      checkGeometry(points.get(i).getCoordinates(), Point.class, geometry.getGeometryN(i));
    }
  }

  LineString createLineString(double... xys) {
    return geometryFactory.createLineString(JtsUtilities.createCoordinateArray(xys));
  }

  @Test
  public void testLineString() {
    LineString lineString = createLineString(0, 0, 1, 1, 2, 2, 3, 3);
    Geometry geometry = sanitizer.apply(ImmutableList.of(lineString));
    assertEquals(LineString.class, geometry.getClass());
    assertEquals(1, geometry.getNumGeometries());
    checkGeometry(lineString.getCoordinates(), LineString.class, geometry);
  }

  @Test
  public void testMultiLineStringList() {
    ImmutableList<LineString> strings = ImmutableList.of(createLineString(0, 0, 1, 1, 2, 2),
        createLineString(0, 1, 1, 2, 2, 3), createLineString(0, 2, 1, 3, 2, 4));
    Geometry geometry = sanitizer.apply(strings);
    assertEquals(MultiLineString.class, geometry.getClass());
    assertEquals(3, geometry.getNumGeometries());
    for (int i = 0; i < 3; i++) {
      checkGeometry(strings.get(i).getCoordinates(), LineString.class, geometry.getGeometryN(i));
    }
  }

  LinearRing createLinearRing(double... xys) {
    double[] coords = new double[xys.length + 2];
    System.arraycopy(xys, 0, coords, 0, xys.length);
    coords[xys.length] = xys[0];
    coords[xys.length + 1] = xys[1];
    return geometryFactory.createLinearRing(JtsUtilities.createCoordinateArray(coords));
  }

  @Test
  public void testLinearRing() {
    LinearRing ring = createLinearRing(0, 0, 1, 0, 1, 1, 0, 1);
    Geometry geometry = sanitizer.apply(ImmutableList.of(ring));
    assertEquals(Polygon.class, geometry.getClass());
    assertEquals(1, geometry.getNumGeometries());
    checkGeometry(ring.getCoordinates(), Polygon.class, geometry);
  }

  @Test
  public void testLinearRingListDisjoint() {
    ImmutableList<LinearRing> rings = ImmutableList.of(createLinearRing(0, 0, 1, 0, 1, 1, 0, 1),
        createLinearRing(2, 0, 3, 0, 3, 1, 2, 1), createLinearRing(4, 0, 5, 0, 5, 1, 4, 1));
    Geometry geometry = sanitizer.apply(rings);
    assertEquals(MultiPolygon.class, geometry.getClass());
    assertEquals(3, geometry.getNumGeometries());
    for (int i = 0; i < geometry.getNumGeometries(); i++) {
      checkPolygon(rings.get(i), ImmutableList.of(), geometry.getGeometryN(i));
    }
  }

  @Test
  public void testLinearRingListOnePolygonWithSingleHole() {
    LinearRing shell = createLinearRing(0, 0, 3, 0, 3, 3, 0, 3.5);
    LinearRing hole = createLinearRing(1, 1, 2, 1, 2, 2, 1, 2);

    Geometry geometry = sanitizer.apply(ImmutableList.of(shell, hole));
    assertEquals(Polygon.class, geometry.getClass());
    checkPolygon(shell, ImmutableList.of(hole), geometry);

    /*
     * And the reverse order
     */
    geometry = sanitizer.apply(ImmutableList.of(hole, shell));
    assertEquals(Polygon.class, geometry.getClass());
    checkPolygon(shell, ImmutableList.of(hole), geometry);
  }

  @Test
  public void testLinearRingListOnePolygonWithMultipleHoles() {
    LinearRing shell = createLinearRing(0, 0, 10, 0, 10, 10, 0, 10.5);
    LinearRing hole1 = createLinearRing(1, 1, 2, 1, 2, 2, 1, 2);
    LinearRing hole2 = createLinearRing(3, 3, 4, 3, 4, 4, 3, 4);

    Geometry geometry = sanitizer.apply(ImmutableList.of(shell, hole1, hole2));
    assertEquals(Polygon.class, geometry.getClass());
    checkPolygon(shell, ImmutableList.of(hole2, hole1), geometry);

    sanitizer.apply(ImmutableList.of(shell, hole2, hole1));
    assertEquals(Polygon.class, geometry.getClass());
    checkPolygon(shell, ImmutableList.of(hole2, hole1), geometry);

    sanitizer.apply(ImmutableList.of(hole1, shell, hole2));
    assertEquals(Polygon.class, geometry.getClass());
    checkPolygon(shell, ImmutableList.of(hole2, hole1), geometry);

    sanitizer.apply(ImmutableList.of(hole2, shell, hole1));
    assertEquals(Polygon.class, geometry.getClass());
    checkPolygon(shell, ImmutableList.of(hole2, hole1), geometry);

    sanitizer.apply(ImmutableList.of(hole1, hole2, shell));
    assertEquals(Polygon.class, geometry.getClass());
    checkPolygon(shell, ImmutableList.of(hole2, hole1), geometry);

    sanitizer.apply(ImmutableList.of(hole2, hole1, shell));
    assertEquals(Polygon.class, geometry.getClass());
    checkPolygon(shell, ImmutableList.of(hole2, hole1), geometry);
  }

  @Test
  public void testLinearRingListMultiPolygonNoHoles() {
    LinearRing shell1 = createLinearRing(0, 0, 1, 0, 1, 1, 0, 1.5);
    LinearRing shell2 = createLinearRing(3, 3, 4, 3, 4, 4, 3, 4.5);
    LinearRing shell3 = createLinearRing(7, 7, 8, 7, 8, 8, 7, 8.5);
    List<LinearRing> shells = ImmutableList.of(shell1, shell2, shell3);

    Polygon[] polygons = new Polygon[3];
    polygons[0] = geometryFactory.createPolygon(shell1);
    polygons[1] = geometryFactory.createPolygon(shell2);
    polygons[2] = geometryFactory.createPolygon(shell3);
    MultiPolygon expected = geometryFactory.createMultiPolygon(polygons);

    Geometry geometry = sanitizer.apply(shells);
    assertEquals(MultiPolygon.class, geometry.getClass());
    assertEquals(expected, geometry);

    geometry = sanitizer.apply(ImmutableList.of(shell1, shell3, shell2));
    assertEquals(MultiPolygon.class, geometry.getClass());
    assertTrue(expected.equalsNorm(geometry));

    geometry = sanitizer.apply(ImmutableList.of(shell3, shell1, shell2));
    assertEquals(MultiPolygon.class, geometry.getClass());
    assertTrue(expected.equalsNorm(geometry));

    geometry = sanitizer.apply(ImmutableList.of(shell2, shell1, shell3));
    assertEquals(MultiPolygon.class, geometry.getClass());
    assertTrue(expected.equalsNorm(geometry));

    geometry = sanitizer.apply(ImmutableList.of(shell2, shell3, shell1));
    assertEquals(MultiPolygon.class, geometry.getClass());
    assertTrue(expected.equalsNorm(geometry));

    geometry = sanitizer.apply(ImmutableList.of(shell3, shell2, shell1));
    assertEquals(MultiPolygon.class, geometry.getClass());
    assertTrue(expected.equalsNorm(geometry));
  }

  @Test
  public void testLinearRingMultipleOverlaps() {
    LinearRing outer = createLinearRing(0, 0, 5, 0, 5, 5, 0, 5.5);
    LinearRing middle = createLinearRing(1, 1, 4, 1, 4, 4, 1, 4.5);
    LinearRing inner = createLinearRing(2, 2, 3, 2, 3, 3, 2, 3.5);
    List<LinearRing> shells = ImmutableList.of(outer, middle, inner);

    Geometry geometry = sanitizer.apply(shells);
    assertEquals(MultiLineString.class, geometry.getClass());

    assertEquals(outer, geometry.getGeometryN(0));
    assertEquals(middle, geometry.getGeometryN(1));
    assertEquals(inner, geometry.getGeometryN(2));
  }

  void checkGeometry(Coordinate[] expected, Class<? extends Geometry> expectedClass,
      Geometry actual) {
    assertEquals(expectedClass, actual.getClass());
    assertArrayEquals(expected, actual.getCoordinates());
  }

  void checkPolygon(LinearRing shell, List<LinearRing> holes, Geometry actual) {
    assertEquals(Polygon.class, actual.getClass());
    Polygon p = (Polygon) actual;
    assertArrayEquals(shell.getCoordinates(), p.getExteriorRing().getCoordinates());
    assertEquals(holes.size(), p.getNumInteriorRing());
    Set<Integer> verified = new HashSet<>();
    for (int i = 0; i < p.getNumInteriorRing(); i++) {
      for (int j = 0; j < holes.size(); j++) {
        if (!verified.contains(j)
            && holes.get(j).getCoordinates().equals(p.getInteriorRingN(i).getCoordinates())) {
          verified.add(j);
        }
      }
    }
    assertEquals(holes.size(), verified.size());
  }


}
