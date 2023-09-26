package picante.math.cones;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import java.util.ArrayList;
import java.util.List;
import org.junit.Test;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;
import picante.surfaces.Ellipse;

public class TestCones {

  @Test
  public void testEllipticalCone() {
    final double TOLERANCE = 1e-15;

    Cone cone = Cones.createEllipticalCone(VectorIJK.ZERO,
        Ellipse.create(VectorIJK.I, VectorIJK.J, VectorIJK.K));

    assertTrue(cone.getVertex().getLength() < TOLERANCE);
    assertTrue(cone.getInteriorPoint().getSeparation(VectorIJK.I) < TOLERANCE);

    assertTrue(new VectorIJK(1, 1, 0).getSeparation(cone.getEdge(0)) < TOLERANCE);
    assertTrue(new VectorIJK(1, 0, 1).getSeparation(cone.getEdge(Math.PI / 2)) < TOLERANCE);
    assertTrue(new VectorIJK(1, -1, 0).getSeparation(cone.getEdge(Math.PI)) < TOLERANCE);
    assertTrue(new VectorIJK(1, 0, -1).getSeparation(cone.getEdge(3 * Math.PI / 2)) < TOLERANCE);
  }

  @Test
  public void testRectangularCone() {
    final double TOLERANCE = 1e-15;

    double ySize = Math.toRadians(10);
    double zSize = Math.toRadians(5);

    PolygonalCone cone =
        Cones.createRectangularCone(VectorIJK.ZERO, VectorIJK.I, VectorIJK.J, ySize, zSize);

    double tanY = Math.tan(ySize);
    double tanZ = Math.tan(zSize);

    assertTrue(new VectorIJK(1, 0, tanZ).getSeparation(cone.getEdge(0.5)) < TOLERANCE);
    assertTrue(new VectorIJK(1, -tanY, 0).getSeparation(cone.getEdge(1.5)) < TOLERANCE);
    assertTrue(new VectorIJK(1, 0, -tanZ).getSeparation(cone.getEdge(2.5)) < TOLERANCE);
    assertTrue(new VectorIJK(1, tanY, 0).getSeparation(cone.getEdge(3.5)) < TOLERANCE);
  }

  @Test
  public void testPolygonalCone() {
    final double TOLERANCE = 1e-15;

    double ySize = Math.toRadians(10);
    double zSize = Math.toRadians(5);

    List<UnwritableVectorIJK> pts3d = new ArrayList<>();
    pts3d.add(new UnwritableVectorIJK(1, Math.tan(ySize), Math.tan(zSize)).createUnitized());
    pts3d.add(new UnwritableVectorIJK(1, -Math.tan(ySize), Math.tan(zSize)).createUnitized());
    pts3d.add(new UnwritableVectorIJK(1, -Math.tan(ySize), -Math.tan(zSize)).createUnitized());
    pts3d.add(new UnwritableVectorIJK(1, Math.tan(ySize), -Math.tan(zSize)).createUnitized());

    Cone cone = Cones.createPolygonalCone(VectorIJK.ZERO, VectorIJK.I, pts3d);

    assertTrue(new VectorIJK(1, 0, Math.tan(zSize)).getSeparation(cone.getEdge(0.5)) < TOLERANCE);
    assertTrue(new VectorIJK(1, -Math.tan(ySize), 0).getSeparation(cone.getEdge(1.5)) < TOLERANCE);
    assertTrue(new VectorIJK(1, 0, -Math.tan(zSize)).getSeparation(cone.getEdge(2.5)) < TOLERANCE);
    assertTrue(new VectorIJK(1, Math.tan(ySize), 0).getSeparation(cone.getEdge(3.5)) < TOLERANCE);
  }

  @Test
  public void testEllipseContains() {
    Cone cone = Cones.createEllipticalCone(VectorIJK.ZERO,
        Ellipse.create(VectorIJK.I, VectorIJK.J, VectorIJK.K));

    double step = 0.01;
    assertTrue(Cones.contains(cone, VectorIJK.I, step));
    assertFalse(Cones.contains(cone, VectorIJK.J, step));
    assertFalse(Cones.contains(cone, VectorIJK.K, step));

    assertTrue(Cones.contains(cone, new UnwritableVectorIJK(1, 0.99, 0), step));
    assertFalse(Cones.contains(cone, new UnwritableVectorIJK(1, 1.01, 0), step));

    assertFalse(Cones.contains(cone, new UnwritableVectorIJK(0.99, 1, 0), step));
    assertTrue(Cones.contains(cone, new UnwritableVectorIJK(1.01, 1, 0), step));

    assertFalse(Cones.contains(cone, new UnwritableVectorIJK(1, 0, 1.01), step));
    assertTrue(Cones.contains(cone, new UnwritableVectorIJK(1, 0, 0.99), step));
  }

  @Test
  public void testPolygonContains() {

    double ySize = Math.toRadians(10);
    double zSize = Math.toRadians(5);

    List<UnwritableVectorIJK> pts3d = new ArrayList<>();
    pts3d.add(new UnwritableVectorIJK(1, Math.tan(ySize), Math.tan(zSize)).createUnitized());
    pts3d.add(new UnwritableVectorIJK(1, -Math.tan(ySize), Math.tan(zSize)).createUnitized());
    pts3d.add(new UnwritableVectorIJK(1, -Math.tan(ySize), -Math.tan(zSize)).createUnitized());
    pts3d.add(new UnwritableVectorIJK(1, Math.tan(ySize), -Math.tan(zSize)).createUnitized());

    Cone cone = Cones.createPolygonalCone(VectorIJK.ZERO, VectorIJK.I, pts3d);

    double step = 0.01;
    assertTrue(Cones.contains(cone, VectorIJK.I, step));
    assertFalse(Cones.contains(cone, VectorIJK.J, step));
    assertFalse(Cones.contains(cone, VectorIJK.K, step));

    assertTrue(Cones.contains(cone, new UnwritableVectorIJK(1, 0.99 * Math.tan(ySize), 0), step));
    assertFalse(Cones.contains(cone, new UnwritableVectorIJK(1, 1.01 * Math.tan(ySize), 0), step));

    assertTrue(Cones.contains(cone, new UnwritableVectorIJK(1, 0, 0.99 * Math.tan(zSize)), step));
    assertFalse(Cones.contains(cone, new UnwritableVectorIJK(1, 0, 1.01 * Math.tan(zSize)), step));

    assertTrue(Cones.contains(cone, new UnwritableVectorIJK(1, -0.99 * Math.tan(ySize), 0), step));
    assertFalse(Cones.contains(cone, new UnwritableVectorIJK(1, -1.01 * Math.tan(ySize), 0), step));

    assertTrue(Cones.contains(cone, new UnwritableVectorIJK(1, 0, -0.99 * Math.tan(zSize)), step));
    assertFalse(Cones.contains(cone, new UnwritableVectorIJK(1, 0, -1.01 * Math.tan(zSize)), step));
  }

}
