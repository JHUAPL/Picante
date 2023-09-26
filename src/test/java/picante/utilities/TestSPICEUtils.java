package picante.utilities;

import static org.junit.Assert.assertEquals;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.junit.Test;
import com.google.common.collect.ImmutableList;
import picante.math.coords.LatitudinalVector;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.mechanics.CelestialBodies;
import picante.spice.SpiceEnvironment;
import picante.surfaces.Ellipsoid;
import picante.surfaces.Surfaces;

public class TestSPICEUtils {

  @Test
  public void testGEOREC() {

    List<String> kernels = new ArrayList<>();
    kernels.add("lsk/naif0012.tls");
    kernels.add("pck/pck00010.tpc");
    SpiceEnvironment env = InitSpice.getSpiceEnvironment(kernels);
    ImmutableList<Double> radii = env.getBodyRadii().get(CelestialBodies.EARTH);
    Ellipsoid shape = Surfaces.createEllipsoidalSurface(radii.get(0), radii.get(1), radii.get(2));

    // test values from https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/IDL/icy/cspice_georec.html
    LatitudinalVector lv = new LatitudinalVector(1, Math.toRadians(30), Math.toRadians(118));
    double altitude = 0;

    UnwritableVectorIJK rec = SPICEUtils.GEOREC(shape, lv, altitude);

    assertEquals(-2595.359123, rec.getI(), 1e-6);
    assertEquals(4881.160589, rec.getJ(), 1e-6);
    assertEquals(3170.373523, rec.getK(), 1e-6);

    System.out.printf(
        "Geodetic coordinates in deg and km (lon, lat, alt)\n%14.6f %14.6f %14.6f\n",
        Math.toDegrees(lv.getLongitude()), Math.toDegrees(lv.getLatitude()), lv.getRadius());
    System.out.printf(
        "Rectangular coordinates in km (x, y, z)\n%14.6f %14.6f %14.6f\n",
        rec.getI(), rec.getJ(), rec.getK());
  }

  @Test
  public void testRECGEO() {
    List<String> kernels = new ArrayList<>();
    kernels.add("lsk/naif0012.tls");
    kernels.add("pck/pck00010.tpc");
    SpiceEnvironment env = InitSpice.getSpiceEnvironment(kernels);
    ImmutableList<Double> radii = env.getBodyRadii().get(CelestialBodies.EARTH);
    Ellipsoid shape = Surfaces.createEllipsoidalSurface(radii.get(0), radii.get(1), radii.get(2));

    UnwritableVectorIJK rec = new UnwritableVectorIJK(-2541.748162, 4780.333036, 3360.428190);

    LatitudinalVector lv = SPICEUtils.RECGEO(shape, rec);

    assertEquals(118.000000, Math.toDegrees(lv.getLongitude()), 1e-6);
    assertEquals(31.999957, Math.toDegrees(lv.getLatitude()), 1e-6);
    assertEquals(0.001916, lv.getRadius(), 1e-6);

    System.out.printf(
        "Rectangular coordinates in km (x, y, z)\n%14.6f %14.6f %14.6f\n",
        rec.getI(), rec.getJ(), rec.getK());
    System.out.printf(
        "Geodetic coordinates in deg and km (lon, lat, alt)\n%14.6f %14.6f %14.6f\n",
        Math.toDegrees(lv.getLongitude()), Math.toDegrees(lv.getLatitude()), lv.getRadius());

    // example 2:
    double CLARKR = 6378.2064d;
    double CLARKF = 1.d / 294.9787d;

    Ellipsoid clarke = Surfaces.createEllipsoidalSurface(CLARKR, CLARKR, CLARKR - CLARKF * CLARKR);

    Map<UnwritableVectorIJK, LatitudinalVector> x = new LinkedHashMap<>();
    x.put(
        new UnwritableVectorIJK(0.d, 0.d, 0.d),
        new LatitudinalVector(-6356.584, Math.toRadians(90), Math.toRadians(0)));
    x.put(
        new UnwritableVectorIJK(6378.2064d, 0.d, 0.d),
        new LatitudinalVector(0, Math.toRadians(0), Math.toRadians(0)));
    x.put(
        new UnwritableVectorIJK(0.d, 6378.2064d, 0.d),
        new LatitudinalVector(0, Math.toRadians(0), Math.toRadians(90)));
    x.put(
        new UnwritableVectorIJK(0.d, 0.d, 6378.2064d),
        new LatitudinalVector(21.623, Math.toRadians(90), Math.toRadians(0)));
    x.put(
        new UnwritableVectorIJK(-6378.2064d, 0.d, 0.d),
        new LatitudinalVector(0, Math.toRadians(0), Math.toRadians(180)));
    x.put(
        new UnwritableVectorIJK(0.d, -6378.2064d, 0.d),
        new LatitudinalVector(0, Math.toRadians(0), Math.toRadians(-90)));
    x.put(
        new UnwritableVectorIJK(0.d, 0.d, -6378.2064d),
        new LatitudinalVector(21.623, Math.toRadians(-90), Math.toRadians(0)));
    x.put(
        new UnwritableVectorIJK(6378.2064d, 6378.2064d, 0.d),
        new LatitudinalVector(2641.940, Math.toRadians(0), Math.toRadians(45)));
    x.put(
        new UnwritableVectorIJK(6378.2064d, 0.d, 6378.2064d),
        new LatitudinalVector(2652.768, Math.toRadians(45.137), Math.toRadians(0)));
    x.put(
        new UnwritableVectorIJK(0.d, 6378.2064d, 6378.2064d),
        new LatitudinalVector(2652.768, Math.toRadians(45.137), Math.toRadians(90)));
    x.put(
        new UnwritableVectorIJK(6378.2064d, 6378.2064d, 6378.2064d),
        new LatitudinalVector(4676.389, Math.toRadians(35.370), Math.toRadians(45)));

    System.out.printf(
        "\n\n%11s%11s%11s%9s%9s%11s\n", "rectan[0]", "rectan[1]", "rectan[2]", "lon", "lat", "alt");

    for (UnwritableVectorIJK thisX : x.keySet()) {
      LatitudinalVector geo = SPICEUtils.RECGEO(clarke, thisX);
      LatitudinalVector expected = x.get(thisX);

      assertEquals(expected.getRadius(), geo.getRadius(), 1e-3);
      assertEquals(Math.toDegrees(expected.getLatitude()), Math.toDegrees(geo.getLatitude()), 1e-3);
      assertEquals(
          Math.toDegrees(expected.getLongitude()), Math.toDegrees(geo.getLongitude()), 1e-3);

      System.out.printf(
          "%11.3f%11.3f%11.3f%9.3f%9.3f%11.3f\n",
          thisX.getI(),
          thisX.getJ(),
          thisX.getK(),
          Math.toDegrees(geo.getLongitude()),
          Math.toDegrees(geo.getLatitude()),
          geo.getRadius());
    }
  }
}
