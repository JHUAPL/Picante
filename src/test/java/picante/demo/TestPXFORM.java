package picante.demo;

import static org.junit.Assert.assertEquals;
import picante.math.coords.CoordConverters;
import picante.math.coords.RaDecVector;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.CelestialFrames;
import picante.mechanics.Coverage;
import picante.mechanics.FrameTransformFunction;
import picante.mechanics.providers.aberrated.AberratedEphemerisProvider;
import picante.spice.SpiceEnvironment;
import picante.utilities.InitSpice;
import picante.time.TimeConversion;
import java.util.*;
import org.junit.Test;

/**
 * Return the matrix that transforms position vectors from one specified frame to another at a
 * specified epoch.
 *
 * <p>Based on the example pxform_ex2 at <a href=
 * "https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/IDL/icy/cspice_pxform.html">CSPICE_PXFORM</a>.
 */
public class TestPXFORM {
  @Test
  public void test01() {

    List<String> kernels = new ArrayList<>();
    kernels.add("lsk/naif0009.tls");
    kernels.add("pck/pck00009.tpc");

    SpiceEnvironment env = InitSpice.getSpiceEnvironment(kernels);
    TimeConversion tc = new TimeConversion(env.getLSK());

    AberratedEphemerisProvider provider = InitSpice.getAberratedProvider(env, false);
    FrameTransformFunction ftf =
        provider.createFrameTransformFunction(
            CelestialFrames.IAU_EARTH, CelestialFrames.J2000, Coverage.ALL_TIME);

    // expected values are from the IDL demo code
    List<Map.Entry<Double, Double>> expectedValues = new ArrayList<>();
    expectedValues.add(new AbstractMap.SimpleEntry<>(180.06410, 89.944300));
    expectedValues.add(new AbstractMap.SimpleEntry<>(180.06357, 89.944764));
    expectedValues.add(new AbstractMap.SimpleEntry<>(180.06303, 89.945228));
    expectedValues.add(new AbstractMap.SimpleEntry<>(180.06250, 89.945692));
    expectedValues.add(new AbstractMap.SimpleEntry<>(180.06196, 89.946155));
    expectedValues.add(new AbstractMap.SimpleEntry<>(180.06143, 89.946619));
    expectedValues.add(new AbstractMap.SimpleEntry<>(180.06090, 89.947083));
    expectedValues.add(new AbstractMap.SimpleEntry<>(180.06036, 89.947547));
    expectedValues.add(new AbstractMap.SimpleEntry<>(180.05983, 89.948011));
    expectedValues.add(new AbstractMap.SimpleEntry<>(180.05930, 89.948475));
    expectedValues.add(new AbstractMap.SimpleEntry<>(180.05876, 89.948938));
    expectedValues.add(new AbstractMap.SimpleEntry<>(180.05823, 89.949402));

    double begin = tc.utcStringToTDB("1 Jan 1990");
    double end = tc.utcStringToTDB("1 Jan 1991");
    double step = (end - begin) / 12;
    for (int i = 0; i < 12; i++) {
      double et = begin + i * step;
      RotationMatrixIJK rotate = ftf.getTransform(et);

      // this is the pole direction in J2000
      VectorIJK pole = rotate.mxv(VectorIJK.K);
      RaDecVector raDecVector = CoordConverters.convertToRaDec(pole);

      Map.Entry<Double, Double> expectedValue = expectedValues.get(i);
      assertEquals(expectedValue.getKey(), Math.toDegrees(raDecVector.getRightAscension()), 1e-5);
      assertEquals(expectedValue.getValue(), Math.toDegrees(raDecVector.getDeclination()), 1e-6);

      System.out.printf(
          "%12.7e %.4f %.6f\n",
          et,
          Math.toDegrees(raDecVector.getRightAscension()),
          Math.toDegrees(raDecVector.getDeclination()));
    }
  }
}
