package picante.demo;

import static org.junit.Assert.assertEquals;

import com.google.common.collect.ImmutableList;
import picante.math.coords.LatitudinalVector;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.*;
import picante.mechanics.providers.aberrated.AberratedEphemerisProvider;
import picante.spice.SpiceEnvironment;
import picante.surfaces.Ellipsoid;
import picante.surfaces.Surfaces;
import picante.utilities.InitSpice;
import picante.utilities.SPICEUtils;
import picante.time.TimeConversion;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.Test;

/**
 * Return the state transformation matrix from one frame to another at a specified epoch.
 *
 * <p>Based on demo for <a href=
 * "https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/IDL/icy/cspice_sxform.html">CSPICE_SXFORM</a>
 *
 * @author Hari.Nair@jhuapl.edu
 */
public class TestSXFORM {
  @Test
  public void test01() {

    List<String> kernels = new ArrayList<>();
    kernels.add("lsk/naif0009.tls");
    kernels.add("pck/pck00008.tpc");

    SpiceEnvironment env = InitSpice.getSpiceEnvironment(kernels);
    TimeConversion tc = new TimeConversion(env.getLSK());
    AberratedEphemerisProvider provider = InitSpice.getAberratedProvider(env, false);

    ImmutableList<Double> radii = env.getBodyRadii().get(CelestialBodies.EARTH);
    Ellipsoid shape = Surfaces.createEllipsoidalSurface(radii.get(0), radii.get(1), radii.get(2));

    double lon = Math.toRadians(118.25);
    double geodeticLat = Math.toRadians(34.05);
    double altitude = 0;

    UnwritableVectorIJK ijk = SPICEUtils.GEOREC(shape, new LatitudinalVector(1, geodeticLat, lon), altitude);

    // Rectangular coordinates of surface point
    VectorIJK surfaceIJK = shape.compute(VectorIJK.ZERO, ijk, new VectorIJK());

    // define a state vector at this position with velocity 0
    StateVector eState = new StateVector(surfaceIJK, VectorIJK.ZERO);

    StateTransformFunction stf =
        provider.createStateTransformFunction(
            CelestialFrames.IAU_EARTH, CelestialFrames.J2000, Coverage.ALL_TIME);

    double beginET = tc.utcStringToTDB("1990 Jan 1 00:00:00");

    StateTransform transform = stf.getStateTransform(beginET);
    /*-
        System.out.println("Rotation matrix:");
        for (int i = 0; i < 3; i++) {
          for (int j = 0; j < 3; j++)
            System.out.printf("%18.12f", transform.getRotation().get(i, j)); // applied to position
          for (int j = 0; j < 3; j++) System.out.printf("%18.12f", 0.); // applied to velocity
          System.out.println();
        }
        for (int i = 0; i < 3; i++) {
          for (int j = 0; j < 3; j++)
            System.out.printf("%18.12f", transform.getRotationDerivative().get(i, j)); // applied to
          // position
          for (int j = 0; j < 3; j++)
            System.out.printf("%18.12f", transform.getRotation().get(i, j)); // applied to velocity
          System.out.println();
        }
    */
    StateVector jState = transform.mxv(eState);

    Map<Double, StateVector> expectedValues = new HashMap<>();
    expectedValues.put(
        beginET,
        new StateVector(
            -4.1314629608797150E+03,
            -3.3083706719071056E+03,
            3.5470215255013850E+03,
            2.4124981025726810E-01,
            -3.0101943992738384E-01,
            2.3421585167437696E-04));

    assertEquals(
        1,
        expectedValues.get(beginET).getPosition().getLength() / jState.getPosition().getLength(),
        1e-13);
    assertEquals(
        0, expectedValues.get(beginET).getPosition().getSeparation(jState.getPosition()), 1e-11);

    assertEquals(
        1,
        expectedValues.get(beginET).getVelocity().getLength() / jState.getVelocity().getLength(),
        1e-13);
    assertEquals(
        0, expectedValues.get(beginET).getVelocity().getSeparation(jState.getVelocity()), 1e-11);

    System.out.printf("%-30s: %s\n", "Epoch", tc.tdbToUTCString(beginET, "C"));
    System.out.printf(
        "%-25s (km): %11.4f %11.4f %11.4f\n",
        "Position in J2000 frame",
        jState.getPosition().getI(),
        jState.getPosition().getJ(),
        jState.getPosition().getK());
    System.out.printf(
        "%-23s (km/s): %11.4f %11.4f %11.4f\n\n",
        "Velocity in J2000 frame",
        jState.getVelocity().getI(),
        jState.getVelocity().getJ(),
        jState.getVelocity().getK());

    beginET = tc.utcStringToTDB("1990 Feb 1 00:00:00");
    double endET = tc.utcStringToTDB("1991 Feb 1 00:00:00");

    int nSteps = 4;
    double step = (endET - beginET) / nSteps;

    expectedValues.put(
        beginET,
        new StateVector(
            -1.8764713161785398E+03,
            -4.9474745217334512E+03,
            3.5492274734910634E+03,
            3.6077509925344431E-01,
            -1.3658491340424694E-01,
            3.4760979088755664E-04));
    expectedValues.put(
        beginET + step,
        new StateVector(
            1.8751001184423260E+03,
            4.9454238622134235E+03,
            3.5528082936029432E+03,
            -3.6062608927962869E-01,
            1.3697782622378735E-01,
            -3.3871449855132381E-04));
    expectedValues.put(
        beginET + 2 * step,
        new StateVector(
            -1.8870730539768458E+03,
            -4.9433817709365794E+03,
            3.5493092789939628E+03,
            3.6047669493122553E-01,
            -1.3737057641146883E-01,
            3.2982594747958313E-04));
    expectedValues.put(
        beginET + 3 * step,
        new StateVector(
            1.8860423669913323E+03,
            4.9413201139425091E+03,
            3.5527262945078028E+03,
            -3.6032682887133033E-01,
            1.3776316369151348E-01,
            -3.2094408624391108E-04));

    for (int i = 0; i < nSteps; i++) {
      double et = i * step + beginET;
      transform = stf.getStateTransform(et);
      jState = transform.mxv(eState);

      assertEquals(
          1,
          expectedValues.get(et).getPosition().getLength() / jState.getPosition().getLength(),
          1e-13);
      assertEquals(
          0, expectedValues.get(et).getPosition().getSeparation(jState.getPosition()), 1e-11);

      assertEquals(
          1,
          expectedValues.get(et).getVelocity().getLength() / jState.getVelocity().getLength(),
          1e-13);
      assertEquals(
          0, expectedValues.get(et).getVelocity().getSeparation(jState.getVelocity()), 1e-11);

      System.out.printf("%-30s: %s\n", "Epoch", tc.tdbToUTCString(et, "C"));
      System.out.printf(
          "%-25s (km): %11.4f %11.4f %11.4f\n",
          "Position in J2000 frame",
          jState.getPosition().getI(),
          jState.getPosition().getJ(),
          jState.getPosition().getK());
      System.out.printf(
          "%-23s (km/s): %11.4f %11.4f %11.4f\n\n",
          "Velocity in J2000 frame",
          jState.getVelocity().getI(),
          jState.getVelocity().getJ(),
          jState.getVelocity().getK());
    }
  }
}
