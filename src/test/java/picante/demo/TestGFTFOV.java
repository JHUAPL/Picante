package picante.demo;

import static org.junit.Assert.*;
import static org.junit.Assume.assumeNoException;

import com.google.common.collect.ImmutableList;
import picante.math.cones.Cone;
import picante.math.cones.Cones;
import picante.math.intervals.IntervalSet;
import picante.math.intervals.UnwritableInterval;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.*;
import picante.mechanics.providers.aberrated.AberratedEphemerisProvider;
import picante.mechanics.providers.aberrated.AberrationCorrection;
import picante.mechanics.utilities.SimpleEphemerisID;
import picante.roots.BooleanStateFinder;
import picante.roots.Stepper;
import picante.roots.Steppers;
import picante.roots.UnivariateBooleanFunction;
import picante.spice.SpiceEnvironment;
import picante.spice.fov.FOV;
import picante.spice.fov.FOVFactory;
import picante.spice.kernelpool.UnwritableKernelPool;
import picante.surfaces.Ellipse;
import picante.surfaces.Ellipsoid;
import picante.surfaces.Surfaces;
import picante.utilities.InitSpice;
import picante.utilities.SPICEUtils;
import picante.time.TimeConversion;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import org.junit.Test;

/**
 * Determine the times when any portion of an ellipsoid is contained within the field of view of an
 * instrument. Based on the example for <a href=
 * "https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/IDL/icy/cspice_gftfov.html">CSPICE_GFTFOV</a>.
 *
 * <p>This does not agree exactly with SPICE as the ellipsoid intersect calculations are done
 * differently.
 *
 * @author Hari.Nair@jhuapl.edu
 */
public class TestGFTFOV {
  @Test
  public void test01() {

    List<String> kernels = new ArrayList<>();
    kernels.add("lsk/naif0012.tls");
    kernels.add("pck/pck00010.tpc");
    kernels.add("spk/041014R_SCPSE_01066_04199.bsp");
    kernels.add("fk/cas_v40.tf");
    kernels.add("ck/04161_04164ra.bc");
    kernels.add("sclk/cas00071.tsc");
    kernels.add("ik/cas_iss_v10.ti");

    String instName = "CASSINI_ISS_NAC";
    EphemerisID target = CelestialBodies.valueOf("PHOEBE");
    FrameID targetFrame = CelestialFrames.valueOf("IAU_PHOEBE");
    EphemerisID observer = new SimpleEphemerisID("CASSINI");

    // need to bind the name "CASSINI" to its ephemeris ID
    Map<String, EphemerisID> bindings = new HashMap<>();
    bindings.put("CASSINI", observer);
    SpiceEnvironment env;
    try {
      env = InitSpice.getSpiceEnvironment(kernels, bindings);
    } catch (RuntimeException e) {
      System.err.println(e.getLocalizedMessage());
      System.err.println(
              "Try running ck/getCK.bash and spk/getSPK.bash in src/test/resources/kernels to download kernels.");
      assumeNoException(e);
      return;
    }
    TimeConversion tc = new TimeConversion(env.getLSK());
    Function<Double, String> timeFormatter =
        tc.format("YYYY-MON-DD HR:MN:SC.###### (TDB) ::TDB ::RND");

    double startTime = tc.tdbStringToTDB("2004 JUN 11 06:30:00 TDB");
    double stopTime = tc.tdbStringToTDB("2004 JUN 11 12:00:00 TDB");

    AberratedEphemerisProvider provider = InitSpice.getAberratedProvider(env, false);

    UnwritableKernelPool envPool = env.getPool();

    Map<String, Integer> bodn2c = SPICEUtils.BODN2C(env);
    assertTrue(bodn2c.containsKey(instName));

    // found the instrument in the kernel pool. Define the FOV cone.
    int instCode = bodn2c.get(instName);
    FOVFactory factory = new FOVFactory(envPool);
    FOV fov = factory.create(instCode);
    Cone fovCone = fov.getCone();
    FrameID instrFrame = fov.getFrameID();

    // Phoebe relative to Cassini in the IAU_PHOEBE frame
    StateVectorFunction stateVectorFunction =
        provider.createAberratedStateVectorFunction(
            target, observer, targetFrame, Coverage.ALL_TIME, AberrationCorrection.LT_S);

    FrameTransformFunction ftf =
        provider.createFrameTransformFunction(targetFrame, instrFrame, Coverage.ALL_TIME);

    UnivariateBooleanFunction searchFunc;
    boolean ellipsoidSearch = true;
    List<UnwritableInterval> expectedIntervals = new ArrayList<>();
    if (ellipsoidSearch) {

      // construct an ellipsoid shape for Phoebe
      ImmutableList<Double> radii = env.getBodyRadii().get(target);
      Ellipsoid targetShape =
          Surfaces.createEllipsoidalSurface(radii.get(0), radii.get(1), radii.get(2));

      // Define a function to check if the ellipsoid is in the FOV
      searchFunc =
          new UnivariateBooleanFunction() {
            @Override
            public boolean evaluate(double t) {

              // find Phoebe's limb seen from Cassini in IAU_PHOEBE
              VectorIJK obsToTarget = stateVectorFunction.getPosition(t);
              Ellipse limb = targetShape.computeLimb(obsToTarget, Ellipse.create());

              // rotate limb to instrument frame
              RotationMatrixIJK targetToInstrFrame = ftf.getTransform(t);
              limb.rotate(targetToInstrFrame);
              obsToTarget = targetToInstrFrame.mxv(obsToTarget);
              limb.offset(obsToTarget);

              Cone targetCone = Cones.createEllipticalCone(VectorIJK.ZERO, limb);

              return Cones.intersects(targetCone, fovCone);
            }
          };

      // expected values are from the IDL demo code
      expectedIntervals.add(new UnwritableInterval(1.4021132706698030E+08, 1.4021568395469636E+08));
      expectedIntervals.add(new UnwritableInterval(1.4021657658004552E+08, 1.4021850403850943E+08));
      expectedIntervals.add(new UnwritableInterval(1.4021939647639722E+08, 1.4022132424287945E+08));
      expectedIntervals.add(new UnwritableInterval(1.4022221628377229E+08, 1.4022414439716548E+08));
      expectedIntervals.add(new UnwritableInterval(1.4022503602064520E+08, 1.4022696473353595E+08));

    } else {

      // Define a function to check if the center of the target is in the FOV
      searchFunc =
          new UnivariateBooleanFunction() {
            @Override
            public boolean evaluate(double t) {
              VectorIJK obsToTarget = stateVectorFunction.getPosition(t);
              RotationMatrixIJK targetToInstrFrame = ftf.getTransform(t);
              return Cones.contains(fovCone, targetToInstrFrame.mxv(obsToTarget), 0.01);
            }
          };
    }

    // find time windows when searchFunc evaluates to true
    BooleanStateFinder finder = BooleanStateFinder.create();
    Stepper stepper = Steppers.createConstant(30);
    IntervalSet eventIntervals =
        finder.locateTransitionToTrue(
            searchFunc, IntervalSet.create(new UnwritableInterval(startTime, stopTime)), stepper);

    if (ellipsoidSearch) {
      assertEquals(eventIntervals.size(), expectedIntervals.size());
    }

    for (int i = 0; i < eventIntervals.size(); i++) {
      UnwritableInterval eventInterval = eventIntervals.get(i);
      UnwritableInterval expectedInterval = expectedIntervals.get(i);
      if (ellipsoidSearch) {
        assertEquals(eventInterval.getBegin(), expectedInterval.getBegin(), 1);
        assertEquals(eventInterval.getEnd(), expectedInterval.getEnd(), 1);
      }
      System.out.printf("From : %s\n", timeFormatter.apply(eventInterval.getBegin()));
      System.out.printf("To   : %s\n\n", timeFormatter.apply(eventInterval.getEnd()));
    }
  }
}
