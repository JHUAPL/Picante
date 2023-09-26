package picante.demo;

import static org.junit.Assert.assertEquals;
import static org.junit.Assume.assumeNoException;

import picante.math.functions.DifferentiableUnivariateFunction;
import picante.math.intervals.Interval;
import picante.math.intervals.IntervalSet;
import picante.math.intervals.UnwritableInterval;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.*;
import picante.mechanics.providers.aberrated.AberratedEphemerisProvider;
import picante.mechanics.providers.aberrated.AberrationCorrection;
import picante.roots.RootFinder;
import picante.roots.Stepper;
import picante.roots.Steppers;
import picante.spice.SpiceEnvironment;
import picante.utilities.InitSpice;
import picante.time.TimeConversion;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import org.junit.Test;

/**
 * Example showing analog to GFDIST. We create a {@link DifferentiableUnivariateFunction} for the
 * range from observer to target and search for time ranges when the distance is greater than a
 * desired value.
 *
 * <p>Picante's root finder is implemented differently than NAIF's solver and the results differ
 * slightly from the NAIF example for <a href=
 * "https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/IDL/icy/cspice_gfdist.html">CSPICE_GFDIST</a>.
 *
 * <p>This procedure can be followed for quantities other than distance; analogs to GFILUM, GFPA,
 * and other geometry-finding routines can be implemented in a similar fashion.
 */
public class TestGFDIST {
  @Test
  public void test01() {
    List<String> kernels = new ArrayList<>();
    kernels.add("lsk/naif0009.tls");
    kernels.add("pck/pck00008.tpc");
    kernels.add("spk/de414.bsp");

    SpiceEnvironment env;
    try {
      env = InitSpice.getSpiceEnvironment(kernels);
    } catch (RuntimeException e) {
      System.err.println(e.getLocalizedMessage());
      System.err.println(
              "Try running src/test/resources/kernels/spk/getSPK.bash to download kernels.");
      assumeNoException(e);
      return;
    }
    TimeConversion tc = new TimeConversion(env.getLSK());
    AberratedEphemerisProvider provider = InitSpice.getAberratedProvider(env, false);
    Function<Double, String> timeFormatter =
        tc.format("YYYY-MON-DD HR:MN:SC.###### (TDB) ::TDB ::RND");

    EphemerisID target = CelestialBodies.MOON;
    EphemerisID observer = CelestialBodies.EARTH;
    FrameID frameID = CelestialFrames.J2000;

    // specify time range and reference distance
    Interval searchInterval =
        new Interval(tc.utcStringToTDB("2007 Jan 01"), tc.utcStringToTDB("2007 Apr 01"));
    double refVal = 4e5;

    StateVectorFunction stateVectorFunction =
        provider.createAberratedStateVectorFunction(
            target, observer, frameID, Coverage.ALL_TIME, AberrationCorrection.NONE);

    DifferentiableUnivariateFunction distFunc =
        new DifferentiableUnivariateFunction() {

          @Override
          public double evaluate(double t) {
            return stateVectorFunction.getPosition(t).getLength();
          }

          @Override
          public double differentiate(double t) {
            StateVector state = stateVectorFunction.getState(t);
            VectorIJK pos = state.getPosition();
            VectorIJK vel = state.getVelocity();

            double r = pos.getLength();
            return pos.getDot(vel) / r;
          }
        };

    // Use a root finder to return time intervals when the intersectionFunc is true
    RootFinder finder = RootFinder.create();

    // The step size should be small enough to catch each transition.
    Stepper stepper = Steppers.createConstant(86400);

    IntervalSet eventIntervals =
        finder.locateGreaterThanValue(
            distFunc, refVal, IntervalSet.create(searchInterval), stepper);

    // expected values are from the IDL demo code
    List<UnwritableInterval> expectedIntervals = new ArrayList<>();
    expectedIntervals.add(new UnwritableInterval(2.2148706762382668E+08, 2.2194226795470613E+08));
    expectedIntervals.add(new UnwritableInterval(2.2384455527911007E+08, 2.2437186184410954E+08));
    expectedIntervals.add(new UnwritableInterval(2.2615322518364078E+08, 2.2680747849760610E+08));
    expectedIntervals.add(new UnwritableInterval(2.2848083814700079E+08, 2.2865766518565452E+08));

    assertEquals(eventIntervals.size(), expectedIntervals.size());

    for (int i = 0; i < eventIntervals.size(); i++) {
      UnwritableInterval eventInterval = eventIntervals.get(i);
      UnwritableInterval expectedInterval = expectedIntervals.get(i);
      assertEquals(eventInterval.getBegin(), expectedInterval.getBegin(), 0.1);
      assertEquals(eventInterval.getEnd(), expectedInterval.getEnd(), 0.1);
      System.out.printf("From : %s\n", timeFormatter.apply(eventInterval.getBegin()));
      System.out.printf("To   : %s\n\n", timeFormatter.apply(eventInterval.getEnd()));
    }
  }
}
