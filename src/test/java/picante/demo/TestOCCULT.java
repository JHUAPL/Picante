package picante.demo;

import com.google.common.collect.ImmutableList;
import picante.math.intervals.IntervalSet;
import picante.math.intervals.UnwritableInterval;
import picante.mechanics.*;
import picante.mechanics.providers.aberrated.AberratedEphemerisProvider;
import picante.mechanics.providers.aberrated.AberrationCorrection;
import picante.mechanics.utilities.SimpleEphemerisID;
import picante.roots.BooleanStateFinder;
import picante.roots.Stepper;
import picante.roots.Steppers;
import picante.roots.UnivariateBooleanFunction;
import picante.spice.SpiceEnvironment;
import picante.surfaces.Ellipsoid;
import picante.surfaces.Surfaces;
import picante.utilities.InitSpice;
import picante.time.TimeConversion;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import org.junit.Test;

import static org.junit.Assume.assumeNoException;

/**
 * A little more complex example, showing how to find transits and occultations like the SPICE
 * function <a href=
 * "https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/spicelib/occult.html">OCCULT</a>.
 *
 * <p>This does the ellipsoid shape only as DSK shape models are not supported.
 *
 * @author Hari.Nair@jhuapl.edu
 */
public class TestOCCULT {

  @Test
  public void test01() {

    List<String> kernels = new ArrayList<>();
    kernels.add("spk/de421.bsp");
    kernels.add("spk/mro_psp22.bsp");
    kernels.add("spk/earthstns_itrf93_050714.bsp");
    kernels.add("pck/earth_latest_high_prec.bpc");
    kernels.add("lsk/naif0010.tls");
    kernels.add("pck/pck00010.tpc");
    kernels.add("fk/earth_topo_050714.tf");

    EphemerisID targ1 = new SimpleEphemerisID("MRO");
    EphemerisID targ2 = CelestialBodies.MARS;
    EphemerisID observr = new SimpleEphemerisID("DSS-13");
    FrameID bodyFixed = CelestialFrames.IAU_MARS;

    // use triple iterated provider for "CN" light time correction
    AberrationCorrection abcorr = AberrationCorrection.LT;

    Map<String, EphemerisID> bindings = new HashMap<>();
    bindings.put("MRO", targ1);
    bindings.put("DSS-13", observr);
    SpiceEnvironment env;
    try {
      env = InitSpice.getSpiceEnvironment(kernels, bindings);
    } catch (RuntimeException e) {
      System.err.println(e.getLocalizedMessage());
      System.err.println(
              "Try running src/test/resources/kernels/spk/getSPK.bash to download kernels.");
      assumeNoException(e);
      return;
    }
    TimeConversion tc = new TimeConversion(env.getLSK());

    AberratedEphemerisProvider provider = InitSpice.getAberratedProvider(env, true);

    double etStart = tc.utcStringToTDB("2012-jan-5 1:15:00");
    double etEnd = tc.utcStringToTDB("2012-jan-5 2:50:00");
    Function<Double, String> timeFormatter = tc.format("YYYY-MM-DD HR:MN UTC");

    StateVectorFunction obsToTarg1Func =
        provider.createAberratedStateVectorFunction(
            targ1, observr, bodyFixed, Coverage.ALL_TIME, abcorr);
    StateVectorFunction obsToTarg2Func =
        provider.createAberratedStateVectorFunction(
            targ2, observr, bodyFixed, Coverage.ALL_TIME, abcorr);
    ImmutableList<Double> radii = env.getBodyRadii().get(targ2);
    Ellipsoid targ2Shape =
        Surfaces.createEllipsoidalSurface(radii.get(0), radii.get(1), radii.get(2));

    // Define a function which evaluates to true if a ray pointing from observer to targ1 intersects
    // the shape of targ2.
    UnivariateBooleanFunction intersectionFunc =
            t -> targ2Shape.intersects(
                obsToTarg2Func.getPosition(t).negate(), obsToTarg1Func.getPosition(t));

    // print out the occultation status at discrete times
    for (double et = etStart; et < etEnd; et += 1000) {
      String type = "not occulted by";
      if (intersectionFunc.evaluate(et)) {
        type = "occulted by";
        if (obsToTarg1Func.getPosition(et).getLength() < obsToTarg2Func.getPosition(et).getLength())
          type = "transits";
      }
      System.out.printf(
          "%s: %s %s %s as seen from %s\n",
          timeFormatter.apply(et), targ1.getName(), type, targ2.getName(), observr.getName());
    }

    System.out.println("\nNow searching for event windows:");

    // Use a root finder to return time intervals when the intersectionFunc is true
    BooleanStateFinder finder = BooleanStateFinder.create();

    // The step size should be small enough to catch each transition.
    Stepper stepper = Steppers.createConstant(600);

    IntervalSet eventWindows =
        finder.locateTransitionToTrue(
            intersectionFunc, IntervalSet.create(new UnwritableInterval(etStart, etEnd)), stepper);

    // If the distance to targ1 > distance to targ2, it's an occultation. Otherwise it's a transit.
    for (UnwritableInterval event : eventWindows) {
      String type = "occulted by";
      if (obsToTarg1Func.getPosition(event.getBegin()).getLength()
          < obsToTarg2Func.getPosition(event.getBegin()).getLength()) type = "transits";
      System.out.printf(
          "%s to %s: %s %s %s as seen from %s\n",
          timeFormatter.apply(event.getBegin()),
          timeFormatter.apply(event.getEnd()),
          targ1.getName(),
          type,
          targ2.getName(),
          observr.getName());
    }
  }
}
