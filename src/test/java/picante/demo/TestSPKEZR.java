package picante.demo;

import static org.junit.Assert.assertEquals;
import static org.junit.Assume.assumeNoException;

import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.*;
import picante.mechanics.providers.aberrated.AberratedEphemerisProvider;
import picante.mechanics.providers.aberrated.AberrationCorrection;
import picante.spice.SpiceEnvironment;
import picante.units.FundamentalPhysicalConstants;
import picante.utilities.InitSpice;
import picante.time.TimeConversion;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.Test;

/**
 * Returns the state (position and velocity) of a target body relative to an observing body,
 * optionally corrected for light time (planetary aberration) and stellar aberration.
 *
 * <p>Based on the demo for <a href=
 * "https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/IDL/icy/cspice_spkezr.html">CSPICE_SPKEZR</a>
 *
 * @author Hari.Nair@jhuapl.edu
 */
public class TestSPKEZR {
  @Test
  public void test01() {

    List<String> kernels = new ArrayList<>();
    kernels.add("spk/de430.bsp");
    kernels.add("spk/mar097.bsp");
    kernels.add("lsk/naif0011.tls");

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

    EphemerisID target = CelestialBodies.MARS;
    EphemerisID observer = CelestialBodies.EARTH;
    FrameID frameID = CelestialFrames.J2000;
    String epoch = "2003 Jul 4 19:00:00";

    double tdb = tc.utcStringToTDB(epoch);

    AberratedEphemerisProvider provider = InitSpice.getAberratedProvider(env, false);

    StateVectorFunction stateVectorFunction = null;

    try {
      stateVectorFunction =
          provider.createAberratedStateVectorFunction(
              target, observer, frameID, Coverage.ALL_TIME, AberrationCorrection.LT_S);
    } catch (EphemerisSourceIOException | EphemerisSourceLinkException e) {
      e.printStackTrace();
      System.exit(1);
    }

    System.out.printf("The position of     : %s\n", target.getName());
    System.out.printf("As observed from    : %s\n", observer.getName());
    System.out.printf("In reference frame  : %s\n", frameID.getName());
    System.out.println();

    // these values come from running the SPKEZR demo IDL code
    Map<Integer, UnwritableVectorIJK> positions = new HashMap<>();
    Map<Integer, UnwritableVectorIJK> velocities = new HashMap<>();
    positions.put(
        0,
        new UnwritableVectorIJK(
            7.3822235331246316E+07, -2.7127919178447232E+07, -1.8741306284805853E+07));
    velocities.put(
        0,
        new UnwritableVectorIJK(
            -6.8085132875776733E+00, 7.5139961792670880E+00, 3.0012985050616376E+00));
    positions.put(
        1,
        new UnwritableVectorIJK(
            7.3140185437168494E+07, -2.6390524955072790E+07, -1.8446763015727006E+07));
    velocities.put(
        1,
        new UnwritableVectorIJK(
            -6.8312193500595368E+00, 7.2341557945991593E+00, 2.8896967125567907E+00));
    positions.put(
        2,
        new UnwritableVectorIJK(
            7.2456239685780525E+07, -2.5681031185418446E+07, -1.8163339123881858E+07));
    velocities.put(
        2,
        new UnwritableVectorIJK(
            -6.8464803936379512E+00, 6.9560178623820601E+00, 2.7789283561277713E+00));
    positions.put(
        3,
        new UnwritableVectorIJK(
            7.1771127035314590E+07, -2.4999259626991127E+07, -1.7890946613528430E+07));
    velocities.put(
        3,
        new UnwritableVectorIJK(
            -6.8546120767890306E+00, 6.6797296535362545E+00, 2.6690805636116668E+00));
    positions.put(
        4,
        new UnwritableVectorIJK(
            7.1085543856297791E+07, -2.4345021342679124E+07, -1.7629490685713712E+07));
    velocities.put(
        4,
        new UnwritableVectorIJK(
            -6.8559458447739443E+00, 6.4053554472290317E+00, 2.5602008025815719E+00));

    final int SIZE = 5;
    for (int i = 0; i < SIZE; i++) {
      double t = tdb + i * 100000;
      StateVector state = stateVectorFunction.getState(t);
      System.out.printf("At epoch            : %s\n", tc.tdbToUTCString(t, "C"));
      VectorIJK v = state.getPosition();
      UnwritableVectorIJK expected = positions.get(i);
      assertEquals(0, (expected.getLength() - v.getLength()) / v.getLength(), 1E-11);
      assertEquals(0, expected.getSeparation(v), 1E-11);
      System.out.printf("R (kilometers)      : %.0f %.0f %.0f\n", v.getI(), v.getJ(), v.getK());
      v = state.getVelocity();
      expected = velocities.get(i);
      assertEquals(0, (expected.getLength() - v.getLength()) / v.getLength(), 1E-11);
      assertEquals(0, expected.getSeparation(v), 1E-11);
      System.out.printf("V (kilometers/sec)  : %.7f %.7f %.7f\n", v.getI(), v.getJ(), v.getK());
      System.out.printf(
          "Light time (secs)   : %f\n",
          1e3
              * state.getPosition().getLength()
              / FundamentalPhysicalConstants.SPEED_OF_LIGHT_IN_VACUUM_M_per_SEC);
      System.out.println();
    }
  }
}
