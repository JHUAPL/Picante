package picante.demo;

import picante.math.coords.CoordConverters;
import picante.math.coords.LatitudinalVector;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.*;
import picante.mechanics.providers.aberrated.AberrationCorrection;
import picante.spice.SpiceEnvironment;
import picante.spice.adapters.AdapterInstantiationException;
import picante.utilities.InitSpice;
import picante.time.TimeConversion;
import java.util.ArrayList;
import java.util.List;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assume.assumeNoException;

/**
 * Convert from latitudinal to rectangular coordinates. Analogous to NAIF routine LATREC.
 *
 * <p>Based on the example for <a href=
 * "https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/IDL/icy/cspice_latrec.html">CSPICE_LATREC</a>.
 *
 * @author Hari.Nair@jhuapl.edu
 */
public class TestLATREC {
  @Test
  public void test01() {

    List<String> kernels = new ArrayList<>();
    kernels.add("lsk/naif0012.tls");
    kernels.add("spk/de421.bsp");

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

    EphemerisID target = CelestialBodies.MOON;
    EphemerisID observer = CelestialBodies.EARTH;
    FrameID frameID = CelestialFrames.J2000;

    double et = tc.utcStringToTDB("2017 Mar 20");

    StateVectorFunction stateVectorFunction = null;

    try {
      stateVectorFunction =
          env.createSingleAberratedProvider()
              .createAberratedStateVectorFunction(
                  target, observer, frameID, Coverage.ALL_TIME, AberrationCorrection.NONE);
    } catch (EphemerisSourceIOException
        | EphemerisSourceLinkException
        | AdapterInstantiationException e) {
      e.printStackTrace();
      System.exit(1);
    }

    StateVector state = stateVectorFunction.getState(et);
    VectorIJK pos = state.getPosition();
    LatitudinalVector lv = CoordConverters.convertToLatitudinal(pos);
    UnwritableVectorIJK latrec = CoordConverters.convert(lv);

    System.out.println("\nOriginal rectangular coordinates:\n");
    System.out.printf(" X          (km): %20.8f\n", pos.getI());
    System.out.printf(" Y          (km): %20.8f\n", pos.getJ());
    System.out.printf(" Z          (km): %20.8f\n", pos.getK());
    System.out.println("\nLatitudinal coordinates:\n");

    assertEquals(403626.33912495, lv.getRadius(), 1e-8);
    assertEquals(-98.34959789, Math.toDegrees(lv.getLongitude()), 1e-8);
    assertEquals(-18.26566077, Math.toDegrees(lv.getLatitude()), 1e-8);

    System.out.printf(" Radius     (km): %20.8f\n", lv.getRadius());
    System.out.printf(" Longitude (deg): %20.8f\n", Math.toDegrees(lv.getLongitude()));
    System.out.printf(" Latitude  (deg): %20.8f\n", Math.toDegrees(lv.getLatitude()));
    System.out.println("\nConverted back to rectangular:\n");

    assertEquals(-55658.44323296, latrec.getI(), 1e-8);
    assertEquals(-379226.32931475, latrec.getJ(), 1e-8);
    assertEquals(-126505.93063865, latrec.getK(), 1e-8);

    System.out.printf(" X          (km): %20.8f\n", latrec.getI());
    System.out.printf(" Y          (km): %20.8f\n", latrec.getJ());
    System.out.printf(" Z          (km): %20.8f\n", latrec.getK());
  }
}
