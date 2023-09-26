package picante.demo;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assume.assumeNoException;

import picante.math.intervals.IntervalSet;
import picante.math.intervals.UnwritableInterval;
import picante.mechanics.EphemerisID;
import picante.mechanics.Mechanics;
import picante.mechanics.providers.aberrated.AberratedEphemerisProvider;
import picante.spice.SpiceEnvironment;
import picante.spice.adapters.SpiceEphemerisID;
import picante.utilities.InitSpice;
import picante.time.TimeConversion;
import java.util.*;
import java.util.function.Function;
import org.junit.Test;

/**
 * Find coverage for ephemeris objects in an SPK. Analogous to SPICE routine SPKCOV.
 *
 * <p>Based on demo for <a href=
 * "https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/IDL/icy/cspice_spkcov.html">CSPICE_SPKCOV</a>
 *
 * @author Hari.Nair@jhuapl.edu
 */
public class TestSPKCOV {
  @Test
  public void test01() {

    List<String> kernels = new ArrayList<>();
    kernels.add("lsk/naif0012.tls");
    kernels.add("spk/mgs_ext12_ipng_mgs95j.bsp");
    kernels.add("spk/mgs_ext26_ipng_mgs95j.bsp");

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
    Function<Double, String> timeFormatter =
        tc.format("YYYY MON DD HR:MN:SC.### (TDB) ::TDB ::RND");

    AberratedEphemerisProvider provider = InitSpice.getAberratedProvider(env, false);

    Set<EphemerisID> objects = provider.getKnownObjects(new LinkedHashSet<>());

    Optional<EphemerisID> mgs =
        objects.stream()
            .filter(o -> o instanceof SpiceEphemerisID && ((SpiceEphemerisID) o).getIDCode() == -94)
            .findFirst();
    assertTrue(mgs.isPresent());
    Map<EphemerisID, IntervalSet> expectedValues = new HashMap<>();
    expectedValues.put(
        mgs.get(),
        IntervalSet.create(
            Arrays.asList(
                new UnwritableInterval(1.1219040000000000e+08, 1.1945160000000000e+08),
                new UnwritableInterval(2.1379680000000000e+08, 2.1621960000000000e+08))));

    for (EphemerisID object : objects) {
      IntervalSet intervals =
          Mechanics.getBasicCoverageSnapshot(env.getEphemerisSources(), object, false);
      if (intervals.size() > 0) {

        IntervalSet expectedIntervals = expectedValues.get(object);
        assertEquals(expectedIntervals.size(), intervals.size());

        System.out.printf("Coverage for object %s\n", object.getName());
        for (int i = 0; i < intervals.size(); i++) {
          UnwritableInterval interval = intervals.get(i);

          assertEquals(expectedIntervals.get(i), interval);

          System.out.printf("Interval: %d\n", i);
          System.out.printf("Start   : %s\n", timeFormatter.apply(interval.getBegin()));
          System.out.printf("Stop    : %s\n\n", timeFormatter.apply(interval.getEnd()));
        }
      }
    }
  }
}
