package picante.demo;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assume.assumeNoException;

import picante.math.intervals.IntervalSet;
import picante.math.intervals.UnwritableInterval;
import picante.mechanics.FrameID;
import picante.mechanics.Mechanics;
import picante.mechanics.providers.aberrated.AberratedEphemerisProvider;
import picante.spice.SpiceEnvironment;
import picante.spice.adapters.SpiceClassedFrameID;
import picante.utilities.InitSpice;
import picante.time.TimeConversion;
import java.util.*;
import java.util.function.Function;
import org.junit.Test;

/**
 * Finds the coverage window for a specified frame. Analogous to SPICE routine CKCOV, but prints all
 * objects in the SPICE environment.
 *
 * <p>Based on demo code for <a href=
 * "https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/IDL/icy/cspice_ckcov.html">CSPICE_CKCOV</a>
 *
 * @author Hari.Nair@jhuapl.edu
 */
public class TestCKCOV {

  @Test
  public void test01() {
    List<String> kernels = new ArrayList<>();
    kernels.add("lsk/naif0010.tls");
    kernels.add("sclk/cas00145.tsc");
    kernels.add("ck/08052_08057ra.bc");

    SpiceEnvironment env;
    try {
      env = InitSpice.getSpiceEnvironment(kernels);
    } catch (RuntimeException e) {
      System.err.println(e.getLocalizedMessage());
      System.err.println(
              "Try running src/test/resources/kernels/ck/getCK.bash to download kernels.");
      assumeNoException(e);
      return;
    }
    TimeConversion tc = new TimeConversion(env.getLSK());
    AberratedEphemerisProvider provider = InitSpice.getAberratedProvider(env, false);

    Function<Double, String> timeFormatter = tc.format("YYYY MON DD HR:MN:SC.### (TDB) ::TDB");

    int expectedFrameID = -82000;

    // this set of frames contains built-in frames as well as any loaded from the C kernel.
    Set<FrameID> knownFrames = provider.getKnownFrames(new LinkedHashSet<>());

    // find the frame we want
    Optional<FrameID> expectedFrame =
        knownFrames.stream()
            .filter(f -> f instanceof SpiceClassedFrameID && ((SpiceClassedFrameID) f).getClassID() == expectedFrameID)
            .findFirst();
    assertTrue(expectedFrame.isPresent());
    FrameID frame = expectedFrame.get();

    // expected values are from the IDL demo code
    List<UnwritableInterval> expectedIntervals = new ArrayList<>();
    expectedIntervals.add(new UnwritableInterval(  2.5682406777118632E+08,  2.5707921000173840E+08));
    expectedIntervals.add(new UnwritableInterval(  2.5707949399973205E+08,  2.5709174591317555E+08));
    expectedIntervals.add(new UnwritableInterval(  2.5709206991088659E+08,  2.5715439347058728E+08));
    expectedIntervals.add(new UnwritableInterval(  2.5715457346931568E+08,  2.5718552125067776E+08));
    expectedIntervals.add(new UnwritableInterval(  2.5718577324889746E+08,  2.5722502497159436E+08));
    expectedIntervals.add(new UnwritableInterval(  2.5722505297139654E+08,  2.5724310484386480E+08));
    expectedIntervals.add(new UnwritableInterval(  2.5724314884355393E+08,  2.5725606475230640E+08));

    IntervalSet intervals = Mechanics.getBasicCoverageSnapshot(env.getFrameSources(), frame, false);
    if (intervals.size() > 0) {
      System.out.printf("Coverage for object %s\n", frame.getName());
      for (int i = 0; i < intervals.size(); i++) {

        UnwritableInterval interval = intervals.get(i);
        assertEquals(expectedIntervals.get(i).getBegin(), interval.getBegin(), 1e-6);
        assertEquals(expectedIntervals.get(i).getEnd(), interval.getEnd(), 1e-6);

        System.out.printf("Interval: %d\n", i);
        System.out.printf("Start   : %s\n", timeFormatter.apply(interval.getBegin()));
        System.out.printf("Stop    : %s\n\n", timeFormatter.apply(interval.getEnd()));
      }
    }
  }
}
