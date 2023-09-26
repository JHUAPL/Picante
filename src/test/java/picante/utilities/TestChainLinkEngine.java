package picante.utilities;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import org.junit.Test;
import picante.math.intervals.IntervalSet;
import picante.math.intervals.UnwritableInterval;
import picante.mechanics.CelestialBodies;
import picante.mechanics.CelestialFrames;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;
import picante.mechanics.FrameTransformFunction;
import picante.mechanics.Mechanics;
import picante.mechanics.PositionVectorFunction;
import picante.mechanics.providers.aberrated.AberratedEphemerisProvider;
import picante.mechanics.utilities.ChainLinkEngine;
import picante.spice.SpiceEnvironment;
import picante.spice.adapters.SpiceClassedFrameID;
import picante.time.TimeConversion;

import static org.junit.Assume.assumeNoException;

/**
 * Test the ephemeris and frame chaining. For objects, trace the position vector back to {@link
 * CelestialBodies#SOLAR_SYSTEM_BARYCENTER}. For frames, trace the frame transforms back to {@link
 * CelestialFrames#J2000}.
 *
 * @author Hari.Nair@jhuapl.edu
 */
public class TestChainLinkEngine {

  @Test
  public void testEphemerisLinkage() {
    List<String> kernels = new ArrayList<>();
    kernels.add("lsk/naif0010.tls");
    kernels.add("pck/cpck26Jan2007.tpc");
    kernels.add("sclk/cas00145.tsc");
    kernels.add("fk/cas_v40.tf");
    kernels.add("ik/cas_uvis_v06.ti");
    kernels.add("spk/080428R_SCPSE_08045_08067.bsp");
    kernels.add("ck/08052_08057ra.bc");

    SpiceEnvironment env;
    try {
      env = InitSpice.getSpiceEnvironment(kernels);
    } catch (RuntimeException e) {
      System.err.println(e.getLocalizedMessage());
      System.err.println(
              "Try running ck/getCK.bash and spk/getSPK.bash in src/test/resources/kernels to download kernels.");
      assumeNoException(e);
      return;
    }
    TimeConversion tc = new TimeConversion(env.getLSK());
    AberratedEphemerisProvider provider = InitSpice.getAberratedProvider(env, false);

    ChainLinkEngine<EphemerisID, PositionVectorFunction> ephEngine =
        Mechanics.createPositionVectorEngine(env.getEphemerisSources());

    Set<EphemerisID> objects = provider.getKnownObjects(new LinkedHashSet<>());

    IntervalSet coveredIntervals = IntervalSet.LINE;

    // find the covered time range by intersecting all coverage windows
    for (EphemerisID object : objects) {
      IntervalSet intervals =
          Mechanics.getBasicCoverageSnapshot(env.getEphemerisSources(), object, false);
      if (!intervals.isEmpty()) coveredIntervals = coveredIntervals.intersect(intervals);
    }

    for (EphemerisID object : objects) {

      System.out.printf(
          "Known object: %s (%s)\n", object.getName(), object.getClass().getSimpleName());
      for (UnwritableInterval interval : coveredIntervals) {
        List<PositionVectorFunction> links = new ArrayList<>();
        if (ephEngine.populateLinkage(
            object, CelestialBodies.SOLAR_SYSTEM_BARYCENTER, interval.getMiddle(), links)) {
          System.out.printf(
              "\t%s-%s",
              tc.tdbToUTCString(interval.getBegin(), "ISOC"),
              tc.tdbToUTCString(interval.getEnd(), "ISOC"));
          for (PositionVectorFunction link : links)
            System.out.printf(", %s", link.getObserverID().getName());
          System.out.println();
        } else {
          System.err.println("\tlinkage failed for object " + object.getName());
          System.err.flush();
        }
      }
    }
  }

  @Test
  public void testFrameLinkage() {
    List<String> kernels = new ArrayList<>();
    kernels.add("lsk/naif0010.tls");
    kernels.add("pck/cpck26Jan2007.tpc");
    kernels.add("sclk/cas00145.tsc");
    kernels.add("fk/cas_v40.tf");
    kernels.add("ik/cas_uvis_v06.ti");
    kernels.add("spk/080428R_SCPSE_08045_08067.bsp");
    kernels.add("ck/08052_08057ra.bc");

    SpiceEnvironment env;
    try {
      env = InitSpice.getSpiceEnvironment(kernels);
    } catch (RuntimeException e) {
      System.err.println(e.getLocalizedMessage());
      System.err.println(
              "Try running ck/getCK.bash and spk/getSPK.bash in src/test/resources/kernels to download kernels.");
      assumeNoException(e);
      return;
    }
    TimeConversion tc = new TimeConversion(env.getLSK());
    AberratedEphemerisProvider provider = InitSpice.getAberratedProvider(env, false);

    ChainLinkEngine<FrameID, FrameTransformFunction> frameEngine =
        Mechanics.createFrameTransformEngine(env.getFrameSources());

    Set<FrameID> frames = provider.getKnownFrames(new LinkedHashSet<>());

    IntervalSet coveredIntervals = IntervalSet.LINE;

    // find the covered time range by intersecting all coverage windows
    for (FrameID frame : frames) {
      IntervalSet intervals =
          Mechanics.getBasicCoverageSnapshot(env.getFrameSources(), frame, false);
      if (!intervals.isEmpty()) coveredIntervals = coveredIntervals.intersect(intervals);
    }

    for (FrameID frame : frames) {

      System.out.printf("Known frame: %s (%s)", frame.getName(), frame.getClass().getSimpleName());
      if (frame instanceof SpiceClassedFrameID)
        System.out.printf(", class %d", ((SpiceClassedFrameID) frame).getClassID());
      System.out.println();
      for (UnwritableInterval interval : coveredIntervals) {
        List<FrameTransformFunction> links = new ArrayList<>();
        if (frameEngine.populateLinkage(
            frame, CelestialFrames.J2000, interval.getMiddle(), links)) {
          System.out.printf(
              "\t%s-%s",
              tc.tdbToUTCString(interval.getBegin(), "ISOC"),
              tc.tdbToUTCString(interval.getEnd(), "ISOC"));
          for (FrameTransformFunction link : links)
            System.out.printf(", %s", link.getToID().getName());
          System.out.println();
        } else {
          System.err.println("\tlinkage failed for frame " + frame.getName());
          System.err.flush();
        }
      }
    }
  }
}
