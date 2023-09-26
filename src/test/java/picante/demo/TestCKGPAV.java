package picante.demo;

import static org.junit.Assert.*;
import static org.junit.Assume.assumeNoException;

import com.google.common.collect.ImmutableMap;
import picante.math.intervals.IntervalSet;
import picante.math.intervals.UnwritableInterval;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.*;
import picante.mechanics.rotations.DifferentiatedRotations;
import picante.spice.SpiceEnvironment;
import picante.spice.adapters.SpiceClassedFrameID;
import picante.spice.kernel.tk.sclk.SCLK;
import picante.spice.kernel.tk.sclk.SCLKKernel;
import picante.utilities.InitSpice;
import picante.utilities.SPICEUtils;
import java.util.*;
import org.junit.Test;

/**
 * Analog to SPICE routine CKGPAV, which returns pointing (attitude) and angular velocity for a
 * specified spacecraft clock time.
 *
 * <p>Based on demo code for <a href=
 * "https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/IDL/icy/cspice_ckgpav.html">CSPICE_CKGPAV</a>
 *
 * @author Hari.Nair@jhuapl.edu
 */
public class TestCKGPAV {

  @Test
  public void test01() {
    List<String> kernels = new ArrayList<>();
    kernels.add("ck/04153_04182ca_ISS.bc");
    kernels.add("lsk/naif0009.tls");
    kernels.add("sclk/cas00071.tsc");

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
    FrameProvider provider = InitSpice.getAberratedProvider(env, false);

    int SC = -82;
    int frameID = -82000;
    FrameID REF = CelestialFrames.J2000;
    UnwritableVectorIJK BORE = new UnwritableVectorIJK(0.0005760d, -0.99999982d, -0.0001710d);

    List<String> sclkTimes = new ArrayList<>();
    sclkTimes.add("1465644281.0");
    sclkTimes.add("1465644351.0");

    // search known frames for the one with the class ID -82000
    Optional<FrameID> expectedFrame =
        provider.getKnownFrames(new LinkedHashSet<>()).stream()
            .filter(
                f ->
                    f instanceof SpiceClassedFrameID
                        && ((SpiceClassedFrameID) f).getClassID() == frameID)
            .findFirst();
    assertTrue(expectedFrame.isPresent());
    FrameID INST = expectedFrame.get();

    StateTransformFunction stateTransformFunc =
        provider.createStateTransformFunction(REF, INST, Coverage.ALL_TIME);

    // check coverage windows for loaded C kernels
    IntervalSet.Builder builder = IntervalSet.builder();
    IntervalSet coverageIntervals =
        Mechanics.getBasicCoverageSnapshot(env.getFrameSources(), INST, false);

    ImmutableMap<Integer, SCLKKernel> sclkKernels = env.getSclkKernels();
    SCLKKernel sclkKernel = sclkKernels.get(SC);
    assertNotNull(sclkKernel);

    NavigableSet<Double> sclkdpBegins = new TreeSet<>();
    NavigableSet<Double> sclkdpEnds = new TreeSet<>();
    for (UnwritableInterval i : coverageIntervals) {
      double b = sclkKernel.convertToEncodedSclk(i.getBegin());
      double e = sclkKernel.convertToEncodedSclk(i.getEnd());
      sclkdpBegins.add(b);
      sclkdpEnds.add(e);
      builder.add(new UnwritableInterval(b, e));
    }
    IntervalSet sclkdpWindows = builder.build();

    // expected values are from the IDL demo code
    Map<String, UnwritableVectorIJK> expectedMap = new HashMap<>();
    expectedMap.put(
        "1465644281.0",
        new UnwritableVectorIJK(0.9376788904180611, 0.3444125245356940, 0.0462418903629186));
    expectedMap.put(
        "1465644351.0",
        new UnwritableVectorIJK(0.9376657339577930, 0.3444503894864143, 0.0462266325951095));

    for (String sclkTimeIn : sclkTimes) {

      System.out.printf("Requested SCLK time : %s\n", sclkTimeIn);

      // Create an SCLK from the input string
      SCLK sclk = SPICEUtils.fromString(sclkTimeIn);
      double sclkdp = sclkKernel.convertToEncodedSclk(sclk);

      // TODO: implement equivalent of cspice_sctiks

      // find the closest covered SCLK time.  Note this ignores the tolerance used in the NAIF
      // example
      if (!sclkdpWindows.contains(sclkdp)) {
        Double next = sclkdpBegins.ceiling(sclkdp);
        Double prev = sclkdpEnds.floor(sclkdp);
        assertNotNull(next);
        assertNotNull(prev);
        if (sclkdp - prev > next - sclkdp) {
          sclkdp = Math.ceil(next);
        } else {
          sclkdp = Math.floor(prev);
        }
        //                 System.out.printf("Requested SCLK %s is outside of CK coverage. ", sclk);
        sclk = sclkKernel.convertToSclk(sclkdp, new SCLK());
        //                 System.out.printf("Nearest covered SCLK is %s\n", sclk);
      }

      double tdb = sclkKernel.convertFromSclkToTDB(sclk);
      SCLK sclkch = sclkKernel.convertFromTDBToSclk(tdb, new SCLK());

      System.out.printf("   CASSINI SCLK time: %s\n", sclkch.toString());

      StateTransform st = stateTransformFunc.getStateTransform(tdb);
      VectorIJK boreJ2000 = st.getRotation().mtxv(BORE);

      /*-
       * From ckgpav_c documentation:<br>
       * This is the axis about which the reference frame tied to the instrument is rotating in the
       * right-handed sense at time 'clkout'. The magnitude of 'av' is the magnitude of the
       * instantaneous velocity of the rotation, in radians per second. The components of 'av' are
       * given relative to the reference frame specified by the input argument 'ref'.
       */
      VectorIJK av = DifferentiatedRotations.getAngularVelocityInFromFrame(st);

      UnwritableVectorIJK expected = expectedMap.get(sclkTimeIn);
      assertEquals(0, expected.getSeparation(boreJ2000), 1e-15);

      System.out.printf(
          "   CASSINI ISS boresight  : %11.7f %11.7f %11.7f\n",
          boreJ2000.getI(), boreJ2000.getJ(), boreJ2000.getK());
      System.out.printf(
          "   Angular velocity vector: %11.7f %11.7f %11.7f\n\n", av.getI(), av.getJ(), av.getK());
    }
  }
}
