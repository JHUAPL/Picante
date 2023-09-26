package picante.demo;

import static org.junit.Assert.*;
import static org.junit.Assume.assumeNoException;

import picante.math.cones.Cone;
import picante.math.cones.Cones;
import picante.math.coords.CoordConverters;
import picante.math.coords.RaDecVector;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.mechanics.CelestialFrames;
import picante.mechanics.Coverage;
import picante.mechanics.FrameID;
import picante.mechanics.FrameTransformFunction;
import picante.mechanics.providers.aberrated.AberratedEphemerisProvider;
import picante.spice.SpiceEnvironment;
import picante.spice.fov.FOV;
import picante.spice.fov.FOVFactory;
import picante.spice.kernelpool.UnwritableKernelPool;
import picante.utilities.InitSpice;
import picante.utilities.SPICEUtils;
import picante.time.TimeConversion;
import java.util.*;
import org.junit.Test;

/**
 * Determine if a specified ray is within the field-of-view (FOV) of a specified instrument at a
 * given time.
 *
 * <p>Based on demo code for <a href=
 * "https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/IDL/icy/cspice_fovray.html">CSPICE_FOVRAY</a>
 *
 * @author Hari.Nair@jhuapl.edu
 */
public class TestFOVRAY {

  @Test
  public void test01() {
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

    UnwritableVectorIJK lookDir =
        CoordConverters.convert(
            new RaDecVector(1.0, Math.toRadians(104.656), Math.toRadians(-28.972)));

    String instName = "CASSINI_UVIS_FUV_OCC";

    List<String> utcTimes = new ArrayList<>();
    utcTimes.add("2008-054T20:31:00.000");
    utcTimes.add("2008-054T21:31:55.158");
    utcTimes.add("2008-054T21:50:00.000");
    utcTimes.add("2008-054T23:50:00.000");

    NavigableSet<Double> ephemerisTimes = new TreeSet<>();
    for (String utc : utcTimes) ephemerisTimes.add(tc.utcStringToTDB(utc));

    UnwritableKernelPool envPool = env.getPool();
    FOVFactory factory = new FOVFactory(envPool);

    Map<String, Integer> bodn2c = SPICEUtils.BODN2C(env);
    assertTrue(bodn2c.containsKey(instName));

    int instCode = bodn2c.get(instName);

    FOV fov = factory.create(instCode);
    Cone fovCone = fov.getCone();
    FrameID instrFrame = fov.getFrameID();

    FrameTransformFunction ftf =
        provider.createFrameTransformFunction(CelestialFrames.J2000, instrFrame, Coverage.ALL_TIME);

    for (Double et : ephemerisTimes) {
      RotationMatrixIJK rotate = ftf.getTransform(et);

      boolean contains = Cones.contains(fovCone, rotate.mxv(lookDir), 0.01);

      System.out.printf(
          "Epsilon CMa is%s visible from the Cassini UVIS instrument at %s\n",
          contains ? "" : " not", tc.format("YYYY-MON-DD HR:MN:SC.### ::RND").apply(et));
    }
  }
}
