package picante.demo;

import static org.junit.Assert.*;
import static org.junit.Assume.assumeNoException;

import com.google.common.collect.ImmutableList;
import picante.math.coords.CoordConverters;
import picante.math.coords.LatitudinalVector;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.*;
import picante.mechanics.providers.aberrated.AberratedEphemerisProvider;
import picante.mechanics.providers.aberrated.AberrationCorrection;
import picante.mechanics.utilities.SimpleEphemerisID;
import picante.spice.SpiceEnvironment;
import picante.spice.fov.FOV;
import picante.spice.fov.FOVFactory;
import picante.spice.fov.FOVSpice;
import picante.surfaces.Ellipsoid;
import picante.surfaces.Surfaces;
import picante.utilities.InitSpice;
import picante.utilities.SPICEUtils;
import picante.time.TimeConversion;
import java.util.*;
import org.junit.Test;

/**
 * Find the surface intercept of a ray with a target body. Analogous to CSPICE routine SINCPT.
 *
 * <p>Based on demo for <a href=
 * "https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/IDL/icy/cspice_sincpt.html">CSPICE_SINCPT</a>
 *
 * <p>The intercepts do not agree exactly with SPICE as the ellipsoid intersect calculations are
 * done differently.
 *
 * @author Hari.Nair@jhuapl.edu
 */
public class TestSINCPT {
  @Test
  public void test01() {

    // There is no AberrationCorrection.CN_S enum, but this functionality is
    // achieved by using AberrationCorrection.LT_S with the triple aberrated
    // provider
    String abcorr = "CN+S";
    String camera = "MGS_MOC_NA";
    FrameID fixref = CelestialFrames.IAU_MARS;
    EphemerisID observer = new SimpleEphemerisID("MGS");
    EphemerisID target = CelestialBodies.MARS;
    String utc = "2003 OCT 13 06:00:00";

    List<String> kernels = new ArrayList<>();
    kernels.add("spk/de430.bsp");
    kernels.add("spk/mar097.bsp");
    kernels.add("pck/pck00010.tpc");
    kernels.add("lsk/naif0011.tls");
    kernels.add("ik/mgs_moc_v20.ti");
    kernels.add("sclk/mgs_sclkscet_00061.tsc");
    kernels.add("ck/mgs_sc_ext12.bc");
    kernels.add("spk/mgs_ext12_ipng_mgs95j.bsp");

    // need to bind the name "MGS" to its ephemeris ID
    Map<String, EphemerisID> bindings = new HashMap<>();
    bindings.put("MGS", observer);
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

    AberratedEphemerisProvider provider =
        InitSpice.getAberratedProvider(env, abcorr.equalsIgnoreCase("CN+S"));

    double et = tc.utcStringToTDB(utc);

    PositionVectorFunction targetToSc =
        provider.createAberratedPositionVectorFunction(
            observer, target, fixref, Coverage.ALL_TIME, AberrationCorrection.LT_S);
    UnwritableVectorIJK scPosBodyFixed = targetToSc.getPosition(et);

    ImmutableList<Double> radii = env.getBodyRadii().get(target);
    Ellipsoid shape = Surfaces.createEllipsoidalSurface(radii.get(0), radii.get(1), radii.get(2));

    Map<String, Integer> bodn2c = SPICEUtils.BODN2C(env);
    assertTrue(bodn2c.containsKey(camera));

    int instrID = bodn2c.get(camera);

    FOVFactory fovFactory = new FOVFactory(env.getPool());
    FOV fov = fovFactory.create(instrID);
    FOVSpice fovSpice = fov.getFovSpice();

    FrameID instrFrame = fovSpice.getFrame();
    FrameTransformFunction instrToBody =
        provider.createFrameTransformFunction(instrFrame, fixref, Coverage.ALL_TIME);

    System.out.println("Surface Intercept Locations for Camera");
    System.out.println("FOV Boundary and Boresight Vectors");
    System.out.println();
    System.out.printf("   Instrument:             %s\n", camera);
    System.out.printf("   Epoch:                  %s\n", utc);
    System.out.printf("   Aberration correction:  %s\n", abcorr);
    System.out.println();

    Map<String, UnwritableVectorIJK> vectors = new LinkedHashMap<>();
    for (int i = 0; i < fovSpice.getBounds().size(); i++)
      vectors.put("Corner vector " + i, fovSpice.getBounds().get(i));
    vectors.put("Boresight vector", fovSpice.getBoresight());

    Map<String, UnwritableVectorIJK> expectedMap = new HashMap<>();
    expectedMap.put(
        "Corner vector 0",
        new UnwritableVectorIJK(-1237.6589684716, -1871.7383051297, -2534.2893456425));
    expectedMap.put(
        "Corner vector 1",
        new UnwritableVectorIJK(-1235.0977575499, -1873.2081021556, -2534.4509658501));
    expectedMap.put(
        "Corner vector 2",
        new UnwritableVectorIJK(-1235.0972365336, -1873.2070838553, -2534.4519605630));
    expectedMap.put(
        "Corner vector 3",
        new UnwritableVectorIJK(-1237.6584444232, -1871.7372885440, -2534.2903405676));
    expectedMap.put(
        "Boresight vector",
        new UnwritableVectorIJK(-1236.3781693983, -1872.4728107424, -2534.3709623027));

    for (String key : vectors.keySet()) {

      UnwritableVectorIJK vector = vectors.get(key);
      UnwritableVectorIJK vectorBodyFixed = instrToBody.getTransform(et).mxv(vector);

      assertTrue(shape.intersects(scPosBodyFixed, vectorBodyFixed));

      VectorIJK sincpt = shape.compute(scPosBodyFixed, vectorBodyFixed, new VectorIJK());
      assertEquals(expectedMap.get(key).getLength(), sincpt.getLength(), 1e-4);
      assertEquals(0., sincpt.getSeparation(expectedMap.get(key)), 1e-5);

      VectorIJK pointToSc = VectorIJK.subtract(scPosBodyFixed, sincpt);

      // planetocentric coordinates
      LatitudinalVector lv = CoordConverters.convertToLatitudinal(sincpt);

      System.out.println(key);
      System.out.println();
      System.out.printf("Vector in %s frame\n", camera);
      System.out.printf("%18.10e, %18.10e, %18.10e\n", vector.getI(), vector.getJ(), vector.getK());
      System.out.println();

      System.out.println("Intercept:");
      System.out.println();
      System.out.printf("     Radius                   (km)   = %19.10f\n", lv.getRadius());
      System.out.printf(
          "     Planetocentric Latitude  (deg)  = %19.10f\n", Math.toDegrees(lv.getLatitude()));
      System.out.printf(
          "     Planetocentric Longitude (deg)  = %19.10f\n", Math.toDegrees(lv.getLongitude()));
      System.out.printf(
          "     Range                    (km)   = %19.10f\n\n", pointToSc.getLength());
    }
  }
}
