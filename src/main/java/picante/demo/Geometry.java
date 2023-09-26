package picante.demo;

import com.google.common.collect.ImmutableList;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import picante.math.coords.CoordConverters;
import picante.math.coords.LatitudinalVector;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.CelestialBodies;
import picante.mechanics.CelestialFrames;
import picante.mechanics.Coverage;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;
import picante.mechanics.FrameTransformFunction;
import picante.mechanics.PositionVectorFunction;
import picante.mechanics.providers.aberrated.AberratedEphemerisProvider;
import picante.mechanics.providers.aberrated.AberrationCorrection;
import picante.mechanics.utilities.SimpleEphemerisID;
import picante.spice.MetakernelReader;
import picante.spice.SpiceEnvironment;
import picante.spice.SpiceEnvironmentBuilder;
import picante.spice.adapters.AdapterInstantiationException;
import picante.spice.fov.FOV;
import picante.spice.fov.FOVFactory;
import picante.spice.kernel.KernelInstantiationException;
import picante.surfaces.Ellipsoid;
import picante.surfaces.Surfaces;
import picante.time.TimeConversion;
import picante.utilities.SPICEUtils;

/**
 * Picante version of SPICE example in the NAIF tutorials (e.g. <a href=
 * "https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/Tutorials/pdf/individual_docs/38_program_idl.pdf">IDL</a>).
 * See slide 23 for results to compare.
 *
 * <p>This does not agree exactly with SPICE as the ellipsoid intersect calculations are done
 * differently.
 *
 * @author Hari.Nair@jhuapl.edu
 */
public class Geometry {

  public static void main(String[] args) {

    EphemerisID bodyID = CelestialBodies.PHOEBE;
    FrameID frameID = CelestialFrames.IAU_PHOEBE;
    EphemerisID spacecraftID = new SimpleEphemerisID("CASSINI");
    String instName = "CASSINI_ISS_NAC";
    String utcString = "2004 jun 11 19:32:00";

    // this is necessary to tell the SpiceEnvironment about objects that are not in the
    // CelestialBodies enumeration
    Map<String, EphemerisID> bindings = new HashMap<>();
    bindings.put("CASSINI", spacecraftID);

    // The MetakernelReader creates the list of files to be read - you may need to run the scripts
    // src/test/resources/kernels/spk/getSPK.bash
    // src/test/resources/kernels/ck/getCK.bash
    // to download all the necessary kernels
    MetakernelReader reader = new MetakernelReader("src/test/resources/kernels/mk/geometry.tm");

    // build the SpiceEnvironment from the list of kernels and object bindings
    SpiceEnvironment env = null;
    try {
      SpiceEnvironmentBuilder builder = new SpiceEnvironmentBuilder();
      for (String name : bindings.keySet()) builder.bindEphemerisID(name, bindings.get(name));
      for (File kernel : reader.getKernelsToLoad()) builder.load(kernel.getName(), kernel);
      env = builder.build();
    } catch (KernelInstantiationException | IOException e) {
      e.printStackTrace();
      System.exit(1);
    }

    // convert the utcString to a TDB time
    TimeConversion tc = new TimeConversion(env.getLSK());
    double tdb = tc.utcStringToTDB(utcString);

    // create an ephemeris and frame provider from the SpiceEnvironment
    AberratedEphemerisProvider provider = null;
    try {
      provider = env.createTripleAberratedProvider();
    } catch (AdapterInstantiationException e) {
      e.printStackTrace();
      System.exit(1);
    }

    // this function evaluates to the vector from the target (Phoebe) to the spacecraft (CASSINI)
    // in Phoebe's body fixed frame
    PositionVectorFunction targetToSc =
        provider.createAberratedPositionVectorFunction(
            spacecraftID, bodyID, frameID, Coverage.ALL_TIME, AberrationCorrection.LT_S);

    // This function evaluates to the vector from the target (Phoebe) to the Sun
    // in Phoebe's body fixed frame
    PositionVectorFunction targetToSun =
        provider.createAberratedPositionVectorFunction(
            CelestialBodies.SUN, bodyID, frameID, Coverage.ALL_TIME, AberrationCorrection.LT_S);

    // Get Phoebe's shape from the kernel pool
    ImmutableList<Double> radii = env.getBodyRadii().get(bodyID);
    Ellipsoid shape = Surfaces.createEllipsoidalSurface(radii.get(0), radii.get(1), radii.get(2));

    // This is the vector from Phoebe's center to CASSINI
    UnwritableVectorIJK scPosBodyFixed = targetToSc.getPosition(tdb);

    // check that the instrument name is in the kernel pool
    Map<String, Integer> bodn2c = SPICEUtils.BODN2C(env);
    if (!bodn2c.containsKey(instName))
      throw new RuntimeException("No body with name " + instName + " found in kernel pool.");

    int instID = bodn2c.get(instName);

    // build the FOV object for the camera
    FOVFactory fovFactory = new FOVFactory(env.getPool());
    FOV fov = fovFactory.create(instID);

    // this is the camera boresight in the instrument frame
    UnwritableVectorIJK boresight = fov.getFovSpice().getBoresight();

    // transform a vector in the instrument frame to Phoebe's body fixed frame
    FrameID instrFrame = fov.getFovSpice().getFrame();
    FrameTransformFunction instrToBody =
        provider.createFrameTransformFunction(instrFrame, frameID, Coverage.ALL_TIME);

    // This is the boresight in Phoebe's body fixed frame
    UnwritableVectorIJK boresightBodyFixed = instrToBody.getTransform(tdb).mxv(boresight);

    // find the intersection point.  The shape, spacecraft position, and look direction are all in IAU_PHOEBE
    if (!shape.intersects(scPosBodyFixed, boresightBodyFixed)) {
      System.out.println("No intersection found.");
      System.exit(0);
    }
    VectorIJK sincpt = shape.compute(scPosBodyFixed, boresightBodyFixed, new VectorIJK());

    // find the illumination angles
    VectorIJK normal = shape.computeOutwardNormal(sincpt);
    VectorIJK pointToSc = VectorIJK.subtract(scPosBodyFixed, sincpt);

    UnwritableVectorIJK sunPosBodyFixed = targetToSun.getPosition(tdb);
    VectorIJK pointToSun = VectorIJK.subtract(sunPosBodyFixed, sincpt);

    double phase = pointToSun.getSeparation(pointToSc);
    double incidence = pointToSun.getSeparation(normal);
    double emission = pointToSc.getSeparation(normal);

    // planetocentric coordinates
    LatitudinalVector pc = CoordConverters.convertToLatitudinal(sincpt);

    // planetographic coordinates
    LatitudinalVector pg = SPICEUtils.RECGEO(shape, sincpt);

    System.out.printf(
        "Date                                    (UTC): %s\n", tc.tdbToUTCString(tdb, "ISOC"));
    System.out.printf(
        "Intercept planetocentric longitude      (deg): %f\n", Math.toDegrees(pc.getLongitude()));
    System.out.printf(
        "Intercept planetocentric latitude       (deg): %f\n", Math.toDegrees(pc.getLatitude()));
    System.out.printf(
        "Intercept planetodetic longitude        (deg): %f\n", Math.toDegrees(pg.getLongitude()));
    System.out.printf(
        "Intercept planetodetic latitude         (deg): %f\n", Math.toDegrees(pg.getLatitude()));
    System.out.printf("Range from spacecraft to intercept point (km): %f\n", pointToSc.getLength());
    System.out.printf("Intercept phase angle                   (deg): %f\n", Math.toDegrees(phase));
    System.out.printf(
        "Intercept solar incidence angle         (deg): %f\n", Math.toDegrees(incidence));
    System.out.printf(
        "Intercept emission angle                (deg): %f\n", Math.toDegrees(emission));
  }
}
