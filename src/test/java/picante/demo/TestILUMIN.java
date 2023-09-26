package picante.demo;

import com.google.common.collect.ImmutableList;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.*;
import picante.mechanics.providers.aberrated.AberratedEphemerisProvider;
import picante.mechanics.providers.aberrated.AberrationCorrection;
import picante.mechanics.utilities.SimpleEphemerisID;
import picante.spice.SpiceEnvironment;
import picante.surfaces.Ellipsoid;
import picante.surfaces.Surfaces;
import picante.utilities.InitSpice;
import picante.time.TimeConversion;
import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assume.assumeNoException;

/**
 * Computes the illumination angles (phase, solar incidence, and emission) at a specified surface
 * point of a target body.
 * <p>
 * Based on the demo code for <a href=
 * "https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/IDL/icy/cspice_ilumin.html">CSPICE_ILUMIN</a>.
 * <p>
 * This does not agree exactly with SPICE as the ellipsoid intersect calculations are done
 * differently.
 *
 * @author Hari.Nair@jhuapl.edu
 *
 */
public class TestILUMIN {
    @Test
    public void test01() {
        List<String> kernels = new ArrayList<>();
        kernels.add("lsk/naif0011.tls");
        kernels.add("pck/pck00010.tpc");
        kernels.add("spk/de430.bsp");
        kernels.add("spk/mar097.bsp");
        kernels.add("spk/mgs_ext13_ipng_mgs95j.bsp");

        FrameID frameID = CelestialFrames.IAU_MARS;
        EphemerisID observer = new SimpleEphemerisID("MGS");
        EphemerisID target = CelestialBodies.MARS;
        String utc = "2004 JAN 1 12:00:00";

        Map<String, EphemerisID> bindings = new HashMap<>();
        bindings.put("MGS", observer);
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
        AberratedEphemerisProvider provider = InitSpice.getAberratedProvider(env, true);
        TimeConversion tc = new TimeConversion(env.getLSK());
        double et = tc.utcStringToTDB(utc);

        // target body shape
        ImmutableList<Double> radii = env.getBodyRadii().get(target);
        Ellipsoid shape = Surfaces.createEllipsoidalSurface(radii.get(0), radii.get(1), radii.get(2));

        // find subsolar point in body fixed coordinates using the nearest point on the ellipsoid to the
        // Sun
        PositionVectorFunction targetToSun = provider.createAberratedPositionVectorFunction(
                CelestialBodies.SUN, target, frameID, Coverage.ALL_TIME, AberrationCorrection.LT_S);
        VectorIJK sunPos = targetToSun.getPosition(et);
        UnwritableVectorIJK subSolarPt = shape.computeNearPoint(sunPos).getPoint();

        // find sub-observer point in body fixed coordinates
        PositionVectorFunction targetToObserver = provider.createAberratedPositionVectorFunction(
                observer, target, frameID, Coverage.ALL_TIME, AberrationCorrection.LT_S);
        VectorIJK obsPos = targetToObserver.getPosition(et);
        UnwritableVectorIJK subObserverPt = shape.computeNearPoint(obsPos).getPoint();

        // find the illumination angles at the subsolar point
        VectorIJK surfaceToSun = VectorIJK.subtract(sunPos, subSolarPt);
        VectorIJK surfaceToObs = VectorIJK.subtract(obsPos, subSolarPt);
        VectorIJK normal = shape.computeOutwardNormal(subSolarPt);

        double sslphs = surfaceToSun.getSeparation(surfaceToObs);
        double sslsol = surfaceToSun.getSeparation(normal);
        double sslemi = surfaceToObs.getSeparation(normal);

        // find the illumination angles at the sub-observer point
        surfaceToSun = VectorIJK.subtract(sunPos, subObserverPt);
        surfaceToObs = VectorIJK.subtract(obsPos, subObserverPt);
        normal = shape.computeOutwardNormal(subObserverPt);

        double sscphs = surfaceToSun.getSeparation(surfaceToObs);
        double sscsol = surfaceToSun.getSeparation(normal);
        double sscemi = surfaceToObs.getSeparation(normal);

        assertEquals(115.542, Math.toDegrees(sslphs), 0.01);
        assertEquals(0, sslsol, 1e-12);
        assertEquals(sslphs, sslemi, 0);

        assertEquals(62.083999, Math.toDegrees(sscphs), 0.001);
        assertEquals(0, sscemi, 1e-12);
        assertEquals(sscphs, sscsol, 1e-12);

        System.out.printf("UTC epoch is %s\n\n", utc);
        System.out.print("Illumination angles at the sub-solar point:\n\n");
        System.out.printf("Phase angle             (deg):  %f\n", Math.toDegrees(sslphs));
        System.out.printf("Solar incidence angle   (deg):  %f\n", Math.toDegrees(sslsol));
        System.out.printf("Emission angle          (deg):  %f\n\n", Math.toDegrees(sslemi));
        System.out.println("The solar incidence angle should be 0.");
        System.out.println("The emission and phase angles should be ");
        System.out.println("equal.\n\n");
        System.out.print("Illumination angles at the sub-s/c point:\n\n");
        System.out.printf("Phase angle             (deg):  %f\n", Math.toDegrees(sscphs));
        System.out.printf("Solar incidence angle   (deg):  %f\n", Math.toDegrees(sscsol));
        System.out.printf("Emission angle          (deg):  %f\n\n", Math.toDegrees(sscemi));
        System.out.println("The emission angle should be 0.");
        System.out.println("The solar incidence and phase angles ");
        System.out.println("should be equal.\n\n");
    }
}
