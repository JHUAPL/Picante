package picante.utilities;

import com.google.common.io.Resources;
import picante.mechanics.Coverage;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;
import picante.mechanics.providers.aberrated.AberratedEphemerisProvider;
import picante.mechanics.providers.aberrated.AberrationCorrection;
import picante.spice.SpiceEnvironment;
import picante.spice.SpiceEnvironmentBuilder;
import picante.spice.adapters.AdapterInstantiationException;
import picante.spice.kernel.KernelInstantiationException;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Convenience methods to create a SpiceEnvironment.
 *
 * @author Hari.Nair@jhuapl.edu
 */
public class InitSpice {

  static final boolean byteSource = true;
  static final String root = byteSource ? "/kernels/" : "src/test/resources/kernels/";

  public static SpiceEnvironment getSpiceEnvironment(List<String> kernels) {
    Map<String, EphemerisID> bindings = new HashMap<>();
    return getSpiceEnvironment(kernels, bindings);
  }

  /**
   * @param kernels kernels to load
   * @param bindings A map of NAIF names to bind to {@link EphemerisID}, analogous to
   *     NAIF_BODY_NAME/NAIF_BODY_CODE pairs.
   * @return SPICE environment
   */
  public static SpiceEnvironment getSpiceEnvironment(
      List<String> kernels, Map<String, EphemerisID> bindings) {
    SpiceEnvironment env = null;
    try {
      SpiceEnvironmentBuilder builder = new SpiceEnvironmentBuilder();
      for (String name : bindings.keySet()) builder.bindEphemerisID(name, bindings.get(name));
      for (String kernel : kernels) {
        if (byteSource) {
          URL resource = InitSpice.class.getResource(root + kernel);
          if (resource == null) throw new RuntimeException("Cannot load kernel " + root + kernel);
          builder.load(kernel, Resources.asByteSource(resource));
        } else builder.load(kernel, new File(root + kernel));
      }
      env = builder.build();
    } catch (KernelInstantiationException | IOException e) {
      e.printStackTrace();
      System.exit(1);
    }
    return env;
  }

  /**
   * @param env SPICE environment
   * @param cn use Converged Newtonian correction (three iterations). Default is Light Time
   *     correction (single iteration).<br>
   *     The {@link AberrationCorrection} for ephemeris calculations is passed as a parameter to the
   *     {@link AberratedEphemerisProvider#createAberratedStateVectorFunction(EphemerisID,
   *     EphemerisID, FrameID, Coverage, AberrationCorrection)} and {@link
   *     AberratedEphemerisProvider#createAberratedPositionVectorFunction(EphemerisID, EphemerisID,
   *     FrameID, Coverage, AberrationCorrection)} functions. The returned provider will evaluate
   *     light-time aberration corrections by single iteration if cn is false, and triple iteration
   *     if cn is true.
   * @return AberratedEphemerisProvider
   */
  public static AberratedEphemerisProvider getAberratedProvider(SpiceEnvironment env, boolean cn) {
    AberratedEphemerisProvider provider = null;
    try {
      provider = cn ? env.createTripleAberratedProvider() : env.createSingleAberratedProvider();
    } catch (AdapterInstantiationException e) {
      e.printStackTrace();
      System.exit(1);
    }

    return provider;
  }

}
