package picante.utilities;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import org.junit.Test;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;
import picante.mechanics.providers.aberrated.AberratedEphemerisProvider;
import picante.spice.SpiceEnvironment;
import picante.spice.adapters.SpiceClassedFrameID;

import static org.junit.Assume.assumeNoException;

public class TestInitSpice {

  @Test
  public void testKnownObjects() {
    List<String> kernels = new ArrayList<>();
    kernels.add("lsk/naif0009.tls");
    kernels.add("spk/de414.bsp");

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
    AberratedEphemerisProvider provider = InitSpice.getAberratedProvider(env, false);

    Set<EphemerisID> objects = provider.getKnownObjects(new LinkedHashSet<>());

    for (EphemerisID object : objects)
      System.out.println("Known object: " + object.getName());
  }

  @Test
  public void testKnownFrames() {
    List<String> kernels = new ArrayList<>();
    kernels.add("ck/030810_031019_c39_port1_pa.bc");
    kernels.add("sclk/cas00082.tsc");
    kernels.add("lsk/naif0009.tls");

    SpiceEnvironment env ;
    try {
      env = InitSpice.getSpiceEnvironment(kernels);
    } catch (RuntimeException e) {
      System.err.println(e.getLocalizedMessage());
      System.err.println(
              "Try running src/test/resources/kernels/ck/getCK.bash to download kernels.");
      assumeNoException(e);
      return;
    }
    AberratedEphemerisProvider provider = InitSpice.getAberratedProvider(env, false);

    Set<FrameID> frames = provider.getKnownFrames(new LinkedHashSet<>());

    for (FrameID frame : frames) {
      System.out.printf("Known frame: %s (%s)", frame.getName(), frame.getClass().getSimpleName());
      if (frame instanceof SpiceClassedFrameID)
        System.out.printf(", class %d", ((SpiceClassedFrameID) frame).getClassID());
      System.out.println();
    }
  }
}
