package picante.demo;

import picante.math.vectorspace.UnwritableVectorIJK;
import picante.spice.SpiceEnvironment;
import picante.spice.fov.FOV;
import picante.spice.fov.FOVFactory;
import picante.spice.fov.FOVSpice;
import picante.utilities.InitSpice;
import java.util.ArrayList;
import java.util.List;
import org.junit.Test;

/**
 * Return the field-of-view (FOV) parameters for a specified instrument.
 *
 * <p>Based on demo code for <a href=
 * "https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/IDL/icy/cspice_getfov.html">CSPICE_GETFOV</a>
 *
 * @author Hari.Nair@jhuapl.edu
 */
public class TestGETFOV {
  @Test
  public void test01() {
    List<String> kernels = new ArrayList<>();
    kernels.add("lsk/naif0012.tls");
    kernels.add("ik/example.ti");

    SpiceEnvironment env = InitSpice.getSpiceEnvironment(kernels);

    FOVFactory factory = new FOVFactory(env.getPool());

    int[] insids = {-999001, -999002, -999003, -999004};

    System.out.println("--------------------------------------");

    for (int i : insids) {
      FOV fov = factory.create(i);
      FOVSpice fovSpice = fov.getFovSpice();
      System.out.printf("Instrument ID: %d\n", fovSpice.getInstrumentID());
      System.out.printf(String.format("    FOV shape: %s\n", fovSpice.getShape().name()));
      System.out.printf(String.format("    FOV frame: %s\n", fovSpice.getFrame().getName()));
      System.out.printf(String.format("FOV boresight: %s\n", fovSpice.getBoresight().toString()));
      System.out.println("  FOV corners:");
      for (UnwritableVectorIJK b : fovSpice.getBounds()) {
        System.out.printf("               %s\n", b.toString());
      }
      System.out.println("--------------------------------------");
    }
  }
}
