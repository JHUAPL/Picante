package picante.demo;

import static org.junit.Assert.*;
import picante.spice.SpiceEnvironment;
import picante.spice.kernelpool.UnwritableKernelPool;
import picante.utilities.InitSpice;
import java.util.ArrayList;
import java.util.List;
import org.junit.Test;

/**
 * Get double precision values for a body specified with an ID code.
 *
 * <p>Based on demo code for <a href=
 * "https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/IDL/icy/cspice_bodvcd.html">CSPICE_BODVCD</a>
 *
 * @author Hari.Nair@jhuapl.edu
 */
public class TestBODVCD {

  @Test
  public void test01() {

    List<String> kernels = new ArrayList<>();
    kernels.add("lsk/naif0009.tls");
    kernels.add("pck/pck00008.tpc");

    SpiceEnvironment env = InitSpice.getSpiceEnvironment(kernels);

    UnwritableKernelPool kp = env.getPool();

    // another way to get radii from the SpiceEnvironment is
    // ImmutableList<Double> radii = env.getBodyRadii().get(CelestialBodies.EARTH);

    final double[] expected = {6378.140, 6378.140, 6356.750};

    int id = 399;
    String keyword = String.format("BODY%d_RADII", id);
    assertTrue(kp.hasKeyword(keyword));
    List<Double> doubles = kp.getDoubles(keyword);
    assertEquals(expected.length, doubles.size());
    System.out.printf("%s:", keyword);
    for (int i = 0; i < doubles.size(); i++) {
      System.out.printf("\t%10.3f", doubles.get(i));
      assertEquals(expected[i], doubles.get(i), 0.);
    }
    System.out.println();

    // kernel pool variable names are case sensitive.  NAIF recommends using upper case.
    keyword = String.format("BODY%d_radii", id);
    assertFalse(kp.hasKeyword(keyword));
  }
}
