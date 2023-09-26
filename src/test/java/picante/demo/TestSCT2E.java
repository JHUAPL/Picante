package picante.demo;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import com.google.common.collect.ImmutableMap;
import picante.spice.SpiceEnvironment;
import picante.spice.kernel.tk.sclk.SCLKKernel;
import picante.utilities.InitSpice;
import picante.time.TimeConversion;
import java.util.ArrayList;
import java.util.List;
import org.junit.Test;

/**
 * Convert an encoded SCLK to ET seconds past J2000. Analogous to SPICE routine SCT2E.
 *
 * <p>Based on demo for <a href=
 * "https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/IDL/icy/cspice_sct2e.html">CSPICE_SCT2E</a>
 *
 * @author Hari.Nair@jhuapl.edu
 */
public class TestSCT2E {
  @Test
  public void test01() {
    List<String> kernels = new ArrayList<>();
    kernels.add("lsk/naif0012.tls");
    kernels.add("sclk/cas00071.tsc");

    SpiceEnvironment env = InitSpice.getSpiceEnvironment(kernels);
    TimeConversion tc = new TimeConversion(env.getLSK());

    int sc = -82;
    ImmutableMap<Integer, SCLKKernel> sclkKernels = env.getSclkKernels();
    SCLKKernel sclkKernel = sclkKernels.get(sc);
    assertNotNull(sclkKernel);

    double sclkdp = 197483593295.0d;
    double et = sclkKernel.convertToTDB(sclkdp);
    String utc = tc.tdbToUTCString(et, "C");

    assertEquals(140223701.758428, et, 1e-6);
    System.out.printf("%-16s  %-16s  %-24s\n", "Encoded SCLK", "ET", "UTC");
    System.out.printf("%16.3f  %16.6f  %s\n", sclkdp, et, utc);
  }
}
