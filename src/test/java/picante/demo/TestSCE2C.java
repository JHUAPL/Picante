package picante.demo;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import com.google.common.collect.ImmutableMap;
import picante.spice.SpiceEnvironment;
import picante.spice.kernel.tk.sclk.SCLKKernel;
import picante.utilities.InitSpice;
import picante.time.TimeConversion;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.junit.Test;

/**
 * Convert ephemeris seconds to spacecraft ticks. Analogous to SPICE routine SCE2C.
 *
 * <p>Based on demo for <a href=
 * "https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/IDL/icy/cspice_sce2c.html">CSPICE_SCE2C</a>
 *
 * @author Hari.Nair@jhuapl.edu
 */
public class TestSCE2C {
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

    Map<String, Double> testMap = new LinkedHashMap<>();
    testMap.put("2004 JUN 11 11:00:37.57200", 197483593294.540863);
    testMap.put("2004 JUN 11 12:00:37.57200", 197484514901.107330);
    testMap.put("2004 JUN 11 13:00:37.57200", 197485436507.673767);
    testMap.put("2004 JUN 11 14:00:37.57200", 197486358114.240204);
    for (String utc : testMap.keySet()) {
      double et = tc.utcStringToTDB(utc);
      double sclkdp = sclkKernel.convertToEncodedSclk(et);
      assertEquals(1.0,testMap.get(utc)/sclkdp, 1e-13);
      System.out.printf("%s  %19.6f\n", utc, sclkdp);
    }
  }
}
