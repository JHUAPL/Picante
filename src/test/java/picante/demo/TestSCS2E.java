package picante.demo;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import com.google.common.collect.ImmutableMap;
import picante.spice.SpiceEnvironment;
import picante.spice.kernel.tk.sclk.SCLK;
import picante.spice.kernel.tk.sclk.SCLKKernel;
import picante.utilities.InitSpice;
import picante.utilities.SPICEUtils;
import picante.time.TimeConversion;
import java.util.*;
import org.junit.Test;

/**
 * Convert a spacecraft clock string to ephemeris time. Analogous to SPICE routine SCS2E.
 *
 * <p>Based on demo for <a href=
 * "https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/IDL/icy/cspice_scs2e.html">CSPICE_SCS2E</a>
 *
 * @author Hari.Nair@jhuapl.edu
 */
public class TestSCS2E {
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

    Map<String, Double> expectedMap = new LinkedHashMap<>();
    expectedMap.put("1/1465644281.165", 140223678.094534);
    expectedMap.put("1/1465646281.165", 140225678.080283);
    expectedMap.put("1/1465647281", 140226677.428631);
    expectedMap.put("1/1465647281.001", 140226677.432538);

    System.out.printf("%-20s  %-16s  %-24s\n", "SCLK String", "ET", "UTC Time");
    for (String timeIn : expectedMap.keySet()) {
      SCLK sclk = SPICEUtils.fromString(timeIn);
      double et = sclkKernel.convertFromSclkToTDB(sclk);
      double expected = expectedMap.get(timeIn);
      assertEquals(expected, et, 1e-6);
      System.out.printf("%-20s  %16.6f  %s\n", timeIn, et, tc.tdbToUTCString(et, "C"));
    }
  }
}
