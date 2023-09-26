package picante.demo;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.junit.Test;
import com.google.common.collect.ImmutableMap;
import picante.spice.SpiceEnvironment;
import picante.spice.kernel.tk.sclk.SCLK;
import picante.spice.kernel.tk.sclk.SCLKKernel;
import picante.utilities.InitSpice;

/**
 * Encodes a character representation of spacecraft clock time into a double precision number.
 * Analogous to SPICE routine SCENCD.
 *
 * <p>Based on demo for <a href=
 * "https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/IDL/icy/cspice_scencd.html">CSPICE_SCENCD</a>
 *
 * @author Hari.Nair@jhuapl.edu
 */
public class TestSCENCD {
  @Test
  public void test01() {
    List<String> kernels = new ArrayList<>();
    kernels.add("lsk/naif0012.tls");
    kernels.add("sclk/cas00071.tsc");

    SpiceEnvironment env = InitSpice.getSpiceEnvironment(kernels);

    int sc = -82;
    ImmutableMap<Integer, SCLKKernel> sclkKernels = env.getSclkKernels();
    SCLKKernel sclkKernel = sclkKernels.get(sc);
    assertNotNull(sclkKernel);

    Map<Double, Map.Entry<String, Double>> expectedMap = new LinkedHashMap<>();
    expectedMap.put(
        197483587237., new AbstractMap.SimpleEntry<>("1/1465644281:165", 197483587237.));
    expectedMap.put(
        197483587250., new AbstractMap.SimpleEntry<>("1/1465644281:178", 197483587250.));
    expectedMap.put(
        197485901583.201, new AbstractMap.SimpleEntry<>("1/1465653322:15", 197485901583.));
    expectedMap.put(197486447183., new AbstractMap.SimpleEntry<>("1/1465655453:79", 197486447183.));
    expectedMap.put(
        198136032015.4, new AbstractMap.SimpleEntry<>("1/1468192894:15", 198136032015.));

    for (double sclkdpIn : expectedMap.keySet()) {
      SCLK sclkch = sclkKernel.convertToSclk(sclkdpIn, new SCLK()); // SCDECD
      double sclkdpOut = sclkKernel.convertToEncodedSclk(sclkch); // SCENCD

      Map.Entry<String, Double> expected = expectedMap.get(sclkdpIn);
      assertEquals(expected.getKey(), sclkch.toString());
      assertEquals(expected.getValue(), sclkdpOut, 0);

      System.out.printf("%10s%21.8f\n", "Original: ", sclkdpIn);
      System.out.printf("SCLKCH: %s\n", sclkch);
      System.out.printf("%10s%21.8f\n\n", "Decoded : ", sclkdpOut);
    }
  }
}
