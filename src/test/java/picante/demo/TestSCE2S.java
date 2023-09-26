package picante.demo;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import com.google.common.collect.ImmutableMap;
import picante.spice.SpiceEnvironment;
import picante.spice.kernel.tk.sclk.SCLK;
import picante.spice.kernel.tk.sclk.SCLKKernel;
import picante.utilities.InitSpice;
import picante.time.TimeConversion;
import java.util.ArrayList;
import java.util.List;
import org.junit.Test;

/**
 * Convert an ephemeris time to an SCLK string. Analogous to SPICE routine SCE2S.
 * <p>
 * Based on demo for <a href=
 * "https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/IDL/icy/cspice_sce2s.html">CSPICE_SCE2S</a>
 *
 * @author Hari.Nair@jhuapl.edu
 *
 */
public class TestSCE2S {
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

    String event_time = "2004 JUN 11 11:00:37.57200";
    double et = tc.utcStringToTDB(event_time);

    SCLK sclkch = sclkKernel.convertFromTDBToSclk(et, new SCLK());
    assertEquals("1/1465644305:79", sclkch.toString());
    System.out.printf("%s %s\n", event_time, sclkch);
  }
}
