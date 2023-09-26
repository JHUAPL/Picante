package picante.time;

import static org.junit.Assert.assertEquals;
import java.io.IOException;
import org.junit.Test;
import com.google.common.io.Resources;
import picante.exceptions.PicanteRuntimeException;
import picante.spice.SpiceEnvironmentBuilder;
import picante.spice.kernel.KernelInstantiationException;
import picante.spice.kernel.tk.lsk.LSK;
import picante.spice.kernelpool.content.LSKFactory;

public class TimeAdapterTest {

  @Test
  public void testNAIF0008() throws KernelInstantiationException, IOException {
    UTCEpoch utc = new UTCEpoch(2016, 8, 18, 7, 0, 0);
    // this is the correct value
    double tdb = 524775668.18286586;

    TimeAdapter ta12 = TimeAdapter.getInstance();

    // do the conversion with an outdated LSK
    SpiceEnvironmentBuilder builder = new SpiceEnvironmentBuilder();
    builder.load("LSK",
        Resources.asByteSource(this.getClass().getResource("/picante/time/naif0008.tls")));
    LSK lsk = new LSKFactory().createLSK(builder.build().getPool());
    TimeAdapter ta08 = TimeAdapter.initialize(lsk);

    // the answer returned is three seconds earlier
    double tdb08 = ta08.convertToET(utc);
    double tdb12 = ta12.convertToET(utc);

    System.out.printf("%.8f, %.8f, %.8f\n", tdb, tdb12, tdb - tdb12);

    assertEquals(tdb12, tdb, 1e-6);
    assertEquals(tdb12 - tdb08, 3, 1e-6);
  }

  @Test(expected = PicanteRuntimeException.class)
  public void testDoubleInitialization() throws KernelInstantiationException, IOException {
    SpiceEnvironmentBuilder builder = new SpiceEnvironmentBuilder();
    builder.load("LSK",
        Resources.asByteSource(this.getClass().getResource("/picante/time/naif0008.tls")));
    LSK lsk = new LSKFactory().createLSK(builder.build().getPool());
    TimeAdapter ta = TimeAdapter.getInstance();
    TimeAdapter.initialize(TSManager.createStandardManager(lsk));
    System.out.println("This should never be displayed: " + ta.convertToUTC(0.).toString());
  }

  @Test
  public void testTDBToTDT() {
    double tdb = 524775668.182866;
    double tdt = 524775668.184000;

    TimeAdapter ta = TimeAdapter.getInstance();
    assertEquals(tdt, ta.convertTDBToTDT(tdb), 1e-6);
    assertEquals(tdb, ta.convertTDTToTDB(tdt), 1e-6);
  }

  @Test
  public void testTDBToTAI() {
    double tdb = 524775600.000000;
    double tai = 524775567.817134;

    TimeAdapter ta = TimeAdapter.getInstance();
    assertEquals(tai, ta.convertTDBtoTAIJ2000(tdb), 1e-6);
    assertEquals(tdb, ta.convertTAIJ2000toTDB(tai), 1e-6);
  }


}
