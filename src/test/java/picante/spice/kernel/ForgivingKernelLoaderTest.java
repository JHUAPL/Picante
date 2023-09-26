package picante.spice.kernel;

import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.junit.Assert.assertEquals;
import org.junit.Before;
import org.junit.Test;
import picante.spice.daf.DAF;
import picante.spice.daf.DAFSegment;
import picante.spice.kernel.spk.SPK;

public class ForgivingKernelLoaderTest {

  private ForgivingKernelLoader loader;

  private DAF mock;
  private DAFSegment segment;

  @Before
  public void setUp() throws Exception {
    loader = new ForgivingKernelLoader();
    mock = createMock(DAF.class);
    segment = createMock(DAFSegment.class);
  }

  @Test
  public void testCreateKernelFromDAF() throws Exception {
    expect(mock.getID()).andReturn("DAF/SPK").anyTimes();
    expect(mock.getSegment(0)).andReturn(segment);
    expect(segment.getND()).andReturn(2).anyTimes();
    expect(segment.getNI()).andReturn(4).anyTimes();
    /*
     * This should be a segment type that is unsupported.
     */
    expect(segment.getIntComponent(3)).andReturn(-Integer.MAX_VALUE);
    expect(mock.getSize()).andReturn(1).anyTimes();
    expect(mock.getName()).andReturn("MOCK").anyTimes();
    expect(mock.getReservedRecords()).andReturn(new byte[] {});
    replay(mock, segment);

    Kernel kernel = loader.createKernelFromDAF("mock", mock);

    assertEquals(KernelType.SPK, kernel.getType());
    assertEquals(0, ((SPK) kernel).getSize());
  }
}
