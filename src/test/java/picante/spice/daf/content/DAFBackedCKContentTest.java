package picante.spice.daf.content;

import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.createNiceMock;
import static org.easymock.EasyMock.expectLastCall;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import org.junit.Before;
import org.junit.Test;
import picante.spice.daf.DAF;
import picante.spice.daf.DAFSegment;
import picante.spice.kernel.ck.CKInstantiationException;

public class DAFBackedCKContentTest {

  private DAFBackedCKContent content;
  private DAF mockDAF;
  private DAFSegment mockSegment;
  private CKSegmentFactory mockFactory;

  @Before
  public void setUp() throws Exception {
    mockDAF = createMockDAF();
    mockSegment = createMock(DAFSegment.class);
    mockFactory = createNiceMock(CKSegmentFactory.class);
    content = new DAFBackedCKContent(mockDAF, mockFactory);
  }

  private DAF createMockDAF() {
    DAF result = createMock(DAF.class);

    result.getName();
    expectLastCall().andReturn("NAME").anyTimes();

    result.getID();
    expectLastCall().andReturn("DAF/CK").anyTimes();

    result.getReservedRecords();
    expectLastCall().andReturn(new byte[0]).anyTimes();

    result.getSize();
    expectLastCall().andReturn(0).anyTimes();

    replay(result);
    return result;
  }

  @Test(expected = CKInstantiationException.class)
  public void testVerifyContentFailure() throws Exception {
    content.verifyContent("TESTID");
  }

  @Test
  public void testVerifyContent() throws Exception {
    content.verifyContent("NAIF/DAF");
    content.verifyContent("DAF/CK");
  }

  @Test(expected = CKInstantiationException.class)
  public void testVerifySegmentBadLowNI() throws Exception {
    mockSegment.getND();
    expectLastCall().andReturn(2).anyTimes();
    mockSegment.getNI();
    expectLastCall().andReturn(1).anyTimes();
    replay(mockSegment);
    content.verifySegment(mockSegment);
    verify(mockSegment);
  }

  @Test(expected = CKInstantiationException.class)
  public void testVerifySegmentBadHighNI() throws Exception {
    mockSegment.getND();
    expectLastCall().andReturn(2).anyTimes();
    mockSegment.getNI();
    expectLastCall().andReturn(5).anyTimes();
    replay(mockSegment);
    content.verifySegment(mockSegment);
    verify(mockSegment);
  }

  @Test(expected = CKInstantiationException.class)
  public void testVerifySegmentBadLowND() throws Exception {
    mockSegment.getND();
    expectLastCall().andReturn(1).anyTimes();
    mockSegment.getNI();
    expectLastCall().andReturn(4).anyTimes();
    replay(mockSegment);
    content.verifySegment(mockSegment);
    verify(mockSegment);
  }

  @Test(expected = CKInstantiationException.class)
  public void testVerifySegmentBadHighND() throws Exception {
    mockSegment.getND();
    expectLastCall().andReturn(5).anyTimes();
    mockSegment.getNI();
    expectLastCall().andReturn(4).anyTimes();
    replay(mockSegment);
    content.verifySegment(mockSegment);
    verify(mockSegment);
  }

  @Test
  public void testVerifySegment() throws Exception {
    mockSegment.getND();
    expectLastCall().andReturn(2).anyTimes();
    mockSegment.getNI();
    expectLastCall().andReturn(4).anyTimes();
    replay(mockSegment);
    content.verifySegment(mockSegment);
    verify(mockSegment);
  }


}
