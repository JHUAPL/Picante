package picante.spice.kernel.pck;

import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.reset;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertEquals;
import java.util.ArrayList;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import com.google.common.collect.Lists;
import picante.spice.kernel.KernelType;
import picante.spice.kernel.SegmentedKernelContent;

public class PCKTest {

  private PCK pck;

  private SegmentedKernelContent<PCKSegment> kernelContent;

  private List<PCKSegment> segments = Lists.newArrayList();

  private int size;

  @Before
  public void setUp() throws Exception {
    size = 10;

    for (int i = 0; i < size; i++) {
      segments.add(createMock(PCKSegment.class));
    }

    kernelContent = createMock(SegmentedKernelContent.class);

    pck = new PCK("name", kernelContent);

  }

  void configureMock() {

    int size = 10;

    expect(kernelContent.getSize()).andReturn(size).anyTimes();

    expect(kernelContent.getComments()).andReturn(Lists.newArrayList("some", "comments", "here"))
        .anyTimes();
    expect(kernelContent.getInternalName()).andReturn("internalName").anyTimes();
    for (int i = 0; i < size; i++) {
      expect(kernelContent.getSegment(i)).andReturn(segments.get(i)).anyTimes();
    }

    expect(kernelContent.getSegments(new ArrayList<PCKSegment>())).andReturn(segments).anyTimes();
  }

  @Test
  public void testGetComments() {
    reset(kernelContent);
    configureMock();
    replay(kernelContent);

    assertEquals(kernelContent.getComments(), pck.getComments());

    verify(kernelContent);
  }

  @Test
  public void testGetInternalName() {
    reset(kernelContent);
    configureMock();
    replay(kernelContent);

    assertEquals(kernelContent.getInternalName(), pck.getInternalName());

    verify(kernelContent);
  }

  @Test
  public void testGetSegment() {
    for (int i = 0; i < pck.getSize(); i++) {
      assertEquals(kernelContent.getSegment(i), pck.getSegment(i));
    }
  }

  @Test
  public void testGetSegments() {
    reset(kernelContent);
    configureMock();
    replay(kernelContent);

    assertEquals(segments, pck.getSegments(new ArrayList<PCKSegment>()));
    assertEquals(segments.size(), pck.getSegments(new ArrayList<PCKSegment>()).size());

    verify(kernelContent);
  }

  @Test
  public void testGetSize() {
    reset(kernelContent);
    configureMock();
    replay(kernelContent);

    assertEquals(size, pck.getSize());

    verify(kernelContent);
  }

  @Test
  public void testGetType() {
    assertEquals(KernelType.PCK, pck.getType());
  }

  @Test
  public void testGetName() {
    assertEquals("name", pck.getName());
  }

  @Test
  public void testToString() {
    assertEquals("PCK[name]", pck.toString());
  }

}
