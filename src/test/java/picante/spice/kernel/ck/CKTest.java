package picante.spice.kernel.ck;

import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.reset;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertEquals;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import picante.spice.kernel.SegmentedKernelContent;

// TODO mockSKC.getSegments(buffer), the return is tested, but the buffer function is not
public class CKTest {

  private final String NAME;
  private final List<String> COMMENTS;
  private final String INTERNAL_NAME;

  private final int NUM_SEGMENTS;

  private List<CKSegment> segments;

  private SegmentedKernelContent<CKSegment> mockSKC;

  private CK ck;


  /**
   * Default constructor that brings up the test class with the default options with regards to
   * simulated parameters and measurements.
   */
  public CKTest() {
    this("name", Arrays.asList(
        new String[] {"here is a list of sample comments", "some more comments", "and some more"}),
        "internalName", 10);
  }

  /**
   * 
   * 
   * @param comments
   * @param internalName
   * @param numSegments
   */
  private CKTest(String name, List<String> comments, String internalName, int numSegments) {
    this.NAME = name;
    this.COMMENTS = comments;
    this.INTERNAL_NAME = internalName;
    this.NUM_SEGMENTS = numSegments;
  }


  @Before
  public void setUp() throws Exception {
    mockSKC = createMock(SegmentedKernelContent.class);
    segments = new ArrayList<CKSegment>();
    for (int i = 0; i < NUM_SEGMENTS; i++) {
      segments.add(createMock(CKSegment.class));
    }
    configureMock();
    replay(mockSKC);
    ck = new CK("name", mockSKC);
    verify(mockSKC);
    reset(mockSKC);
  }

  private void configureMock() {
    expect(mockSKC.getComments()).andReturn(COMMENTS).anyTimes();
    expect(mockSKC.getInternalName()).andReturn(INTERNAL_NAME).anyTimes();

    expect(mockSKC.getSize()).andReturn(NUM_SEGMENTS).anyTimes();
    for (int i = 0; i < segments.size(); i++) {
      expect(mockSKC.getSegment(i)).andReturn(segments.get(i)).anyTimes();
    }

    List<CKSegment> buffer = new ArrayList<CKSegment>();

    expect(mockSKC.getSegments(buffer)).andReturn(segments).anyTimes();
  }

  @Test
  public void testGetName() {
    assertEquals(NAME, ck.getName());
  }

  @Test
  public void testGetComments() {
    configureMock();
    replay(mockSKC);
    assertEquals(COMMENTS, ck.getComments());
    verify(mockSKC);
  }

  @Test
  public void testCK() {
    configureMock();
    replay(mockSKC);
    assertEquals(NAME, ck.getName());
    assertEquals(COMMENTS, ck.getComments());
    assertEquals(INTERNAL_NAME, ck.getInternalName());
    assertEquals(NUM_SEGMENTS, ck.getSize());

    for (int i = 0; i < NUM_SEGMENTS; i++) {
      assertEquals(segments.get(i), mockSKC.getSegment(i));
    }

    List<CKSegment> buffer = new ArrayList<CKSegment>();

    buffer = mockSKC.getSegments(buffer);

    for (int i = 0; i < NUM_SEGMENTS; i++) {
      assertEquals(segments.get(i), buffer.get(i));
    }

    verify(mockSKC);
  }

  @Test
  public void testGetInternalName() {
    configureMock();
    replay(mockSKC);
    assertEquals(INTERNAL_NAME, ck.getInternalName());
    verify(mockSKC);
  }

  @Test
  public void testGetSegment() {
    configureMock();
    replay(mockSKC);

    for (int i = 0; i < NUM_SEGMENTS; i++) {
      assertEquals(segments.get(i), mockSKC.getSegment(i));
    }

    verify(mockSKC);
  }

  @Test
  public void testGetSegments() {
    configureMock();
    replay(mockSKC);

    List<CKSegment> buffer = new ArrayList<CKSegment>();

    buffer = mockSKC.getSegments(buffer);

    for (int i = 0; i < NUM_SEGMENTS; i++) {
      assertEquals(segments.get(i), buffer.get(i));
    }

    verify(mockSKC);
  }

  @Test
  public void testGetSize() {
    configureMock();
    replay(mockSKC);
    assertEquals(NUM_SEGMENTS, ck.getSize());
    verify(mockSKC);
  }


}
