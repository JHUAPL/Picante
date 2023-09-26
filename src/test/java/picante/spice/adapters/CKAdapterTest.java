package picante.spice.adapters;

import static org.easymock.EasyMock.createMock;
import static org.junit.Assert.assertEquals;
import java.util.Arrays;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import com.google.common.collect.Lists;
import picante.mechanics.FrameID;
import picante.mechanics.FrameSourceIOException;
import picante.spice.kernel.ck.CKSegment;
import picante.spice.kernel.tk.sclk.EncodedSCLKConverter;

public class CKAdapterTest {

  private CKAdapter ckAdapter;

  private List<CKSegmentAdapter> adapters = Lists.newArrayList();
  private CKMetaData metaData;

  private FrameID fromID;
  private FrameID toID;
  private EncodedSCLKConverter converter;

  private CKSegment mockSegment;

  @Before
  public void setUp() throws Exception {
    metaData =
        new CKMetaData("name", "internalName", Arrays.asList(new String[] {"My", "Comments"}));
    fromID = createMock(FrameID.class);
    toID = createMock(FrameID.class);
    converter = createMock(EncodedSCLKConverter.class);
    mockSegment = createMock(CKSegment.class);
    adapters.add(new CKSegmentAdapter(fromID, toID, converter, mockSegment));

    ckAdapter = new CKAdapter(adapters, metaData);
  }

  @Test
  public void testCKAdapter() {
    try {
      assertEquals(adapters, ckAdapter.getData());
      assertEquals(adapters.size(), ckAdapter.getData().size());
      assertEquals(metaData, ckAdapter.getMetaData());
    } catch (FrameSourceIOException e) {
      throw new RuntimeException(e);
    }
  }

  @Test
  public void testGetData() {
    try {
      assertEquals(adapters, ckAdapter.getData());
      assertEquals(adapters.size(), ckAdapter.getData().size());
    } catch (FrameSourceIOException e) {
      throw new RuntimeException(e);
    }
  }

  @Test
  public void testGetMetaData() {
    try {
      assertEquals(metaData, ckAdapter.getMetaData());
    } catch (FrameSourceIOException e) {
      throw new RuntimeException(e);
    }
  }

}
