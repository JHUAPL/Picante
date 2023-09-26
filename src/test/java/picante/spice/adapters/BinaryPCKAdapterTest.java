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
import picante.spice.kernel.pck.PCKSegment;

public class BinaryPCKAdapterTest {

  private BinaryPCKAdapter binaryPCKAdapter;

  private PCKMetaData metaData;
  private List<BinaryPCKSegmentAdapter> data = Lists.newArrayList();

  private FrameID fromID;
  private FrameID toID;
  private PCKSegment pcks;

  @Before
  public void setUp() throws Exception {
    metaData =
        new PCKMetaData("name", "internalName", Arrays.asList(new String[] {"my", "comments"}));

    fromID = createMock(FrameID.class);
    toID = createMock(FrameID.class);
    pcks = createMock(PCKSegment.class);

    data.add(new BinaryPCKSegmentAdapter(fromID, toID, pcks));
    data.add(new BinaryPCKSegmentAdapter(fromID, toID, pcks));
    data.add(new BinaryPCKSegmentAdapter(fromID, toID, pcks));

    binaryPCKAdapter = new BinaryPCKAdapter(data, metaData);
  }

  @Test
  public void testBinaryPCKAdapter() {
    try {
      assertEquals(data, binaryPCKAdapter.getData());
      assertEquals(data.size(), binaryPCKAdapter.getData().size());
      assertEquals(metaData.getComments(), binaryPCKAdapter.getMetaData().getComments());
      assertEquals(metaData.getName(), binaryPCKAdapter.getMetaData().getName());
    } catch (FrameSourceIOException e) {
      throw new RuntimeException(e);
    }
  }

  @Test
  public void testGetData() {
    try {
      assertEquals(data, binaryPCKAdapter.getData());
      assertEquals(data.size(), binaryPCKAdapter.getData().size());
    } catch (FrameSourceIOException e) {
      throw new RuntimeException(e);
    }
  }

  @Test
  public void testGetMetaData() {
    try {
      assertEquals(metaData.getComments(), binaryPCKAdapter.getMetaData().getComments());
      assertEquals(metaData.getName(), binaryPCKAdapter.getMetaData().getName());
    } catch (FrameSourceIOException e) {
      throw new RuntimeException(e);
    }
  }

}
