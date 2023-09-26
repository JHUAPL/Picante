package picante.spice.daf.content.recordtables;

import static org.junit.Assert.assertTrue;
import java.util.Arrays;
import org.junit.Before;
import org.junit.Test;
import picante.spice.daf.DAFSegment;

public class DAFRecordRetrieverTest {

  private DAFSegment segment;

  @Before
  public void setUp() throws Exception {
    segment = ArrayDAFSegment.createSequentialSegment(-100, 100);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testAbstractDAFRecordTableEdgeDAFException() {
    new DAFRecordRetriever(segment, 2, 200, 1);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testAbstractDAFRecordTableEdgeNonTrivalRecordLengthDAFException() {
    new DAFRecordRetriever(segment, 2, 198, 2);
  }

  @Test
  public void testReadRecord() {

    /*
     * The readRecord method only guarentees that it will hand back a buffer that is at least of
     * length record size provided to the constructor. Test accordingly.
     */
    DAFRecordRetriever retriever = new DAFRecordRetriever(segment, 10, 10, 4);
    double[] record = retriever.readRecord(0);
    double[] dataRecord = new double[4];
    double[] testRecord = new double[] {-90, -89, -88, -87};
    assertTrue(record.length >= 4);
    System.arraycopy(record, 0, dataRecord, 0, 4);
    assertTrue(Arrays.equals(testRecord, dataRecord));

    record = retriever.readRecord(5);
    testRecord = new double[] {-70, -69, -68, -67};
    assertTrue(record.length >= 4);
    System.arraycopy(record, 0, dataRecord, 0, 4);
    assertTrue(Arrays.equals(testRecord, dataRecord));

    record = retriever.readRecord(9);
    testRecord = new double[] {-54, -53, -52, -51};
    assertTrue(record.length >= 4);
    System.arraycopy(record, 0, dataRecord, 0, 4);
    assertTrue(Arrays.equals(testRecord, dataRecord));
  }

}
