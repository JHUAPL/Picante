package picante.spice.kernelpool.content;

import static org.junit.Assert.assertEquals;
import java.util.ArrayList;
import java.util.LinkedList;
import org.junit.Before;
import org.junit.Test;
import picante.spice.kernel.tk.sclk.SCLKType1Record;

public class SCLKType1ListBasedRecordTableTest {

  private ArrayList<Double> coefficients;
  private SCLKType1ListBasedRecordTable table;
  private SCLKType1Record record;

  @Before
  public void setUp() throws Exception {
    coefficients = new ArrayList<Double>();
    coefficients.add(0.0);
    coefficients.add(0.0);
    coefficients.add(1.0);
    coefficients.add(1e9);
    coefficients.add(1e6);
    coefficients.add(1.1);
    coefficients.add(2e9);
    coefficients.add(2e6);
    coefficients.add(1.2);

    // coefficients = new double[] { 0.0, 0.0, 1.0, 1e9, 1e6, 1.1, 2e9, 2e6,
    // 1.2 };
    table = new SCLKType1ListBasedRecordTable(coefficients, 1000.0);
    record = new SCLKType1Record();
  }

  @Test(expected = IllegalArgumentException.class)
  public void testSclkType1ListBasedRecordTableIllegalArgumentException() {
    ArrayList<Double> list = new ArrayList<Double>();
    list.add(1.0);
    list.add(2.0);
    list.add(3.0);
    list.add(4.0);
    list.add(5.0);
    new SCLKType1ListBasedRecordTable(list, 10.0);
  }

  @Test
  public void testSclkType1ListBasedRecordTable() {

    /*
     * Verify that the record table is retaining a reference to the records. This is an
     * implementation detail, but an important one.
     */
    table.get(0, record);
    assertEquals(0.0, record.getEncodedSCLK(), 0.0);
    coefficients.set(0, 10.0);
    table.get(0, record);
    assertEquals(10.0, record.getEncodedSCLK(), 0.0);

  }

  @Test
  public void testSclkType1ListBasedRecordTableCopyCase() {
    /*
     * Verify that the record table is copying a the records when the supplied list does not
     * implement RandomAccess.
     */
    LinkedList<Double> list = new LinkedList<Double>(coefficients);
    table = new SCLKType1ListBasedRecordTable(list, 1000.0);
    table.get(0, record);
    assertEquals(0.0, record.getEncodedSCLK(), 0.0);
    list.set(0, 10.0);
    table.get(0, record);
    assertEquals(0.0, record.getEncodedSCLK(), 0.0);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetTimeHighIndexException() {
    table.getGauge(4);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetTimeLowIndexException() {
    table.getGauge(-1);
  }

  @Test
  public void testGetTime() {
    assertEquals(0.0, table.getGauge(0), 0.0);
    assertEquals(1e9, table.getGauge(1), 0.0);
    assertEquals(2e9, table.getGauge(2), 0.0);
  }

  @Test
  public void testGetLength() {
    assertEquals(3, table.size());
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetRecordLowIndexException() {
    table.get(-1, record);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetRecordHighIndexException() {
    table.get(4, record);
  }

  @Test
  public void testGetRecord() {

    table.get(0, record);
    assertEquals(0.0, record.getEncodedSCLK(), 0.0);
    assertEquals(0.0, record.getParallelTime(), 0.0);
    assertEquals(1.0, record.getRate(), 0.0);
    assertEquals(1000.0, record.getTicksPerMostSignificantCount(), 0.0);

    table.get(1, record);
    assertEquals(1e9, record.getEncodedSCLK(), 0.0);
    assertEquals(1e6, record.getParallelTime(), 0.0);
    assertEquals(1.1, record.getRate(), 0.0);
    assertEquals(1000.0, record.getTicksPerMostSignificantCount(), 0.0);

    table.get(2, record);
    assertEquals(2e9, record.getEncodedSCLK(), 0.0);
    assertEquals(2e6, record.getParallelTime(), 0.0);
    assertEquals(1.2, record.getRate(), 0.0);
    assertEquals(1000.0, record.getTicksPerMostSignificantCount(), 0.0);

  }

}
