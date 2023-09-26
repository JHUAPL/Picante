package picante.spice.daf.content.recordtables;

import static org.junit.Assert.assertEquals;
import org.junit.Before;
import org.junit.Test;
import picante.spice.daf.DAFSegment;

public class DAFTimeListTableTest {

  private DAFSegment segment;
  private DAFTimeListTable tableAtZero;
  private DAFTimeListTable tableAt100;

  @Before
  public void setUp() throws Exception {
    segment = ArrayDAFSegment.createSequentialSegment(-100, 100);
    tableAtZero = new DAFTimeListTable(segment, 100, 0);
    tableAt100 = new DAFTimeListTable(segment, 100, 100);
  }

  /**
   * Indirectly checks that the record length is passed along to the abstract parent class properly.
   */
  @Test(expected = IllegalArgumentException.class)
  public void testDAFTimeListTableDAFLengthEdgeException() {
    new DAFTimeListTable(segment, 200, 2);
  }

  @Test
  public void testObtainTime() {
    assertEquals(-100, tableAtZero.getGauge(0), 0.0);
    assertEquals(-1, tableAtZero.getGauge(99), 0.0);
    assertEquals(0, tableAt100.getGauge(0), 0.0);
    assertEquals(99, tableAt100.getGauge(99), 0.0);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testObtainTimeNegativeIndexException() {
    tableAtZero.getGauge(-1);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testObtainTimeIndexTooLargeException() {
    tableAtZero.getGauge(100);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testObtainTimeNegativeIndexException100() {
    tableAt100.getGauge(-1);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testObtainTimeIndexTooLargeException100() {
    tableAt100.getGauge(100);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testObtainRecordIntObject() {
    tableAtZero.obtainRecord(1, new Object());
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testObtainRecordIntObject100() {
    tableAt100.obtainRecord(1, new Object());
  }

}
