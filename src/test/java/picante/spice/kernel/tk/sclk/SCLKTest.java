package picante.spice.kernel.tk.sclk;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import org.junit.Before;
import org.junit.Test;

public class SCLKTest {

  private SCLK shortSCLK;
  private SCLK longSCLK;
  private SCLK sclk;
  private long[] fields;

  @Before
  public void setUp() throws Exception {
    shortSCLK = new SCLK(2, 1, 2, 3, 4);
    longSCLK = new SCLK(2, 1, 2, 3, 4, 5, 6, 7, 8);
    longSCLK.set(2, 1, 2, 3, 4);
    sclk = new SCLK(1, 20);
  }

  @Test
  public void testSCLKSCLK() {
    SCLK a = new SCLK(1, 10);
    SCLK b = new SCLK(a);

    assertNotSame(a, b);
    assertEquals(a, b);
    assertEquals(b, a);
  }

  @Test
  public void testSCLK() {
    assertEquals(new SCLK(), new SCLK(1, 0));
    assertEquals(new SCLK(1, 0), new SCLK());
  }

  @Test
  public void testSCLKIntIntArray() {
    assertEquals(2, shortSCLK.getPartition());
    fields = new long[4];
    shortSCLK.getFields(4, fields);
    assertArrayEquals(new long[] {1, 2, 3, 4}, fields);
  }

  @Test
  public void testGetPartition() {
    assertEquals(2, shortSCLK.getPartition());
    assertEquals(2, longSCLK.getPartition());
    assertEquals(1, sclk.getPartition());
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetFieldIndexOutOfBoundsExceptionFromContraction() {
    longSCLK.getField(5);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetFieldIndexOutOfBoundsExceptionBadIndex() {
    longSCLK.getField(30);
  }

  @Test
  public void testGetField() {
    fields = new long[] {1, 2, 3, 4};
    for (int i = 0; i < fields.length; i++) {
      assertEquals(fields[i], shortSCLK.getField(i));
      assertEquals(fields[i], longSCLK.getField(i));
    }
    assertEquals(20, sclk.getField(0));
  }

  @Test
  public void testGetNumberOfFields() {
    assertEquals(4, shortSCLK.getNumberOfFields());
    assertEquals(4, longSCLK.getNumberOfFields());
    assertEquals(1, sclk.getNumberOfFields());
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetFieldsIntIntArrayIndexOutOfBoundsException() {
    shortSCLK.getFields(3, new long[2]);
  }

  @Test
  public void testGetFieldsIntIntArrayPilotErrorOK() {
    fields = new long[] {0, 1, 2, 3, 4};
    /*
     * This is likely a problem with the caller's code, but the SCLK class should fulfill the
     * request without error.
     */
    long[] expected = new long[] {1, 2, 3, 4, 4};
    long[] result = shortSCLK.getFields(12, fields);

    assertSame(result, fields);
    assertArrayEquals(expected, fields);
  }

  @Test
  public void testGetFieldsIntIntArray() {
    fields = new long[] {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    long[] expected = new long[] {1, 2, 3, 4, 4, 5, 6, 7, 8, 9};
    long[] result = shortSCLK.getFields(4, fields);
    assertSame(result, fields);
    assertArrayEquals(expected, result);

    fields = new long[] {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    expected = new long[] {1, 2, 3, 4, 4, 5, 6, 7, 8, 9};
    result = longSCLK.getFields(4, fields);
    assertSame(result, fields);
    assertArrayEquals(expected, result);

    fields = new long[] {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    expected = new long[] {20, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    result = sclk.getFields(1, fields);
    assertSame(result, fields);
    assertArrayEquals(expected, result);
  }

  @Test
  public void testGetFieldsIntIntArraySubset() {
    fields = new long[] {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    long[] expected = new long[] {1, 2, 2, 3, 4, 5, 6, 7, 8, 9};
    long[] result = shortSCLK.getFields(2, fields);
    assertSame(result, fields);
    assertArrayEquals(expected, result);

    fields = new long[] {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    expected = new long[] {1, 2, 2, 3, 4, 5, 6, 7, 8, 9};
    result = longSCLK.getFields(2, fields);
    assertSame(result, fields);
    assertArrayEquals(expected, result);

  }

  @Test
  public void testSetPartition() {
    shortSCLK.setPartition(20);
    assertEquals(20, shortSCLK.getPartition());
    longSCLK.setPartition(21);
    assertEquals(21, longSCLK.getPartition());
    sclk.setPartition(38);
    assertEquals(38, sclk.getPartition());
  }

  @Test
  public void testSetFieldsContraction() {
    shortSCLK.setFields(10, 11, 12);
    assertEquals(3, shortSCLK.getNumberOfFields());
    assertArrayEquals(new long[] {10, 11, 12, 0}, shortSCLK.getFields(4, new long[4]));
  }

  @Test
  public void testSetFields() {
    shortSCLK.setFields(10, 11, 12, 13);
    assertEquals(4, shortSCLK.getNumberOfFields());
    assertArrayEquals(new long[] {10, 11, 12, 13}, shortSCLK.getFields(4, new long[4]));
  }

  @Test
  public void testSetFieldsExpansion() {
    shortSCLK.setFields(10, 11, 12, 13, 14);
    assertEquals(5, shortSCLK.getNumberOfFields());
    assertArrayEquals(new long[] {10, 11, 12, 13, 14}, shortSCLK.getFields(5, new long[5]));
  }

  @Test
  public void testSetContraction() {
    longSCLK.set(5, 20, 30, 40, 50, 60, 70, 80);
    assertEquals(7, longSCLK.getNumberOfFields());
    assertArrayEquals(new long[] {20, 30, 40, 50, 60, 70, 80}, longSCLK.getFields(7, new long[7]));
  }

  @Test
  public void testSet() {
    longSCLK.set(4, 21, 31, 41, 51);
    assertEquals(4, longSCLK.getNumberOfFields());
    assertArrayEquals(new long[] {21, 31, 41, 51}, longSCLK.getFields(4, new long[4]));
  }

  @Test
  public void testSetExpansion() {
    longSCLK.set(5, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000);
    assertEquals(10, longSCLK.getNumberOfFields());
    assertArrayEquals(new long[] {100, 200, 300, 400, 500, 600, 700, 800, 900, 1000},
        longSCLK.getFields(10, new long[10]));
  }

  @Test
  public void testToString() {
    assertEquals("2/1:2:3:4", shortSCLK.toString());
    assertEquals("2/1:2:3:4", longSCLK.toString());
    assertEquals("1/20", sclk.toString());
    assertEquals("1/0", new SCLK().toString());
  }

  @Test
  public void testHashCode() {
    assertEquals(shortSCLK.hashCode(), longSCLK.hashCode());
  }

  @Test
  public void testEquals() {
    assertTrue(longSCLK.equals(shortSCLK));
    assertTrue(shortSCLK.equals(longSCLK));
    assertFalse(longSCLK.equals(sclk));
    assertFalse(shortSCLK.equals(sclk));
    assertFalse(sclk.equals(longSCLK));
    assertFalse(sclk.equals(shortSCLK));
  }

  @Test
  public void testSetToSCLK() {
    SCLK a = new SCLK(1, 20, 40);
    SCLK b = new SCLK(5, 1);

    assertFalse(a.equals(b));

    b.setTo(a);

    assertTrue(a.equals(b));
    assertTrue(b.equals(a));
  }

}
