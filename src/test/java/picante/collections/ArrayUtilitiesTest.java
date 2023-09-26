package picante.collections;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Before;
import org.junit.Test;

public class ArrayUtilitiesTest {

  private double[] doubleEmptyList;
  private double[] doubleSingleton;
  private double[] doubleSortedList;
  private double[] doubleRepeatedList;

  private double[] zDoubleEmptyList;
  private double[] zDoubleSingleton;
  private double[] zDoubleSortedList;
  private double[] zDoubleRepeatedList;

  @Before
  public void setUp() throws Exception {

    doubleEmptyList = new double[10];
    doubleSingleton = new double[11];

    doubleSortedList = new double[10];
    doubleSortedList[5] = 0;
    doubleSortedList[6] = 10;
    doubleSortedList[7] = 20;
    doubleSortedList[8] = 30;

    doubleRepeatedList = new double[15];
    doubleRepeatedList[5] = 0;
    doubleRepeatedList[6] = 10;
    doubleRepeatedList[7] = 10;
    doubleRepeatedList[8] = 10;
    doubleRepeatedList[9] = 10;
    doubleRepeatedList[10] = 10;
    doubleRepeatedList[11] = 20;

    zDoubleEmptyList = new double[0];
    zDoubleSingleton = new double[1];

    zDoubleSortedList = new double[4];
    zDoubleSortedList[0] = 0;
    zDoubleSortedList[1] = 10;
    zDoubleSortedList[2] = 20;
    zDoubleSortedList[3] = 30;

    zDoubleRepeatedList = new double[7];
    zDoubleRepeatedList[0] = 0;
    zDoubleRepeatedList[1] = 10;
    zDoubleRepeatedList[2] = 10;
    zDoubleRepeatedList[3] = 10;
    zDoubleRepeatedList[4] = 10;
    zDoubleRepeatedList[5] = 10;
    zDoubleRepeatedList[6] = 20;
  }

  @Test
  public void testFullListDoubleLastLessThanOrEqualTo() {
    assertEquals(-1, ArrayUtilities.lastLessThanOrEqualTo(zDoubleEmptyList, 0));
    assertEquals(-1, ArrayUtilities.lastLessThanOrEqualTo(zDoubleSingleton, -10));
    assertEquals(0, ArrayUtilities.lastLessThanOrEqualTo(zDoubleSingleton, 0));
    assertEquals(0, ArrayUtilities.lastLessThanOrEqualTo(zDoubleSingleton, 10));

    assertEquals(-1, ArrayUtilities.lastLessThanOrEqualTo(zDoubleSortedList, -10));
    assertEquals(0, ArrayUtilities.lastLessThanOrEqualTo(zDoubleSortedList, 0));
    assertEquals(0, ArrayUtilities.lastLessThanOrEqualTo(zDoubleSortedList, 5));
    assertEquals(1, ArrayUtilities.lastLessThanOrEqualTo(zDoubleSortedList, 10));
    assertEquals(1, ArrayUtilities.lastLessThanOrEqualTo(zDoubleSortedList, 15));
    assertEquals(2, ArrayUtilities.lastLessThanOrEqualTo(zDoubleSortedList, 20));
    assertEquals(2, ArrayUtilities.lastLessThanOrEqualTo(zDoubleSortedList, 25));
    assertEquals(3, ArrayUtilities.lastLessThanOrEqualTo(zDoubleSortedList, 30));
    assertEquals(3, ArrayUtilities.lastLessThanOrEqualTo(zDoubleSortedList, 35));

    assertEquals(-1, ArrayUtilities.lastLessThanOrEqualTo(zDoubleRepeatedList, -10));
    assertEquals(0, ArrayUtilities.lastLessThanOrEqualTo(zDoubleRepeatedList, 0));
    assertEquals(0, ArrayUtilities.lastLessThanOrEqualTo(zDoubleRepeatedList, 5));
    assertEquals(5, ArrayUtilities.lastLessThanOrEqualTo(zDoubleRepeatedList, 10));
    assertEquals(5, ArrayUtilities.lastLessThanOrEqualTo(zDoubleRepeatedList, 15));
    assertEquals(6, ArrayUtilities.lastLessThanOrEqualTo(zDoubleRepeatedList, 20));
    assertEquals(6, ArrayUtilities.lastLessThanOrEqualTo(zDoubleRepeatedList, 25));

  }

  @Test
  public void testSubsetDoubleLastLessThanOrEqualTo() {

    assertEquals(4, ArrayUtilities.lastLessThanOrEqualTo(doubleEmptyList, 5, 0, 0));
    assertEquals(4, ArrayUtilities.lastLessThanOrEqualTo(doubleSingleton, 5, 1, -10));
    assertEquals(5, ArrayUtilities.lastLessThanOrEqualTo(doubleSingleton, 5, 1, 0));
    assertEquals(5, ArrayUtilities.lastLessThanOrEqualTo(doubleSingleton, 5, 1, 10));

    assertEquals(4, ArrayUtilities.lastLessThanOrEqualTo(doubleSortedList, 5, 4, -10));
    assertEquals(5, ArrayUtilities.lastLessThanOrEqualTo(doubleSortedList, 5, 4, 0));
    assertEquals(5, ArrayUtilities.lastLessThanOrEqualTo(doubleSortedList, 5, 4, 5));
    assertEquals(6, ArrayUtilities.lastLessThanOrEqualTo(doubleSortedList, 5, 4, 10));
    assertEquals(6, ArrayUtilities.lastLessThanOrEqualTo(doubleSortedList, 5, 4, 15));
    assertEquals(7, ArrayUtilities.lastLessThanOrEqualTo(doubleSortedList, 5, 4, 20));
    assertEquals(7, ArrayUtilities.lastLessThanOrEqualTo(doubleSortedList, 5, 4, 25));
    assertEquals(8, ArrayUtilities.lastLessThanOrEqualTo(doubleSortedList, 5, 4, 30));
    assertEquals(8, ArrayUtilities.lastLessThanOrEqualTo(doubleSortedList, 5, 4, 35));

    assertEquals(4, ArrayUtilities.lastLessThanOrEqualTo(doubleRepeatedList, 5, 7, -10));
    assertEquals(5, ArrayUtilities.lastLessThanOrEqualTo(doubleRepeatedList, 5, 7, 0));
    assertEquals(5, ArrayUtilities.lastLessThanOrEqualTo(doubleRepeatedList, 5, 7, 5));
    assertEquals(10, ArrayUtilities.lastLessThanOrEqualTo(doubleRepeatedList, 5, 7, 10));
    assertEquals(10, ArrayUtilities.lastLessThanOrEqualTo(doubleRepeatedList, 5, 7, 15));
    assertEquals(11, ArrayUtilities.lastLessThanOrEqualTo(doubleRepeatedList, 5, 7, 20));
    assertEquals(11, ArrayUtilities.lastLessThanOrEqualTo(doubleRepeatedList, 5, 7, 25));

  }

  @Test
  public void testFullListDoubleLastLessThan() {
    assertEquals(-1, ArrayUtilities.lastLessThan(zDoubleEmptyList, 0));

    assertEquals(-1, ArrayUtilities.lastLessThan(zDoubleSingleton, -10));
    assertEquals(-1, ArrayUtilities.lastLessThan(zDoubleSingleton, 0));
    assertEquals(0, ArrayUtilities.lastLessThan(zDoubleSingleton, 10));

    assertEquals(-1, ArrayUtilities.lastLessThan(zDoubleSortedList, -10));
    assertEquals(-1, ArrayUtilities.lastLessThan(zDoubleSortedList, 0));
    assertEquals(0, ArrayUtilities.lastLessThan(zDoubleSortedList, 5));
    assertEquals(0, ArrayUtilities.lastLessThan(zDoubleSortedList, 10));
    assertEquals(1, ArrayUtilities.lastLessThan(zDoubleSortedList, 15));
    assertEquals(1, ArrayUtilities.lastLessThan(zDoubleSortedList, 20));
    assertEquals(2, ArrayUtilities.lastLessThan(zDoubleSortedList, 25));
    assertEquals(2, ArrayUtilities.lastLessThan(zDoubleSortedList, 30));
    assertEquals(3, ArrayUtilities.lastLessThan(zDoubleSortedList, 35));

    assertEquals(-1, ArrayUtilities.lastLessThan(zDoubleRepeatedList, -10));
    assertEquals(-1, ArrayUtilities.lastLessThan(zDoubleRepeatedList, 0));
    assertEquals(0, ArrayUtilities.lastLessThan(zDoubleRepeatedList, 5));
    assertEquals(0, ArrayUtilities.lastLessThan(zDoubleRepeatedList, 10));
    assertEquals(5, ArrayUtilities.lastLessThan(zDoubleRepeatedList, 15));
    assertEquals(5, ArrayUtilities.lastLessThan(zDoubleRepeatedList, 20));
    assertEquals(6, ArrayUtilities.lastLessThan(zDoubleRepeatedList, 25));

  }

  @Test
  public void testSubsetDoubleLastLessThan() {
    assertEquals(4, ArrayUtilities.lastLessThan(doubleEmptyList, 5, 0, 0));

    assertEquals(4, ArrayUtilities.lastLessThan(doubleSingleton, 5, 1, -10));
    assertEquals(4, ArrayUtilities.lastLessThan(doubleSingleton, 5, 1, 0));
    assertEquals(5, ArrayUtilities.lastLessThan(doubleSingleton, 5, 1, 10));

    assertEquals(4, ArrayUtilities.lastLessThan(doubleSortedList, 5, 4, -10));
    assertEquals(4, ArrayUtilities.lastLessThan(doubleSortedList, 5, 4, 0));
    assertEquals(5, ArrayUtilities.lastLessThan(doubleSortedList, 5, 4, 5));
    assertEquals(5, ArrayUtilities.lastLessThan(doubleSortedList, 5, 4, 10));
    assertEquals(6, ArrayUtilities.lastLessThan(doubleSortedList, 5, 4, 15));
    assertEquals(6, ArrayUtilities.lastLessThan(doubleSortedList, 5, 4, 20));
    assertEquals(7, ArrayUtilities.lastLessThan(doubleSortedList, 5, 4, 25));
    assertEquals(7, ArrayUtilities.lastLessThan(doubleSortedList, 5, 4, 30));
    assertEquals(8, ArrayUtilities.lastLessThan(doubleSortedList, 5, 4, 35));

    assertEquals(4, ArrayUtilities.lastLessThan(doubleRepeatedList, 5, 7, -10));
    assertEquals(4, ArrayUtilities.lastLessThan(doubleRepeatedList, 5, 7, 0));
    assertEquals(5, ArrayUtilities.lastLessThan(doubleRepeatedList, 5, 7, 5));
    assertEquals(5, ArrayUtilities.lastLessThan(doubleRepeatedList, 5, 7, 10));
    assertEquals(10, ArrayUtilities.lastLessThan(doubleRepeatedList, 5, 7, 15));
    assertEquals(10, ArrayUtilities.lastLessThan(doubleRepeatedList, 5, 7, 20));
    assertEquals(11, ArrayUtilities.lastLessThan(doubleRepeatedList, 5, 7, 25));
  }

  @Test
  public void testFullListDoubleFirstGreaterThanOrEqualTo() {
    assertEquals(0, ArrayUtilities.firstGreaterThanOrEqualTo(zDoubleEmptyList, 0));

    assertEquals(0, ArrayUtilities.firstGreaterThanOrEqualTo(zDoubleSingleton, -10));
    assertEquals(0, ArrayUtilities.firstGreaterThanOrEqualTo(zDoubleSingleton, 0));
    assertEquals(1, ArrayUtilities.firstGreaterThanOrEqualTo(zDoubleSingleton, 10));

    assertEquals(0, ArrayUtilities.firstGreaterThanOrEqualTo(zDoubleSortedList, -10));
    assertEquals(0, ArrayUtilities.firstGreaterThanOrEqualTo(zDoubleSortedList, 0));
    assertEquals(1, ArrayUtilities.firstGreaterThanOrEqualTo(zDoubleSortedList, 5));
    assertEquals(1, ArrayUtilities.firstGreaterThanOrEqualTo(zDoubleSortedList, 10));
    assertEquals(2, ArrayUtilities.firstGreaterThanOrEqualTo(zDoubleSortedList, 15));
    assertEquals(2, ArrayUtilities.firstGreaterThanOrEqualTo(zDoubleSortedList, 20));
    assertEquals(3, ArrayUtilities.firstGreaterThanOrEqualTo(zDoubleSortedList, 25));
    assertEquals(3, ArrayUtilities.firstGreaterThanOrEqualTo(zDoubleSortedList, 30));
    assertEquals(4, ArrayUtilities.firstGreaterThanOrEqualTo(zDoubleSortedList, 35));

    assertEquals(0, ArrayUtilities.firstGreaterThanOrEqualTo(zDoubleRepeatedList, -10));
    assertEquals(0, ArrayUtilities.firstGreaterThanOrEqualTo(zDoubleRepeatedList, 0));
    assertEquals(1, ArrayUtilities.firstGreaterThanOrEqualTo(zDoubleRepeatedList, 5));
    assertEquals(1, ArrayUtilities.firstGreaterThanOrEqualTo(zDoubleRepeatedList, 10));
    assertEquals(6, ArrayUtilities.firstGreaterThanOrEqualTo(zDoubleRepeatedList, 15));
    assertEquals(6, ArrayUtilities.firstGreaterThanOrEqualTo(zDoubleRepeatedList, 20));
    assertEquals(7, ArrayUtilities.firstGreaterThanOrEqualTo(zDoubleRepeatedList, 25));

  }

  @Test
  public void testSubsetDoubleFirstGreaterThanOrEqualTo() {
    assertEquals(5, ArrayUtilities.firstGreaterThanOrEqualTo(doubleEmptyList, 5, 0, 0));

    assertEquals(5, ArrayUtilities.firstGreaterThanOrEqualTo(doubleSingleton, 5, 1, -10));
    assertEquals(5, ArrayUtilities.firstGreaterThanOrEqualTo(doubleSingleton, 5, 1, 0));
    assertEquals(6, ArrayUtilities.firstGreaterThanOrEqualTo(doubleSingleton, 5, 1, 10));

    assertEquals(5, ArrayUtilities.firstGreaterThanOrEqualTo(doubleSortedList, 5, 4, -10));
    assertEquals(5, ArrayUtilities.firstGreaterThanOrEqualTo(doubleSortedList, 5, 4, 0));
    assertEquals(6, ArrayUtilities.firstGreaterThanOrEqualTo(doubleSortedList, 5, 4, 5));
    assertEquals(6, ArrayUtilities.firstGreaterThanOrEqualTo(doubleSortedList, 5, 4, 10));
    assertEquals(7, ArrayUtilities.firstGreaterThanOrEqualTo(doubleSortedList, 5, 4, 15));
    assertEquals(7, ArrayUtilities.firstGreaterThanOrEqualTo(doubleSortedList, 5, 4, 20));
    assertEquals(8, ArrayUtilities.firstGreaterThanOrEqualTo(doubleSortedList, 5, 4, 25));
    assertEquals(8, ArrayUtilities.firstGreaterThanOrEqualTo(doubleSortedList, 5, 4, 30));
    assertEquals(9, ArrayUtilities.firstGreaterThanOrEqualTo(doubleSortedList, 5, 4, 35));

    assertEquals(5, ArrayUtilities.firstGreaterThanOrEqualTo(doubleRepeatedList, 5, 7, -10));
    assertEquals(5, ArrayUtilities.firstGreaterThanOrEqualTo(doubleRepeatedList, 5, 7, 0));
    assertEquals(6, ArrayUtilities.firstGreaterThanOrEqualTo(doubleRepeatedList, 5, 7, 5));
    assertEquals(6, ArrayUtilities.firstGreaterThanOrEqualTo(doubleRepeatedList, 5, 7, 10));
    assertEquals(11, ArrayUtilities.firstGreaterThanOrEqualTo(doubleRepeatedList, 5, 7, 15));
    assertEquals(11, ArrayUtilities.firstGreaterThanOrEqualTo(doubleRepeatedList, 5, 7, 20));
    assertEquals(12, ArrayUtilities.firstGreaterThanOrEqualTo(doubleRepeatedList, 5, 7, 25));

  }

  @Test
  public void testFullListDoubleFirstGreaterThan() {
    assertEquals(0, ArrayUtilities.firstGreaterThan(zDoubleEmptyList, 0));

    assertEquals(0, ArrayUtilities.firstGreaterThan(zDoubleSingleton, -10));
    assertEquals(1, ArrayUtilities.firstGreaterThan(zDoubleSingleton, 0));
    assertEquals(1, ArrayUtilities.firstGreaterThan(zDoubleSingleton, 10));

    assertEquals(0, ArrayUtilities.firstGreaterThan(zDoubleSortedList, -10));
    assertEquals(1, ArrayUtilities.firstGreaterThan(zDoubleSortedList, 0));
    assertEquals(1, ArrayUtilities.firstGreaterThan(zDoubleSortedList, 5));
    assertEquals(2, ArrayUtilities.firstGreaterThan(zDoubleSortedList, 10));
    assertEquals(2, ArrayUtilities.firstGreaterThan(zDoubleSortedList, 15));
    assertEquals(3, ArrayUtilities.firstGreaterThan(zDoubleSortedList, 20));
    assertEquals(3, ArrayUtilities.firstGreaterThan(zDoubleSortedList, 25));
    assertEquals(4, ArrayUtilities.firstGreaterThan(zDoubleSortedList, 30));
    assertEquals(4, ArrayUtilities.firstGreaterThan(zDoubleSortedList, 35));

    assertEquals(0, ArrayUtilities.firstGreaterThan(zDoubleRepeatedList, -10));
    assertEquals(1, ArrayUtilities.firstGreaterThan(zDoubleRepeatedList, 0));
    assertEquals(1, ArrayUtilities.firstGreaterThan(zDoubleRepeatedList, 5));
    assertEquals(6, ArrayUtilities.firstGreaterThan(zDoubleRepeatedList, 10));
    assertEquals(6, ArrayUtilities.firstGreaterThan(zDoubleRepeatedList, 15));
    assertEquals(7, ArrayUtilities.firstGreaterThan(zDoubleRepeatedList, 20));
    assertEquals(7, ArrayUtilities.firstGreaterThan(zDoubleRepeatedList, 25));

  }

  @Test
  public void testSubsetDoubleFirstGreaterThan() {
    assertEquals(5, ArrayUtilities.firstGreaterThan(doubleEmptyList, 5, 0, 0));

    assertEquals(5, ArrayUtilities.firstGreaterThan(doubleSingleton, 5, 1, -10));
    assertEquals(6, ArrayUtilities.firstGreaterThan(doubleSingleton, 5, 1, 0));
    assertEquals(6, ArrayUtilities.firstGreaterThan(doubleSingleton, 5, 1, 10));

    assertEquals(5, ArrayUtilities.firstGreaterThan(doubleSortedList, 5, 4, -10));
    assertEquals(6, ArrayUtilities.firstGreaterThan(doubleSortedList, 5, 4, 0));
    assertEquals(6, ArrayUtilities.firstGreaterThan(doubleSortedList, 5, 4, 5));
    assertEquals(7, ArrayUtilities.firstGreaterThan(doubleSortedList, 5, 4, 10));
    assertEquals(7, ArrayUtilities.firstGreaterThan(doubleSortedList, 5, 4, 15));
    assertEquals(8, ArrayUtilities.firstGreaterThan(doubleSortedList, 5, 4, 20));
    assertEquals(8, ArrayUtilities.firstGreaterThan(doubleSortedList, 5, 4, 25));
    assertEquals(9, ArrayUtilities.firstGreaterThan(doubleSortedList, 5, 4, 30));
    assertEquals(9, ArrayUtilities.firstGreaterThan(doubleSortedList, 5, 4, 35));

    assertEquals(5, ArrayUtilities.firstGreaterThan(doubleRepeatedList, 5, 7, -10));
    assertEquals(6, ArrayUtilities.firstGreaterThan(doubleRepeatedList, 5, 7, 0));
    assertEquals(6, ArrayUtilities.firstGreaterThan(doubleRepeatedList, 5, 7, 5));
    assertEquals(11, ArrayUtilities.firstGreaterThan(doubleRepeatedList, 5, 7, 10));
    assertEquals(11, ArrayUtilities.firstGreaterThan(doubleRepeatedList, 5, 7, 15));
    assertEquals(12, ArrayUtilities.firstGreaterThan(doubleRepeatedList, 5, 7, 20));
    assertEquals(12, ArrayUtilities.firstGreaterThan(doubleRepeatedList, 5, 7, 25));

  }

  @Test
  public void testIsRagged() {

    Double[][] d0 = new Double[0][0];
    Double[][] d1 = new Double[0][1];
    Double[][] d2 = new Double[1][0];
    Double[][] d3 = new Double[1][1];

    Double[][] d4 = new Double[][] {{1., 2.0}, {3.0, 4.0}};
    Double[][] d5 = new Double[3][3];
    Double[][] d6 = new Double[][] {{1., 2.0, 3.0, 4.0}};
    Double[][] d7 = new Double[][] {{1.}, {2.0}, {3.0}, {4.0}};
    Double[][] dragged = new Double[][] {{1., 2., 3.}, {4., 5.}, {6., 7., 8.}, {9., 10., 11.}};
    Double[][] dragged2 = new Double[][] {{1., 2.}, {3.0, 4., 5.}, {6., 7., 8.}, {9., 10., 11.}};

    assertFalse(ArrayUtilities.isRagged(d0));
    assertFalse(ArrayUtilities.isRagged(d1));
    assertFalse(ArrayUtilities.isRagged(d2));
    assertFalse(ArrayUtilities.isRagged(d3));
    assertFalse(ArrayUtilities.isRagged(d4));
    assertFalse(ArrayUtilities.isRagged(d5));
    assertFalse(ArrayUtilities.isRagged(d6));
    assertFalse(ArrayUtilities.isRagged(d7));
    assertTrue(ArrayUtilities.isRagged(dragged));
    assertTrue(ArrayUtilities.isRagged(dragged2));

  }

  @Test
  public void testIsRagged3Dimensions() {

    Double[][][] d0 = new Double[0][0][0];
    Double[][][] d1 = new Double[0][0][1];
    Double[][][] d2 = new Double[0][1][0];
    Double[][][] d3 = new Double[0][1][1];
    Double[][][] d4 = new Double[1][0][0];
    Double[][][] d5 = new Double[1][0][1];
    Double[][][] d6 = new Double[1][1][0];
    Double[][][] d7 = new Double[1][1][1];
    Double[][][] d8 = new Double[3][3][3];

    Double[][][] d9 = new Double[][][] {{{1.0, 2.0}, {3.0, 4.0}}};
    Double[][][] d10 = new Double[][][] {};
    Double[][][] d11 = new Double[][][] {{}};
    Double[][][] d12 = new Double[][][] {{{}}};
    Double[][][] d13 = new Double[][][] {{}, {}};
    Double[][][] d14 = new Double[][][] {{{}}, {{}}};
    Double[][][] d15 = new Double[][][] {{{}, {}}, {{}, {}}};
    Double[][][] d16 = new Double[][][] {{{}, {}}};

    Double[][][] d17 = new Double[][][] {{{1.0}}};
    Double[][][] d18 = new Double[][][] {{{1.0}, {1.0}}};
    Double[][][] d19 = new Double[][][] {{{1.0, 2.0, 3.0, 4.0}}};
    Double[][][] d20 = new Double[][][] {{{1.0, 2.0, 3.0, 4.0}}, {{5.0, 6.0, 7.0, 8.0}}};
    Double[][][] d21 = new Double[][][] {{{}}, {{}}};
    Double[][][] d22 = new Double[][][] {{{1.0, 2.0, 3.0}, {4.0, 5.0, 6.0}},
        {{7.0, 8.0, 9.0}, {10.0, 11.0, 12.0}}};
    Double[][][] d23 = new Double[][][] {{{}, {}}};

    Double[][][] dr1 = new Double[][][] {{{1., 2., 3.}, {4., 5.}, {6., 7., 8.}, {9., 10., 11.}}};
    Double[][][] dr2 = new Double[][][] {{{1., 2.0}, {3.0, 4.0, 5.0}}};
    Double[][][] dr3 = new Double[][][] {{{1.0, 2.0, 3.0}, {4.0, 5.0, 6.0}, {4.0, 5.0, 6.0}},
        {{7.0, 8.0, 9.0}, {10.0, 11.0, 12.0}}};
    Double[][][] dr4 = new Double[][][] {{{1.0, 2.0, 3.0}, {4.0, 5.0, 6.0}, {4.0, 5.0, 6.0}},
        {{7.0, 8.0, 9.0}, {4.0, 5.0, 6.0}, {10.0, 11.0}}};

    assertFalse(ArrayUtilities.isRagged(d0));
    assertFalse(ArrayUtilities.isRagged(d1));
    assertFalse(ArrayUtilities.isRagged(d2));
    assertFalse(ArrayUtilities.isRagged(d3));
    assertFalse(ArrayUtilities.isRagged(d4));
    assertFalse(ArrayUtilities.isRagged(d5));
    assertFalse(ArrayUtilities.isRagged(d6));
    assertFalse(ArrayUtilities.isRagged(d7));
    assertFalse(ArrayUtilities.isRagged(d8));
    assertFalse(ArrayUtilities.isRagged(d9));
    assertFalse(ArrayUtilities.isRagged(d10));
    assertFalse(ArrayUtilities.isRagged(d11));
    assertFalse(ArrayUtilities.isRagged(d12));
    assertFalse(ArrayUtilities.isRagged(d13));
    assertFalse(ArrayUtilities.isRagged(d14));
    assertFalse(ArrayUtilities.isRagged(d15));
    assertFalse(ArrayUtilities.isRagged(d16));
    assertFalse(ArrayUtilities.isRagged(d17));
    assertFalse(ArrayUtilities.isRagged(d18));
    assertFalse(ArrayUtilities.isRagged(d19));
    assertFalse(ArrayUtilities.isRagged(d20));
    assertFalse(ArrayUtilities.isRagged(d21));
    assertFalse(ArrayUtilities.isRagged(d22));
    assertFalse(ArrayUtilities.isRagged(d23));

    assertTrue(ArrayUtilities.isRagged(dr1));
    assertTrue(ArrayUtilities.isRagged(dr2));
    assertTrue(ArrayUtilities.isRagged(dr3));
    assertTrue(ArrayUtilities.isRagged(dr4));

  }

  @Test
  public void testIsRaggedDouble() {

    double[][] d0 = new double[0][0];
    double[][] d1 = new double[1][1];
    double[][] d2 = new double[][] {{1., 2.0}, {3.0, 4.0}};
    double[][] d3 = new double[3][3];
    double[][] d4 = new double[][] {{1., 2.0, 3.0, 4.0}};
    double[][] d5 = new double[][] {{1.}, {2.0}, {3.0}, {4.0}};
    double[][] dragged = new double[][] {{1., 2., 3.}, {4., 5.}, {6., 7., 8.}, {9., 10., 11.}};
    double[][] dragged2 = new double[][] {{1., 2.}, {3.0, 4., 5.}, {6., 7., 8.}, {9., 10., 11.}};

    assertFalse(ArrayUtilities.isRagged(d0));
    assertFalse(ArrayUtilities.isRagged(d1));
    assertFalse(ArrayUtilities.isRagged(d2));
    assertFalse(ArrayUtilities.isRagged(d3));
    assertFalse(ArrayUtilities.isRagged(d4));
    assertFalse(ArrayUtilities.isRagged(d5));
    assertTrue(ArrayUtilities.isRagged(dragged));
    assertTrue(ArrayUtilities.isRagged(dragged2));

  }

  @Test
  public void testIsRaggedDouble3Dimensions() {

    double[][][] d0 = new double[0][0][0];
    double[][][] d1 = new double[0][0][1];
    double[][][] d2 = new double[0][1][0];
    double[][][] d3 = new double[0][1][1];
    double[][][] d4 = new double[1][0][0];
    double[][][] d5 = new double[1][0][1];
    double[][][] d6 = new double[1][1][0];
    double[][][] d7 = new double[1][1][1];
    double[][][] d8 = new double[3][3][3];

    double[][][] d9 = new double[][][] {{{1.0, 2.0}, {3.0, 4.0}}};
    double[][][] d10 = new double[][][] {};
    double[][][] d11 = new double[][][] {{}};
    double[][][] d12 = new double[][][] {{{}}};
    double[][][] d13 = new double[][][] {{}, {}};
    double[][][] d14 = new double[][][] {{{}}, {{}}};
    double[][][] d15 = new double[][][] {{{}, {}}, {{}, {}}};
    double[][][] d16 = new double[][][] {{{}, {}}};

    double[][][] d17 = new double[][][] {{{1.0}}};
    double[][][] d18 = new double[][][] {{{1.0}, {1.0}}};
    double[][][] d19 = new double[][][] {{{1.0, 2.0, 3.0, 4.0}}};
    double[][][] d20 = new double[][][] {{{1.0, 2.0, 3.0, 4.0}}, {{5.0, 6.0, 7.0, 8.0}}};
    double[][][] d21 = new double[][][] {{{}}, {{}}};
    double[][][] d22 = new double[][][] {{{1.0, 2.0, 3.0}, {4.0, 5.0, 6.0}},
        {{7.0, 8.0, 9.0}, {10.0, 11.0, 12.0}}};
    double[][][] d23 = new double[][][] {{{}, {}}};

    double[][][] dr1 = new double[][][] {{{1., 2., 3.}, {4., 5.}, {6., 7., 8.}, {9., 10., 11.}}};
    double[][][] dr2 = new double[][][] {{{1., 2.0}, {3.0, 4.0, 5.0}}};
    double[][][] dr3 = new double[][][] {{{1.0, 2.0, 3.0}, {4.0, 5.0, 6.0}, {4.0, 5.0, 6.0}},
        {{7.0, 8.0, 9.0}, {10.0, 11.0, 12.0}}};
    double[][][] dr4 = new double[][][] {{{1.0, 2.0, 3.0}, {4.0, 5.0, 6.0}, {4.0, 5.0, 6.0}},
        {{7.0, 8.0, 9.0}, {4.0, 5.0, 6.0}, {10.0, 11.0}}};

    assertFalse(ArrayUtilities.isRagged(d0));
    assertFalse(ArrayUtilities.isRagged(d1));
    assertFalse(ArrayUtilities.isRagged(d2));
    assertFalse(ArrayUtilities.isRagged(d3));
    assertFalse(ArrayUtilities.isRagged(d4));
    assertFalse(ArrayUtilities.isRagged(d5));
    assertFalse(ArrayUtilities.isRagged(d6));
    assertFalse(ArrayUtilities.isRagged(d7));
    assertFalse(ArrayUtilities.isRagged(d8));
    assertFalse(ArrayUtilities.isRagged(d9));
    assertFalse(ArrayUtilities.isRagged(d10));
    assertFalse(ArrayUtilities.isRagged(d11));
    assertFalse(ArrayUtilities.isRagged(d12));
    assertFalse(ArrayUtilities.isRagged(d13));
    assertFalse(ArrayUtilities.isRagged(d14));
    assertFalse(ArrayUtilities.isRagged(d15));
    assertFalse(ArrayUtilities.isRagged(d16));
    assertFalse(ArrayUtilities.isRagged(d17));
    assertFalse(ArrayUtilities.isRagged(d18));
    assertFalse(ArrayUtilities.isRagged(d19));
    assertFalse(ArrayUtilities.isRagged(d20));
    assertFalse(ArrayUtilities.isRagged(d21));
    assertFalse(ArrayUtilities.isRagged(d22));
    assertFalse(ArrayUtilities.isRagged(d23));

    assertTrue(ArrayUtilities.isRagged(dr1));
    assertTrue(ArrayUtilities.isRagged(dr2));
    assertTrue(ArrayUtilities.isRagged(dr3));
    assertTrue(ArrayUtilities.isRagged(dr4));

  }
}
