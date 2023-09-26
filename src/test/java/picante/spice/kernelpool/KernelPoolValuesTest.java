package picante.spice.kernelpool;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertTrue;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import com.google.common.collect.Lists;

public class KernelPoolValuesTest {

  private List<String> list;
  private List<String> otherList;
  private KernelPoolValues<String> values;

  @Before
  public void setUp() throws Exception {
    list = Lists.newArrayList();
    list.add("A");
    list.add("B");

    otherList = Lists.newArrayList();
    otherList.add("C");
    otherList.add("D");
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testKernelPoolValuesBooleanListOfTTrueAppend() {
    values = new KernelPoolValues<String>(true, list);
    assertTrue(values.appendState);
    assertNotSame(list, values.values);
    assertEquals(list, values.values);
    values.values.add("TEST");
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testKernelPoolValuesBooleanListOfTFalseAppend() {
    values = new KernelPoolValues<String>(false, list);
    assertFalse(values.appendState);
    assertNotSame(list, values.values);
    assertEquals(list, values.values);
    values.values.add("TEST");
  }


  @Test(expected = UnsupportedOperationException.class)
  public void testKernelPoolValuesKernelPoolValuesOfTListOfTTrueAppend() {
    KernelPoolValues<String> existing = new KernelPoolValues<String>(true, list);
    values = new KernelPoolValues<String>(existing, otherList);
    assertTrue(values.appendState);
    assertNotSame(list, values.values);
    assertNotSame(otherList, values.values);
    List<String> expected = Lists.newArrayList(list);
    expected.addAll(otherList);
    assertEquals(expected, values.values);
    values.values.add("TEST");
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testKernelPoolValuesKernelPoolValuesOfTListOfTFalseAppend() {
    KernelPoolValues<String> existing = new KernelPoolValues<String>(false, list);
    values = new KernelPoolValues<String>(existing, otherList);
    assertFalse(values.appendState);
    assertNotSame(list, values.values);
    assertNotSame(otherList, values.values);
    List<String> expected = Lists.newArrayList(list);
    expected.addAll(otherList);
    assertEquals(expected, values.values);
    values.values.add("TEST");
  }

  @Test
  public void testKernelPoolValuesKernelPoolValuesOfTTrueAppend() {
    KernelPoolValues<String> existing = new KernelPoolValues<String>(true, list);
    values = new KernelPoolValues<String>(existing);
    assertTrue(values.appendState);
    assertNotSame(list, values.values);
    assertEquals(list, values.values);
  }

  @Test
  public void testKernelPoolValuesKernelPoolValuesOfTFalseAppend() {
    KernelPoolValues<String> existing = new KernelPoolValues<String>(false, list);
    values = new KernelPoolValues<String>(existing);
    assertFalse(values.appendState);
    assertNotSame(list, values.values);
    assertEquals(list, values.values);
  }

}
