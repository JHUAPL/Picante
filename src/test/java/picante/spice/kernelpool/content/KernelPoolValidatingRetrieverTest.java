package picante.spice.kernelpool.content;

import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.junit.Before;
import org.junit.Test;
import com.google.common.base.Predicate;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Sets;
import picante.spice.kernelpool.BasicKernelPool;

public class KernelPoolValidatingRetrieverTest {

  private BasicKernelPool pool;
  private KernelPoolValidatingRetriever retriever;
  private Set<Integer> testSet;
  private List<Double> testDoubleListLong;

  @Before
  public void setUp() throws Exception {
    pool = new BasicKernelPool();

    List<String> strings = new ArrayList<String>();
    List<Double> doubles = new ArrayList<Double>();

    strings.add("VALUE");
    pool.addStrings("TESTSTRING", strings);

    doubles.add(1.0);
    pool.addDoubles("TESTDOUBLE", doubles);

    doubles.add(2.0);
    pool.addDoubles("TESTDOUBLELIST", doubles);

    doubles.add(3.0);
    doubles.add(4.0);
    doubles.add(5.0);
    doubles.add(6.0);
    doubles.add(7.0);
    doubles.add(8.0);
    doubles.add(9.0);
    pool.addDoubles("TESTDOUBLELISTLONG", doubles);
    testDoubleListLong = new ArrayList<Double>(10);
    testDoubleListLong.addAll(doubles);

    retriever = new KernelPoolValidatingRetriever(pool);

    testSet = new HashSet<Integer>();
    testSet.add(1);
    testSet.add(2);
  }

  @Test
  public void testGetPool() {
    assertSame(pool, retriever.getPool());
  }

  @Test
  public void testContainsKeyword() {
    assertFalse(retriever.containsKeyword("NOT_PRESENT"));
    assertTrue(retriever.containsKeyword("TESTSTRING"));
    assertTrue(retriever.containsKeyword("TESTDOUBLE"));
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetDoubleStringStringException() throws Exception {
    retriever.getDouble("TESTSTRING");
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetDoubleStringEmptyException() throws Exception {
    retriever.getDouble("NOT_PRESENT");
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetDoubleStringNonUnitLengthException() throws Exception {
    retriever.getDouble("TESTDOUBLELIST");
  }

  @Test
  public void testGetDoubleString() throws Exception {
    assertEquals(1.0, retriever.getDouble("TESTDOUBLE"), 0.0);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetIntegerStringStringException() throws Exception {
    retriever.getInteger("TESTSTRING");
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetIntegerStringEmptyException() throws Exception {
    retriever.getInteger("NOT_PRESENT");
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetIntegerStringNonUnitLengthException() throws Exception {
    retriever.getInteger("TESTDOUBLELIST");
  }

  @Test
  public void testGetIntegerString() throws Exception {
    assertEquals(1, retriever.getInteger("TESTDOUBLE"));
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetIntegerStringIntArrayStringException() throws Exception {
    retriever.getInteger("TESTSTRING", testSet);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetIntegerStringIntArrayEmptyException() throws Exception {
    retriever.getInteger("NOT_PRESENT", testSet);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetIntegerStringIntArrayNonUnitLengthException() throws Exception {
    retriever.getInteger("TESTDOUBLELIST", testSet);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetIntegerStringIntArrayInvalidValueException() throws Exception {
    Set<Integer> missingValueSet = new HashSet<Integer>();
    missingValueSet.add(3);
    missingValueSet.add(5);
    missingValueSet.add(7);
    assertEquals(1, retriever.getInteger("TESTDOUBLE", missingValueSet));
  }

  @Test
  public void testGetIntegerStringIntArray() throws Exception {
    Set<Integer> valueSet = new HashSet<Integer>();
    valueSet.add(1);
    valueSet.add(2);
    valueSet.add(3);
    assertEquals(1, retriever.getInteger("TESTDOUBLE", valueSet));
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetDoublesNotPresentException() throws Exception {
    retriever.getDoubles("NOT_PRESENT");
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetDoublesStringValueException() throws Exception {
    retriever.getDoubles("TESTSTRING");
  }

  @Test
  public void testGetDoubles() throws Exception {
    List<Double> expected = new ArrayList<Double>();
    expected.add(1.0);

    assertEquals(expected, retriever.getDoubles("TESTDOUBLE"));

    expected.add(2.0);
    assertEquals(expected, retriever.getDoubles("TESTDOUBLELIST"));

  }

  @Test
  public void testGetDoublesWithValueValidation() throws Exception {
    List<Double> expected = Arrays.asList(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0);

    Predicate<Double> mock = createMock(Predicate.class);

    for (Double d : expected) {
      expect(mock.apply(d)).andReturn(true);
    }

    replay(mock);

    assertEquals(expected, retriever.getDoublesWithValueValidation("TESTDOUBLELISTLONG", mock));

    verify(mock);

  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetDoublesWithValueValidationFailure() throws Exception {
    retriever.getDoublesWithValueValidation("TESTDOUBLELISTLONG", new Predicate<Double>() {

      @Override
      public boolean apply(Double input) {
        return input <= 7.0;
      }
    });
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetDoublesExpectedLengthNotPresentException() throws Exception {
    retriever.getDoublesExpectedLength("NOT_PRESENT", 1);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetDoublesExpectedLengthStringValueException() throws Exception {
    retriever.getDoublesExpectedLength("TESTSTRING", 1);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetDoublesExpectedLengthViolationException() throws Exception {
    retriever.getDoublesExpectedLength("TESTDOUBLELISTLONG", 5);
  }

  public void testGetDoublesExpectedLength() throws Exception {
    assertEquals(testDoubleListLong, retriever.getDoublesExpectedLength("TESTDOUBLELISTLONG", 9));
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetDoublesLengthNoMoreThanNotPresentException() throws Exception {
    retriever.getDoublesLengthNoMoreThan("NOT_PRESENT", 1);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetDoublesLengthNoMoreThanStringValueException() throws Exception {
    retriever.getDoublesLengthNoMoreThan("TESTSTRING", 1);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetDoublesLengthNoMoreThanLengthViolationException() throws Exception {
    retriever.getDoublesLengthNoMoreThan("TESTDOUBLELISTLONG", 3);
  }

  public void testGetDoublesLengthNoMoreThanEqual() throws Exception {
    retriever.getDoublesLengthNoMoreThan("TESTDOUBLELISTLONG", 9);
  }

  public void testGetDoublesLengthNoMoreThanGreater() throws Exception {
    retriever.getDoublesLengthNoMoreThan("TESTDOUBLELISTLONG", 12);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetDoublesLengthModuloNotPresentException() throws Exception {
    retriever.getDoublesLengthModulo("NOT_PRESENT", 1);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetDoublesLengthModuloStringValueException() throws Exception {
    retriever.getDoublesLengthModulo("TESTSTRING", 1);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetDoublesLengthModuloViolationSingletonException() throws Exception {
    retriever.getDoublesLengthModulo("TESTDOUBLE", 3);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetDoublesLengthModuloViolationException() throws Exception {
    retriever.getDoublesLengthModulo("TESTDOUBLELISTLONG", 6);
  }

  @Test
  public void testGetDoublesLengthModulo() throws Exception {
    assertEquals(testDoubleListLong, retriever.getDoublesLengthModulo("TESTDOUBLELISTLONG", 3));
  }

  @Test
  public void testGetDoublesMaximumLength() throws Exception {
    assertEquals(testDoubleListLong, retriever.getDoublesMaximumLength("TESTDOUBLELISTLONG", 20));
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetDoublesMaximumLengthException() throws Exception {
    assertEquals(testDoubleListLong, retriever.getDoublesMaximumLength("TESTDOUBLELISTLONG", 5));
  }

  @Test
  public void testGetMatchingKeywords() {

    Matcher matcher = Pattern.compile(".*DOUBLE.*").matcher("");

    ImmutableList<String> keywords = retriever.getMatchingKeywords(matcher);

    assertEquals(3, keywords.size());
    assertTrue(keywords.contains("TESTDOUBLE"));
    assertTrue(keywords.contains("TESTDOUBLELIST"));
    assertTrue(keywords.contains("TESTDOUBLELISTLONG"));

  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetIntegersExpectedLengthStringIntShortLengthException() throws Exception {
    retriever.getIntegersExpectedLength("TESTDOUBLELISTLONG", 5);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetIntegersExpectedLengthStringIntLongLengthException() throws Exception {
    retriever.getIntegersExpectedLength("TESTDOUBLELISTLONG", 12);
  }

  @Test
  public void testGetIntegersExpectedLengthStringInt() throws Exception {
    List<Integer> values = retriever.getIntegersExpectedLength("TESTDOUBLELISTLONG", 9);
    List<Integer> expected = Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9);
    assertEquals(expected, values);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetIntegersExpectedLengthStringSetIntegerIntUnacceptableValuesException()
      throws Exception {
    Set<Integer> acceptableValues = Sets.newHashSet(Arrays.asList(1, 2, 3));
    retriever.getIntegersExpectedLength("TESTDOUBLELISTLONG", acceptableValues, 9);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetIntegersExpectedLengthStringSetIntegerIntLengthException() throws Exception {
    Set<Integer> acceptableValues = Sets.newHashSet(Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9));
    retriever.getIntegersExpectedLength("TESTDOUBLELISTLONG", acceptableValues, 5);
  }

  @Test
  public void testGetIntegersExpectedLengthStringSetIntegerInt() throws Exception {
    Set<Integer> acceptableValues =
        Sets.newHashSet(Arrays.asList(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
    List<Integer> values =
        retriever.getIntegersExpectedLength("TESTDOUBLELISTLONG", acceptableValues, 9);
    List<Integer> expected = Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9);
    assertEquals(expected, values);

  }

}
