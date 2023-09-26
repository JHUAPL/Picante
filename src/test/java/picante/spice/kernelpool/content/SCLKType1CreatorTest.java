package picante.spice.kernelpool.content;

import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.util.Arrays;
import org.junit.Before;
import org.junit.Test;
import picante.spice.kernel.tk.lsk.UniformTimeProvider;
import picante.spice.kernel.tk.sclk.SCLKType1;
import picante.spice.kernel.tk.sclk.SCLKType1Record;

public class SCLKType1CreatorTest {

  private SCLKType1Creator creator;
  private KernelPoolValidatingRetriever retriever;
  private UniformTimeProvider tdb;
  private UniformTimeProvider tdt;

  @Before
  public void setUp() throws Exception {
    creator = new SCLKType1Creator();
    tdb = new ConverterMarker();
    tdt = new ConverterMarker();
    retriever = createMock(KernelPoolValidatingRetriever.class);
  }

  @Test
  public void testIntegralFilterPredicate() {
    assertTrue(SCLKType1Creator.INTEGRAL_FILTER.apply(1.0));
    assertTrue(SCLKType1Creator.INTEGRAL_FILTER.apply(-1.0));
    assertFalse(SCLKType1Creator.INTEGRAL_FILTER.apply(1.0 - Math.ulp(1.0)));
    assertFalse(SCLKType1Creator.INTEGRAL_FILTER.apply(1.0 + Math.ulp(1.0)));
  }

  @Test
  public void testGreaterThanOnePredicate() {
    assertTrue(SCLKType1Creator.GREATER_THAN_OR_EQUAL_TO_ONE.apply(1.0));
    assertFalse(SCLKType1Creator.GREATER_THAN_OR_EQUAL_TO_ONE.apply(1.0 - Math.ulp(1.0)));
  }

  @Test
  public void testModuliValidationPredicate() {
    assertTrue(SCLKType1Creator.MODULI_VALIDATION.apply(10.0));
    assertFalse(SCLKType1Creator.MODULI_VALIDATION.apply(10.1));
    assertFalse(SCLKType1Creator.MODULI_VALIDATION.apply(0.0));
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testCreateForgivingValidationException() throws Exception {
    expect(retriever.containsKeyword("SCLK01_TIME_SYSTEM_82")).andReturn(false);
    expect(retriever.getDoublesLengthModulo("SCLK01_COEFFICIENTS_82", 3))
        .andReturn(Arrays.asList(0.0, 0.0, 1.0, 10000.0, 10000.0, 1.0));
    expect(retriever.getDoubles("SCLK_PARTITION_START_82")).andReturn(Arrays.asList(0.0));
    expect(retriever.getDoubles("SCLK_PARTITION_END_82")).andReturn(Arrays.asList(1000000.0));

    /*
     * Simulate an unacceptable moduli value
     */
    expect(retriever.getDoublesWithValueValidation("SCLK01_MODULI_82",
        SCLKType1Creator.GREATER_THAN_OR_EQUAL_TO_ONE))
            .andThrow(new KernelPoolValidationException());
    expect(retriever.getDoublesWithValueValidation("SCLK01_OFFSETS_82",
        SCLKType1Creator.INTEGRAL_FILTER)).andReturn(Arrays.asList(0.0));

    replay(retriever);

    SCLKType1 sclk = creator.createForgiving(82, retriever, tdb, tdt);

    verify(retriever);

    assertEquals(-82, sclk.getID());
    assertEquals(1e6, sclk.getMaxEncodedSCLK(), 0.0);

    SCLKType1Record record = sclk.getRecordTable().get(0, new SCLKType1Record());
    assertEquals(1.0, record.getTicksPerMostSignificantCount(), 0.0);
    assertEquals(0.0, record.getEncodedSCLK(), 0.0);
    assertEquals(0.0, record.getParallelTime(), 0.0);
    assertEquals(1.0, record.getRate(), 0.0);
  }

  @Test
  public void testCreateForgiving() throws Exception {
    expect(retriever.containsKeyword("SCLK01_TIME_SYSTEM_82")).andReturn(false);
    expect(retriever.getDoublesLengthModulo("SCLK01_COEFFICIENTS_82", 3))
        .andReturn(Arrays.asList(0.0, 0.0, 1.0, 10000.0, 10000.0, 1.0));
    expect(retriever.getDoubles("SCLK_PARTITION_START_82")).andReturn(Arrays.asList(0.0));
    expect(retriever.getDoubles("SCLK_PARTITION_END_82")).andReturn(Arrays.asList(1000000.0));

    /*
     * Simulate an unacceptable moduli value
     */
    expect(retriever.getDoublesWithValueValidation("SCLK01_MODULI_82",
        SCLKType1Creator.GREATER_THAN_OR_EQUAL_TO_ONE)).andReturn(Arrays.asList(1000000.1));
    expect(retriever.getDoubles("SCLK01_OFFSETS_82")).andReturn(Arrays.asList(0.0));

    replay(retriever);

    SCLKType1 sclk = creator.createForgiving(82, retriever, tdb, tdt);

    verify(retriever);

    assertEquals(-82, sclk.getID());
    assertEquals(1e6, sclk.getMaxEncodedSCLK(), 0.0);

    SCLKType1Record record = sclk.getRecordTable().get(0, new SCLKType1Record());
    assertEquals(1.0, record.getTicksPerMostSignificantCount(), 0.0);
    assertEquals(0.0, record.getEncodedSCLK(), 0.0);
    assertEquals(0.0, record.getParallelTime(), 0.0);
    assertEquals(1.0, record.getRate(), 0.0);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testCreateValidationException() throws Exception {
    expect(retriever.containsKeyword("SCLK01_TIME_SYSTEM_82")).andReturn(false);
    expect(retriever.getDoublesLengthModulo("SCLK01_COEFFICIENTS_82", 3))
        .andReturn(Arrays.asList(0.0, 0.0, 1.0, 10000.0, 10000.0, 1.0));
    expect(retriever.getDoubles("SCLK_PARTITION_START_82")).andReturn(Arrays.asList(0.0));
    expect(retriever.getDoubles("SCLK_PARTITION_END_82")).andReturn(Arrays.asList(1000000.0));

    /*
     * Simulate an unacceptable moduli value
     */
    expect(retriever.getDoublesWithValueValidation("SCLK01_MODULI_82",
        SCLKType1Creator.MODULI_VALIDATION)).andThrow(new KernelPoolValidationException());
    expect(retriever.getDoublesWithValueValidation("SCLK01_OFFSETS_82",
        SCLKType1Creator.INTEGRAL_FILTER)).andReturn(Arrays.asList(0.0));

    replay(retriever);

    SCLKType1 sclk = creator.create(82, retriever, tdb, tdt);

    verify(retriever);

    assertEquals(-82, sclk.getID());
    assertEquals(1e6, sclk.getMaxEncodedSCLK(), 0.0);

    SCLKType1Record record = sclk.getRecordTable().get(0, new SCLKType1Record());
    assertEquals(1.0, record.getTicksPerMostSignificantCount(), 0.0);
    assertEquals(0.0, record.getEncodedSCLK(), 0.0);
    assertEquals(0.0, record.getParallelTime(), 0.0);
    assertEquals(1.0, record.getRate(), 0.0);
  }

  @Test
  public void testCreate() throws Exception {
    KernelPoolValidatingRetriever retriever = createMock(KernelPoolValidatingRetriever.class);

    expect(retriever.containsKeyword("SCLK01_TIME_SYSTEM_82")).andReturn(false);
    expect(retriever.getDoublesLengthModulo("SCLK01_COEFFICIENTS_82", 3))
        .andReturn(Arrays.asList(0.0, 0.0, 1.0, 10000.0, 10000.0, 1.0));
    expect(retriever.getDoubles("SCLK_PARTITION_START_82")).andReturn(Arrays.asList(0.0));
    expect(retriever.getDoubles("SCLK_PARTITION_END_82")).andReturn(Arrays.asList(1000000.0));
    expect(retriever.getDoublesWithValueValidation("SCLK01_MODULI_82",
        SCLKType1Creator.MODULI_VALIDATION)).andReturn(Arrays.asList(1000000.0));
    expect(retriever.getDoublesWithValueValidation("SCLK01_OFFSETS_82",
        SCLKType1Creator.INTEGRAL_FILTER)).andReturn(Arrays.asList(0.0));

    replay(retriever);

    SCLKType1 sclk = creator.create(82, retriever, tdb, tdt);

    verify(retriever);

    assertEquals(-82, sclk.getID());
    assertEquals(1e6, sclk.getMaxEncodedSCLK(), 0.0);

    SCLKType1Record record = sclk.getRecordTable().get(0, new SCLKType1Record());
    assertEquals(1.0, record.getTicksPerMostSignificantCount(), 0.0);
    assertEquals(0.0, record.getEncodedSCLK(), 0.0);
    assertEquals(0.0, record.getParallelTime(), 0.0);
    assertEquals(1.0, record.getRate(), 0.0);

  }

  @Test
  public void testComputeTicksPerMostSignificantCount() {
    assertEquals(6.24E6, SCLKType1Creator
        .computeTicksPerMostSignificantCount(Arrays.asList(1000.0, 1200.0, 52.0, 100.0)), 0.0);
  }

  @Test
  public void testComputeTicksPerMostSignificantCountSingleStage() {
    assertEquals(1.0, SCLKType1Creator.computeTicksPerMostSignificantCount(Arrays.asList(1000.0)),
        0.0);

  }

  @Test
  public void testChooseConverterDefaultValue() throws Exception {
    expect(retriever.containsKeyword("SCLK01_TIME_SYSTEM_82")).andReturn(false);
    replay(retriever);
    UniformTimeProvider converter = SCLKType1Creator.chooseConverter(82, retriever, tdb, tdt);
    verify(retriever);
    assertSame(tdb, converter);
  }

  @Test
  public void testChooseConverterTDTValue() throws Exception {
    expect(retriever.containsKeyword("SCLK01_TIME_SYSTEM_82")).andReturn(true);
    expect(retriever.getInteger("SCLK01_TIME_SYSTEM_82",
        SCLKType1Creator.ACCEPTABLE_TIME_SYSTEM_VALUES)).andReturn(2);
    replay(retriever);
    UniformTimeProvider converter = SCLKType1Creator.chooseConverter(82, retriever, tdb, tdt);
    verify(retriever);
    assertSame(tdt, converter);
  }

  @Test
  public void testChooseConverterTDBValue() throws Exception {
    expect(retriever.containsKeyword("SCLK01_TIME_SYSTEM_82")).andReturn(true);
    expect(retriever.getInteger("SCLK01_TIME_SYSTEM_82",
        SCLKType1Creator.ACCEPTABLE_TIME_SYSTEM_VALUES)).andReturn(1);
    replay(retriever);
    UniformTimeProvider converter = SCLKType1Creator.chooseConverter(82, retriever, tdb, tdt);
    verify(retriever);
    assertSame(tdb, converter);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testChooseConverterInvalidElementException() throws Exception {
    expect(retriever.containsKeyword("SCLK01_TIME_SYSTEM_82")).andReturn(true);
    expect(retriever.getInteger("SCLK01_TIME_SYSTEM_82",
        SCLKType1Creator.ACCEPTABLE_TIME_SYSTEM_VALUES))
            .andThrow(new KernelPoolValidationException());
    replay(retriever);
    SCLKType1Creator.chooseConverter(82, retriever, tdb, tdt);
  }

}


@SuppressWarnings("unused")
/*
 * We can't simply throw unsupported operation exceptions here, as various constructors may utilize
 * these methods.
 */
class ConverterMarker implements UniformTimeProvider {

  @Override
  public double convertToUniformTime(double tdb) {
    return 0.0;
  }

  @Override
  public double convertToTDB(double parallelTime) {
    return 0.0;
  }

}
