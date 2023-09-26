package picante.spice.kernel.tk.sclk;

import java.util.List;
import picante.data.list.AbstractGaugedRetrievable;
import picante.data.list.GaugedRetrievableLLE;
import picante.math.intervals.Interval;
import picante.math.intervals.UnwritableInterval;
import picante.spice.kernel.tk.lsk.UniformTimeProvider;

/**
 * Implementation of the NAIF type 1 SCLK.
 * 
 * TODO: Provide detailed discussion of the type 1 SCLK conversion and required data
 */
public class SCLKType1 extends SCLKKernel {

  /**
   * Table of SCLK record triplets
   */
  private final GaugedRetrievableLLE<SCLKType1Record> recordTable;

  /**
   * Converter that moves from the parallel time system to TDB and back.
   */
  private final UniformTimeProvider parallelTimeConverter;

  /**
   * Table that enables searching for records on the parallel time system.
   */
  private final ParallelTimeSearchTable parallelTable = new ParallelTimeSearchTable();

  /**
   * Interval that describes the range of TDBs over which conversions back and forth to encoded SCLK
   * may be safely performed.
   */
  private final UnwritableInterval tdbRange;

  /**
   * The range of encoded SCLKs that may be converted back and forth between TDB or TDT.
   */
  private final UnwritableInterval convertibleSclkRange;

  /**
   * Creates a new type 1 SCLK implementation of the {@link EncodedSCLKConverter} interface.
   * 
   * @param clockID the NAIF ID for the spacecraft clock
   * @param recordTable a record table that searches SclkType1Records on the encoded SCLK field
   */
  public SCLKType1(int clockID, UniformTimeProvider converter,
      GaugedRetrievableLLE<Interval> partitionTable, List<Double> moduli, List<Double> offsets,
      GaugedRetrievableLLE<SCLKType1Record> recordTable) throws SCLKInstantiationException {

    super(clockID, new SCLKType1TickConverter(clockID, moduli, offsets), partitionTable,
        new UnwritableInterval(recordTable.getGauge(0), computeMaxEncodedSCLK(partitionTable)));

    this.parallelTimeConverter = converter;
    this.recordTable = recordTable;

    /*
     * Compute the maximum acceptable encoded SCLK from the supplied partition table.
     */
    this.convertibleSclkRange =
        new UnwritableInterval(recordTable.getGauge(0), validSclkRange.getEnd());
    this.tdbRange = new UnwritableInterval(convertToTDB(convertibleSclkRange.getBegin()),
        convertToTDB(convertibleSclkRange.getEnd()));
  }

  @Override
  public int getType() {
    return 1;
  }

  @Override
  protected SCLKType1TickConverter getTickConverter() {
    return (SCLKType1TickConverter) super.getTickConverter();
  }

  /**
   * Retrieves the number of clock fields used to define this type 1 clock.
   * 
   * @return the number of clock fields
   */
  public int getNumberOfFields() {
    return getTickConverter().getNumberOfFields();
  }

  /**
   * Retrieves the number of ticks (counts of least significant field of the clock) per increment of
   * the specified field.
   * 
   * @param index
   * 
   * @return
   * 
   * @throws IndexOutOfBoundsException if index is in [0,{@link SCLKType1#getNumberOfFields()-1];
   */
  public double getTicksPerField(int index) {
    return getTickConverter().getTicksPerField(index);
  }

  /**
   * Retrieves the modulus of the specified field.
   * 
   * @param index
   * @return
   * 
   * @throws IndexOutOfBoundsException if index is in [0,{@link SCLKType1#getNumberOfFields()-1];
   */
  public double getFieldModulus(int index) {
    return getTickConverter().getFieldModulus(index);
  }

  /**
   * Retrieves the offset of the specified field.
   * 
   * @param index
   * @return
   * 
   * @throws IndexOutOfBoundsException if index is in [0,{@link SCLKType1#getNumberOfFields()-1];
   */
  public double getFieldOffset(int index) {
    return getTickConverter().getOffset(index);
  }

  @Override
  public Interval getTDBRange(Interval buffer) {
    return buffer.setTo(tdbRange);
  }

  @Override
  public Interval getEncodedSclkRange(Interval buffer) {
    return buffer.setTo(convertibleSclkRange);
  }

  /**
   * Retrieves the SCLK record table
   * 
   * @return a reference to the internally held record table of SCLK Type 1 records
   */
  public GaugedRetrievableLLE<SCLKType1Record> getRecordTable() {
    return recordTable;
  }

  @Override
  public double convertToTDB(double encodedSCLK) {

    SCLKType1Record recordBuffer = new SCLKType1Record();

    /*
     * First check to see if the supplied encoded SCLK exceeds the maximum allowed value.
     */
    if (encodedSCLK > convertibleSclkRange.getEnd()) {
      throw new SCLKEvaluationException(getID(),
          "The supplied encoded SCLK: " + encodedSCLK
              + " exceeds the maximum allowed value supported " + "by the underlying SCLK kernel: "
              + convertibleSclkRange.getEnd());
    }

    /*
     * Locate the appropriate record in the table.
     */
    int index = recordTable.indexLastLessThanOrEqualTo(encodedSCLK);

    if (index == -1) {
      throw new SCLKEvaluationException(getID(),
          "Supplied SCLK: " + encodedSCLK + " precedes the first entry "
              + "in SCLK coefficient table: " + recordTable.get(0, recordBuffer).getEncodedSCLK());
    }

    recordTable.get(index, recordBuffer);

    return parallelTimeConverter.convertToTDB(recordBuffer.extrapolateToParallelTime(encodedSCLK));
  }

  @Override
  public double convertToEncodedSclk(double tdb) {

    SCLKType1Record recordBuffer = new SCLKType1Record();

    double parallelTime = parallelTimeConverter.convertToUniformTime(tdb);

    int index = parallelTable.indexLastLessThanOrEqualTo(parallelTime);

    if (index == -1) {
      throw new SCLKEvaluationException(getID(), "The supplied TDB: " + tdb
          + " precedes the first entry in the SCLK " + "coefficient table: "
          + parallelTimeConverter.convertToTDB(recordTable.get(0, recordBuffer).getParallelTime()));
    }

    recordTable.get(index, recordBuffer);

    double encodedSCLK = recordBuffer.extrapolateToEncodedSCLK(parallelTime);

    if (encodedSCLK > convertibleSclkRange.getEnd()) {
      throw new SCLKEvaluationException(getID(),
          "The derived encoded SCLK: " + encodedSCLK + " associated with TDB: " + tdb
              + " exceeds the maximum value allowed by the " + "underlying SCLK kernel: "
              + convertibleSclkRange.getEnd());
    }

    return encodedSCLK;
  }

  /**
   * Simple inner class that adapts the record table supplied to the constructor, which should
   * perform searches over the encoded SCLK time, to perform searches over the parallel time
   * provided by the record.
   */
  private class ParallelTimeSearchTable extends AbstractGaugedRetrievable<SCLKType1Record> {

    @Override
    public double getGauge(int index) {
      SCLKType1Record recordBuffer = new SCLKType1Record();
      recordTable.get(index, recordBuffer);
      return recordBuffer.getParallelTime();
    }

    @Override
    public int size() {
      return recordTable.size();
    }

    @Override
    public SCLKType1Record get(int index, SCLKType1Record buffer) {
      return recordTable.get(index, buffer);
    }

  }

}
