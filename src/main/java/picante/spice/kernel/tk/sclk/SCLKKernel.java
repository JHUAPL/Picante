package picante.spice.kernel.tk.sclk;

import com.google.common.annotations.VisibleForTesting;
import picante.data.list.GaugedRetrievableLLE;
import picante.math.intervals.Interval;
import picante.math.intervals.UnwritableInterval;
import picante.spice.Utilities;

/**
 * Parent class of all implementations of the various types of supported SCLK kernels.
 */
public abstract class SCLKKernel implements EncodedSCLKConverter {

  /**
   * Integer code capturing the clock ID. Note: this is the negative of the ID used to build the
   * keyword names in the kernel pool.
   */
  private final int clockID;

  /**
   * Converter used to reduce SCLK elements to a count of encoded SCLK.
   */
  private final TickConverter tickConverter;

  /**
   * Table capturing the SCLK partition definitions.
   */
  private final GaugedRetrievableLLE<Interval> partitionTable;

  /**
   * This interval captures the range of valid SCLKs for conversion to encoded SCLK. It starts at
   * 0.0 and ends at the last representable encoded SCLK allowed by the partition table.
   */
  final UnwritableInterval validSclkRange;

  /**
   * This interval captures the range of all encoded SCLKs that are supported by the SCLK. This
   * range may be a subset of the validSclkRange defined above.
   */
  final UnwritableInterval validEncodedSclkRange;

  /**
   * Buffer used to prevent unnecessary memory allocation during execution of various methods on
   * this class.
   */
  final Interval intervalBuffer = new Interval();

  /**
   * Package private constructor used to create this abstract parent class.
   * 
   * @param clockID the ID code of the clock in question
   * @param tickConverter the converter used to process SCLK to ticks independent of partition
   * @param partitionTable the partition table defining the partitions for this clock
   * @param validEncodedSclkRange the supported range of encoded SCLKs for encoded SCLK to TDB
   *        conversion
   */
  SCLKKernel(int clockID, TickConverter tickConverter,
      GaugedRetrievableLLE<Interval> partitionTable, UnwritableInterval validEncodedSclkRange) {

    this.clockID = clockID;
    this.tickConverter = tickConverter;
    this.partitionTable = partitionTable;

    this.validEncodedSclkRange = new UnwritableInterval(validEncodedSclkRange);
    this.validSclkRange = new UnwritableInterval(0.0, computeMaxEncodedSCLK(partitionTable));
  }

  /**
   * Computes the maximum encoded SCLK value supported by the partition table.
   * 
   * @param partitionTable the table in question
   * 
   * @return the maximum encoded SCLK allowed by the partition table for conversion
   */
  @VisibleForTesting
  static double computeMaxEncodedSCLK(GaugedRetrievableLLE<Interval> partitionTable) {
    Interval buffer = partitionTable.get(partitionTable.size() - 1, new Interval());
    return partitionTable.getGauge(partitionTable.size() - 1) - buffer.getBegin() + buffer.getEnd();
  }

  /**
   * Abstract method defining the integer ID code of the type.
   * 
   * @return the type code
   */
  public abstract int getType();

  /**
   * Retrieves the ID of the SCLK
   * 
   * @return the, typically negative, ID code of the SCLK
   */
  public int getID() {
    return clockID;
  }

  /**
   * Retrieves a reference to the internally held partition table
   * 
   * @return the table defining the clock partitions. The gauge is the encodedSCLK value of the
   *         start of each partition. The interval itself contains the tick values associated with
   *         the start and stop of each clock reading on the partition boundary.
   */
  public GaugedRetrievableLLE<Interval> getPartitionTable() {
    return partitionTable;
  }

  /**
   * Retrieves the number of partitions defined for this SCLK
   * 
   * @return the partition count
   */
  public int getNumberOfPartitions() {
    return partitionTable.size();
  }

  /**
   * Determines in a non-exception generating way if the supplied SCLK is valid for this clock.
   * 
   * @param sclk the SCLK to validate
   * 
   * @return true, if sclk is valid and will convert to encoded SCLK without error; false otherwise
   */
  public boolean isValidSCLK(SCLK sclk) {

    /*
     * Check if the partition number is valid.
     */
    if ((sclk.getPartition() < 1) || (sclk.getPartition() > partitionTable.size())) {
      return false;
    }

    /*
     * Next check that the clock reading itself is valid.
     */
    if (!tickConverter.isValidClock(sclk)) {
      return false;
    }

    /*
     * Lastly verify that the clock fields lie within the appropriate range for the partition.
     */
    double ticks = tickConverter.convertToTicks(sclk);

    partitionTable.get(sclk.getPartition() - 1, intervalBuffer);

    if (!intervalBuffer.closedContains(ticks)) {
      return false;
    }

    return true;

  }

  /**
   * Converts the supplied encoded SCLK to SCLK
   * 
   * @param encodedSCLK the encoded SCLK to convert from
   * @param buffer a buffer to receive SCLK
   * 
   * @return a reference to buffer for convenience
   */
  public SCLK convertToSclk(double encodedSCLK, SCLK buffer) {

    /*
     * Round the supplied encodedSCLK into an appropriate double. This value should ultimately be
     * integral.
     */
    double encodedSCLKForLookup = Utilities.round(encodedSCLK);

    /*
     * Check to see if the value rounded to the nearest integer is in the range of valid SCLKs.
     */
    if (!validSclkRange.closedContains(encodedSCLKForLookup)) {
      throw new SCLKEvaluationException(clockID,
          "Conversion from encoded SCLK: " + encodedSCLK
              + " to SCLK has failed.  It's nearest integral neighbor: " + encodedSCLKForLookup
              + " lies outside the range of supported encoded SCLKs: " + validSclkRange);
    }

    /*
     * Locate the partition that is last less than or equal to this value.
     */
    int index = partitionTable.indexLastLessThanOrEqualTo(encodedSCLKForLookup);

    partitionTable.get(index, intervalBuffer);

    /*
     * Convert the encoded SCLK to the appropriate field breakdown.
     */
    encodedSCLKForLookup += intervalBuffer.getBegin();
    encodedSCLKForLookup -= partitionTable.getGauge(index);

    tickConverter.convertToSCLK(encodedSCLKForLookup, buffer);
    buffer.setPartition(index + 1);

    return buffer;
  }

  /**
   * Converts the supplied SCLK to encoded SCLK.
   * 
   * @param sclk the SCLK to convert
   * 
   * @return the encoded SCLK value.
   */
  public double convertToEncodedSclk(SCLK sclk) {

    /*
     * First verify that the number of partitions is appropriate.
     */
    int partition = sclk.getPartition();

    if (partition > partitionTable.size()) {
      throw new SCLKEvaluationException(clockID,
          "Requested SCLK: " + sclk + " specifies partition: " + partition
              + " which exceeds the maximum partition + (" + partitionTable.size()
              + ") supported by the clock.");
    }

    /*
     * Convert the supplied clock reading to ticks, and verify that this reading lies in the
     * acceptable range.
     */
    double ticks = tickConverter.convertToTicks(sclk);
    partitionTable.get(partition - 1, intervalBuffer);

    if (!intervalBuffer.closedContains(ticks)) {
      throw new SCLKEvaluationException(clockID,
          "Requested SCLK: " + sclk + " specifies a clock count that lies outside the range"
              + " of the requested partition.");
    }

    return partitionTable.getGauge(partition - 1) + ticks - intervalBuffer.getBegin();
  }

  /**
   * Convenience method that returns the largest encoded SCLK that is supported for clock
   * conversions.
   * 
   * @return the largest supported encoded SCLK value
   */
  public double getMaxEncodedSCLK() {
    return validEncodedSclkRange.getEnd();
  }

  /**
   * Convenience method that converts the supplied SCLK into TDB seconds past J2000.0
   * 
   * @param sclk the SCLK value to convert to TDB
   * 
   * @return TDB seconds past J2000.0
   * 
   * @throws SCLKEvaluationException if a problem occurs in the conversion process
   */
  public double convertFromSclkToTDB(SCLK sclk) {
    return convertToTDB(convertToEncodedSclk(sclk));
  }

  /**
   * Convenience method that converts the supplied TDB seconds past J2000.0 into an SCLK
   * 
   * @param tdb the TDB value to convert to SCLK
   * @param buffer a buffer to receive the resultant SCLK
   * 
   * @return a reference to buffer for convenience
   * 
   * @throws SCLKEvaluationException if a problem occurs in the conversion process
   */
  public SCLK convertFromTDBToSclk(double tdb, SCLK buffer) {
    return convertToSclk(convertToEncodedSclk(tdb), buffer);
  }

  /**
   * Protected method to allow subclasses to gain access to the tick converter they install on this
   * abstract parent.
   * 
   * @return
   */
  protected TickConverter getTickConverter() {
    return tickConverter;
  }

}
