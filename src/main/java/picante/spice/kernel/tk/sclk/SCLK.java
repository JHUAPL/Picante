package picante.spice.kernel.tk.sclk;

import com.google.common.base.Preconditions;
import picante.designpatterns.Writable;

/**
 * Primitive class used to capture an SCLK readout. It is not tied to any specific clock, and more
 * or less functions as a parsed version of the SCLK string in the SPICE library. The primitive
 * value used internally is a long, as this should provide sufficient storage to capture NAIF's SCLK
 * string for type 1 clocks.
 * <p>
 * No error checking is performed on any of the setters, as this is low-level class designed purely
 * to capture the parsed clock fields. There is a slight risk with this approach, as NAIF's generic
 * type mechanism for SCLK pushes everything but the partition definition mechanism off onto the
 * specific clock type.
 * </p>
 * <p>
 * Another deviation from the NAIF standard is the fact that you must specify a partition when
 * utilizing this class. If you do not, the default constructor assigns the value of 1. This is a
 * low level class, and it seemed better to leave the handling of partitions to some higher level
 * piece of code.
 * </p>
 * <p>
 * While no error checking is done on the set methods of this class, save for negative clock counts
 * which are disallowed, there are some unenforced restrictions that will ultimately result in
 * errors when supplied to a type 1 converter:
 * </p>
 * <ul>
 * <li>The specified partition must be strictly positive to be valid. The specific acceptable range
 * is defined by the SCLK kernel for which this is intended to be associated.</li>
 * <li>While negative values would be allowed if a "negative" offset is specified in the SCLK
 * kernel, it is likely this is a problem that the appropriate converter will ultimately catch.</li>
 * </ul>
 */
public class SCLK implements Writable.ImplementationInterface<SCLK, SCLK> {

  /**
   * Delimiter used in the {@link SCLK#toString()} method to separate the partition from the clock
   * fields.
   * 
   */
  public static final char PARTITION_DELIMITER = '/';

  /**
   * Delimiter used in the {@link SCLK#toString()} method to separate the individual fields of the
   * clock.
   */
  public static final char DEFAULT_FIELD_DELIMITER = ':';

  /**
   * Field capturing the strictly positive partition counter.
   */
  private int partition;

  /**
   * Integer array capturing the individual field values, from most significant (0 index) to least
   * significant.
   */
  private long[] fields;

  /**
   * The number of actual fields utilized in the fields array. This allows the fields array to only
   * change when an expansion is requested.
   */
  private int numFields;

  /**
   * Creates a default SCLK reading of "1/0".
   */
  public SCLK() {
    partition = 1;
    fields = new long[2];
    numFields = 1;
  }

  /**
   * Creates an SCLK reading from the supplied partition and list of fields.
   * 
   * @param partition the partition of the SCLK
   * @param fields the list of fields, from most significant to least
   */
  public SCLK(int partition, long... fields) {
    this.partition = partition;
    this.fields = new long[fields.length];
    System.arraycopy(fields, 0, this.fields, 0, fields.length);
    this.numFields = fields.length;
  }

  /**
   * Copy constructor.
   * 
   * @param sclk the value to copy.
   */
  public SCLK(SCLK sclk) {
    this(sclk.partition, sclk.fields);
  }

  /**
   * @return the partition in which the SCLK reading resides
   */
  public int getPartition() {
    return partition;
  }

  /**
   * Retrieves a specific field of the SCLK
   * 
   * @param index the index of the field to retrieve
   * 
   * @return the desired field
   * 
   * @throws IndexOutOfBoundsException if index lies outside the range of 0,
   *         {@link SCLK#getNumberOfFields()}-1.
   */
  public long getField(int index) {
    return fields[Preconditions.checkPositionIndex(index, numFields)];
  }

  /**
   * Retrieves the number of fields currently in use by SCLK.
   * 
   * @return the number of fields
   */
  public int getNumberOfFields() {
    return numFields;
  }

  /**
   * Retrieves all of the fields from the most significant (index 0) through a specified number. If
   * you request more fields than SCLK currently contains it will only assign the first
   * {@link SCLK#getNumberOfFields()} values in the output buffer, leaving the others untouched.
   * 
   * @param numberOfFields the number of fields to retrieve from the most significant field
   * @param buffer an integer array to capture the desired field values
   * 
   * @return a reference to buffer for convenience
   * 
   * @throws IndexOutOfBoundsException if buffer is not large enough to contain the requested number
   *         of fields.
   */
  public long[] getFields(int numberOfFields, long[] buffer) {
    System.arraycopy(this.fields, 0, buffer, 0, Math.min(numberOfFields, numFields));
    return buffer;
  }

  /**
   * Sets the partition for the SCLK reading
   * 
   * @param partition the partition
   */
  public void setPartition(int partition) {
    this.partition = partition;
  }

  /**
   * Sets the fields of the clock to the supplied list
   * 
   * @param fields a varargs list that can take an <code>int[]</code> or list of integers defining
   *        the fields from most significant to least significant
   */
  public void setFields(long... fields) {

    /*
     * Expand the integer array capturing the fields to accommodate the new field value, if
     * appropriate.
     */
    if (fields.length > this.fields.length) {
      this.fields = new long[fields.length];
    }
    System.arraycopy(fields, 0, this.fields, 0, fields.length);
    numFields = fields.length;
  }

  /**
   * Sets the SCLK to the supplied values in one method call. This is analogous to invoking
   * {@link SCLK#setPartition(int)} followed by {@link SCLK#setFields(int...)}.
   * 
   * @param partition the partition
   * @param fields a list or <code>int[]</code> of fields from most significant to least
   */
  public void set(int partition, long... fields) {
    setPartition(partition);
    setFields(fields);
  }

  /**
   * {@inheritDoc}
   * <p>
   * Displays the clock string with the default partition ( {@value #PARTITION_DELIMITER}) and field
   * delimiter ( {@value #DEFAULT_FIELD_DELIMITER).
   * </p>
   */
  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder().append(partition).append(PARTITION_DELIMITER);
    for (int i = 0; i < numFields - 1; i++) {
      builder.append(fields[i]).append(DEFAULT_FIELD_DELIMITER);
    }
    builder.append(fields[numFields - 1]);
    return builder.toString();
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;

    /*
     * Normally we would just utilize Arrays.hashCode(fields) here, but this doesn't work because we
     * may not be using ALL of the elements present in the fields array.
     */
    for (int i = 0; i < numFields; i++) {
      result = 31 * result + (int) (fields[i] ^ (fields[i] >>> 32));
    }

    result = prime + result;
    result = prime * result + numFields;
    result = prime * result + partition;
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    SCLK other = (SCLK) obj;
    if (numFields != other.numFields) {
      return false;
    }
    if (partition != other.partition) {
      return false;
    }

    /*
     * Normally we would just utilize Arrays.equals(fields, other.fields) here, but this doesn't
     * work properly as we may only be comparing a subset of the values present in either array.
     */
    for (int i = 0; i < numFields; i++) {
      if (fields[i] != other.fields[i]) {
        return false;
      }
    }
    return true;

  }

  /**
   * Sets the value of the current SCLK to match that of the supplied SCLK.
   */
  @Override
  public SCLK setTo(SCLK sclk) {
    set(sclk.getPartition(), sclk.fields);
    return this;
  }

}
