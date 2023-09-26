package picante.spice.daf.content.recordtables;

import picante.spice.daf.DAFSegment;

/**
 * This utility class provides a simple double precision array retrieval system for DAF. It is
 * useful in implementing DAF record table and lists, where the records are fixed length and
 * contiguous.
 */
class DAFRecordRetriever {

  /**
   * The number of doubles in a record
   */
  private final int recordSize;

  /**
   * The buffer used to retrieve information from the DAF. A reference to this memory is returned by
   * the {@link DAFRecordRetriever#readRecord(int)} method.
   */
  private final double[] buffer;

  /**
   * The segment containing the record block of interest.
   */
  private final DAFSegment segment;

  /**
   * The index of the first double in the record block.
   */
  private final int startIndex;

  /**
   * Constructs a record retriever.
   * 
   * @param segment the DAF segment from which records are to be retrieved
   * @param length the number of blocks of size, recordSize, in the segment
   * @param startIndex the index first double precision number of the contiguous record block
   * @param recordSize the length, in doubles, of a record
   */
  DAFRecordRetriever(DAFSegment segment, int length, int startIndex, int recordSize) {

    this.startIndex = startIndex;
    this.recordSize = recordSize;
    buffer = new double[recordSize];
    this.segment = segment;

    /*
     * Check to see if the specified startIndex, coupled with length and recordSize will result in
     * stepping off the end of the DAF segment.
     */
    if (startIndex + length * recordSize > segment.getLength()) {
      throw new IllegalArgumentException("Attempt to construct DAF based record table has failed."
          + " The combination of the starting index: " + startIndex
          + " with the specified record size: " + recordSize + " and length: " + length
          + " results in a indices past the end of the supplied DAF segment: " + segment.getName()
          + " with length: " + segment.getLength());
    }

  }

  /**
   * Implements a record retrieval mechanism from the DAF segment interface. No error checking is
   * performed, as this method assumes that the caller has already validated the index and record
   * size entries to be compatible with the underlying data.
   * 
   * @param index index of the record of interest
   * 
   * @return a double array of at least length recordSize, containing the elements of the record
   *         starting at index 0.
   */
  double[] readRecord(int index) {
    segment.get(startIndex + recordSize * index, buffer, 0, recordSize);
    return buffer;
  }

}
