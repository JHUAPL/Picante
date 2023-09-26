package picante.spice.daf.content;

import java.util.List;
import picante.spice.daf.DAFSegment;
import picante.spice.daf.content.recordtables.DAFChebyshevTripletRecordList;
import picante.spice.daf.content.recordtables.DAFStateVectorRecordList;
import picante.spice.daf.content.recordtables.DAFStateVectorRecordTable;
import picante.spice.daf.content.recordtables.DAFTimeListTable;
import picante.spice.daf.content.recordtables.DAFType1SPKRecordTable;
import picante.spice.daf.content.recordtables.DAFType3SPKRecordList;
import picante.spice.kernel.spk.SPKInstantiationException;
import picante.spice.kernel.spk.SPKSegment;
import picante.spice.kernel.spk.SPKType1;
import picante.spice.kernel.spk.SPKType12;
import picante.spice.kernel.spk.SPKType13;
import picante.spice.kernel.spk.SPKType17;
import picante.spice.kernel.spk.SPKType2;
import picante.spice.kernel.spk.SPKType2Record;
import picante.spice.kernel.spk.SPKType3;
import picante.spice.kernel.spk.SPKType5;
import picante.spice.kernel.spk.SPKType8;
import picante.spice.kernel.spk.SPKType9;

/**
 * Interface describing the methods necessary to convert the contents of a DAF segment into an SPK
 * segment.
 * <p>
 * Two default implementations are provided:
 * <ul>
 * <li>{@link SPKSegmentFactory#DISREGARDING} disregards any unsupported segments and omits them
 * from the resultant list</li>
 * <li>{@link SPKSegmentFactory#VALIDATING} throws an exception if an unsupported segment is
 * encountered</li>
 * </ul>
 * </p>
 * <p>
 * In practice consumers of SPK content will not need to understand the details of this interface,
 * only which of the two factories they intend to use in their code.
 * </p>
 */
public interface SPKSegmentFactory
    extends DAFContentFactory<SPKSegment, SPKInstantiationException> {

  /**
   * Index into the DAF segment integer component metadata indicating the ID code of the target
   * body.
   */
  static final int SPK_TARGET_INDEX = 0;

  /**
   * Index into the DAF segment integer component metadata indicating the ID code of the observing
   * body.
   */
  static final int SPK_OBSERVER_INDEX = 1;

  /**
   * Index into the DAF segment integer component metadata indicating the ID code of the frame the
   * segment data is expressed in.
   */
  static final int SPK_FRAME_INDEX = 2;

  /**
   * Index into the DAF segment integer component metadata indicating the type ID code of the
   * segment.
   */
  static final int SPK_TYPE_INDEX = 3;

  /**
   * Index into the DAF segment double component metadata indicating the ephemeris time of the start
   * of the segment's applicability.
   */
  static final int SPK_START_INDEX = 0;

  /**
   * Index into the DAF segment double component metadata indicating the ephemeris time of the end
   * of the segment's applicability.
   */
  static final int SPK_FINISH_INDEX = 1;

  /**
   * This implementation of the <code>SPKSegmentFactory</code> disregards any DAF segments supplied
   * to it that it does not support. The resultant SPK created using this factory will only contain
   * supported segments in the order they occur.
   * <p>
   * <b>Caution:</b> This could alter the intent of the ephemeris producer, as segments implemented
   * in the file may mask other segments implemented in the file, for example. Care should be
   * exercised when using this factory.
   * </p>
   */
  public static final SPKSegmentFactory DISREGARDING = new SPKSegmentFactory() {

    @Override
    public void createAndAdd(DAFSegment segment, List<SPKSegment> list)
        throws SPKInstantiationException {

      /*
       * Array to retrieve metadata from the end of DAF segments that describe various attributes
       * necessary to instantiate the individual pieces that are supplied to the segment
       * constructors. Maintainers, expand as necessary to accommodate the length of any new types.
       */
      double[] values = new double[12];
      int degree;
      int records;
      int recordSize;
      int windowSizeMinus1;
      double initialEpoch;
      double intervalLength;

      try {

        switch (segment.getIntComponent(SPK_TYPE_INDEX)) {

          case 1:

            segment.get(segment.getLength() - 1, values, 0, 1);
            records = (int) values[0];

            list.add(new SPKType1(segment.getName(), segment.getIntComponent(SPK_TARGET_INDEX),
                segment.getIntComponent(SPK_OBSERVER_INDEX),
                segment.getIntComponent(SPK_FRAME_INDEX),
                segment.getDoubleComponent(SPK_START_INDEX),
                segment.getDoubleComponent(SPK_FINISH_INDEX), new DAFType1SPKRecordTable(segment,
                    new DAFTimeListTable(segment, records, 71 * records), 0)));
            return;

          case 2:

            segment.get(segment.getLength() - 4, values, 0, 4);
            // double initialEpoch = values[0];
            // double intervalLength = values[1];
            recordSize = (int) values[2];
            records = (int) values[3];

            list.add(new SPKType2(segment.getName(), segment.getIntComponent(SPK_TARGET_INDEX),
                segment.getIntComponent(SPK_OBSERVER_INDEX),
                segment.getIntComponent(SPK_FRAME_INDEX),
                segment.getDoubleComponent(SPK_START_INDEX),
                segment.getDoubleComponent(SPK_FINISH_INDEX), values[0], values[1],
                new DAFChebyshevTripletRecordList<SPKType2Record>(segment, records, 0,
                    recordSize)));
            return;

          case 3:

            segment.get(segment.getLength() - 4, values, 0, 4);
            // double initialEpoch = values[0];
            // double intervalLength = values[1];
            recordSize = (int) values[2];
            records = (int) values[3];

            list.add(new SPKType3(segment.getName(), segment.getIntComponent(SPK_TARGET_INDEX),
                segment.getIntComponent(SPK_OBSERVER_INDEX),
                segment.getIntComponent(SPK_FRAME_INDEX),
                segment.getDoubleComponent(SPK_START_INDEX),
                segment.getDoubleComponent(SPK_FINISH_INDEX), values[0], values[1],
                new DAFType3SPKRecordList(segment, records, 0, recordSize)));

            return;

          case 5:

            segment.get(segment.getLength() - 2, values, 0, 2);
            records = (int) values[1];

            list.add(new SPKType5(segment.getName(), segment.getIntComponent(SPK_TARGET_INDEX),
                segment.getIntComponent(SPK_OBSERVER_INDEX),
                segment.getIntComponent(SPK_FRAME_INDEX),
                segment.getDoubleComponent(SPK_START_INDEX),
                segment.getDoubleComponent(SPK_FINISH_INDEX),
                new DAFStateVectorRecordTable(new DAFStateVectorRecordList(segment, records, 0),
                    new DAFTimeListTable(segment, records, records * 6)),
                values[0]));

            return;

          case 8:

            segment.get(segment.getLength() - 4, values, 0, 4);
            initialEpoch = values[0];
            intervalLength = values[1];
            degree = (int) values[2];
            records = (int) values[3];

            list.add(new SPKType8(segment.getName(), segment.getIntComponent(SPK_TARGET_INDEX),
                segment.getIntComponent(SPK_OBSERVER_INDEX),
                segment.getIntComponent(SPK_FRAME_INDEX),
                segment.getDoubleComponent(SPK_START_INDEX),
                segment.getDoubleComponent(SPK_FINISH_INDEX),
                new DAFStateVectorRecordList(segment, records, 0), initialEpoch, intervalLength,
                degree + 1));

            return;

          case 9:

            segment.get(segment.getLength() - 2, values, 0, 2);
            // double initialEpoch = values[0];
            // double intervalLength = values[1];
            degree = (int) values[0];
            records = (int) values[1];

            list.add(new SPKType9(segment.getName(), segment.getIntComponent(SPK_TARGET_INDEX),
                segment.getIntComponent(SPK_OBSERVER_INDEX),
                segment.getIntComponent(SPK_FRAME_INDEX),
                segment.getDoubleComponent(SPK_START_INDEX),
                segment.getDoubleComponent(SPK_FINISH_INDEX),
                new DAFStateVectorRecordTable(new DAFStateVectorRecordList(segment, records, 0),
                    new DAFTimeListTable(segment, records, records * 6)),
                degree + 1));

            return;

          case 12:

            segment.get(segment.getLength() - 4, values, 0, 4);
            initialEpoch = values[0];
            intervalLength = values[1];
            windowSizeMinus1 = (int) values[2];
            records = (int) values[3];

            list.add(new SPKType12(segment.getName(), segment.getIntComponent(SPK_TARGET_INDEX),
                segment.getIntComponent(SPK_OBSERVER_INDEX),
                segment.getIntComponent(SPK_FRAME_INDEX),
                segment.getDoubleComponent(SPK_START_INDEX),
                segment.getDoubleComponent(SPK_FINISH_INDEX),
                new DAFStateVectorRecordList(segment, records, 0), initialEpoch, intervalLength,
                windowSizeMinus1 + 1));

            return;

          case 13:

            segment.get(segment.getLength() - 2, values, 0, 2);
            // double initialEpoch = values[0];
            // double intervalLength = values[1];
            windowSizeMinus1 = (int) values[0];
            records = (int) values[1];

            list.add(
                new SPKType13(segment.getName(), segment.getIntComponent(SPK_TARGET_INDEX),
                    segment.getIntComponent(SPK_OBSERVER_INDEX),
                    segment.getIntComponent(SPK_FRAME_INDEX),
                    segment.getDoubleComponent(SPK_START_INDEX),
                    segment.getDoubleComponent(SPK_FINISH_INDEX),
                    new DAFStateVectorRecordTable(new DAFStateVectorRecordList(segment, records, 0),
                        new DAFTimeListTable(segment, records, records * 6)),
                    windowSizeMinus1 + 1));

            return;

          case 17:

            if (segment.getLength() != 12) {
              throw new SPKInstantiationException(
                  "Invalid type 17 segment, record length differs from 12.");
            }
            segment.get(0, values, 0, 12);

            list.add(new SPKType17(segment.getName(), segment.getIntComponent(SPK_TARGET_INDEX),
                segment.getIntComponent(SPK_OBSERVER_INDEX),
                segment.getIntComponent(SPK_FRAME_INDEX),
                segment.getDoubleComponent(SPK_START_INDEX),
                segment.getDoubleComponent(SPK_FINISH_INDEX), values));

          default:
            return;

        }

      } catch (Exception e) {
        throw new SPKInstantiationException(e);
      }
    }

  };

  /**
   * This implementation of the <code>SPKSegmentFactory</code> throws an exception whenever an
   * unsupported segment is encountered.
   */
  public static final SPKSegmentFactory VALIDATING = new SPKSegmentFactory() {

    @Override
    public void createAndAdd(DAFSegment segment, List<SPKSegment> list)
        throws SPKInstantiationException {

      int priorSize = list.size();
      DISREGARDING.createAndAdd(segment, list);

      // If the segment type is not known, then the segment
      // won't actually get added, this is an easy way to check
      // if it worked without having to know about all the supported
      // types.
      if (list.size() != priorSize + 1) {
        throw new SPKInstantiationException(
            "Unsupported SPK segment encountered: " + segment.getName() + ".");
      }
    }

  };

}
