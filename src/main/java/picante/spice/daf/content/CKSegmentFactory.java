package picante.spice.daf.content;

import java.math.RoundingMode;
import java.util.List;

import com.google.common.math.DoubleMath;
import picante.spice.daf.DAFSegment;
import picante.spice.daf.content.recordtables.DAFCKType2RecordTable;
import picante.spice.daf.content.recordtables.DAFQuaternionAVRecordTable;
import picante.spice.daf.content.recordtables.DAFQuaternionRecordTable;
import picante.spice.daf.content.recordtables.DAFTimeListTable;
import picante.spice.kernel.ck.CKInstantiationException;
import picante.spice.kernel.ck.CKSegment;
import picante.spice.kernel.ck.CKType2;
import picante.spice.kernel.ck.CKType3;
import picante.spice.kernel.ck.CKType3WithAV;

public interface CKSegmentFactory extends DAFContentFactory<CKSegment, CKInstantiationException> {

  /**
   * Index into the DAF segment integer component metadata containing the ID code of the instrument
   * frame.
   */
  static final int CK_INSTRUMENT_INDEX = 0;

  /**
   * Index into the DAF segment integer component metadata containing the ID code of the reference
   * frame.
   */
  static final int CK_REFERENCE_INDEX = 1;

  /**
   * Index into the DAF segment integer component metadata containing the type ID code of the
   * segment.
   */
  static final int CK_TYPE_INDEX = 2;

  /**
   * Index into the DAF segment integer component metadata containing the flag that indicates
   * whether angular velocity data is present.
   */
  static final int CK_AVFLAG_INDEX = 3;

  /**
   * Index into the DAF segment double component metadata indicating the encoded SCLK time of the
   * start of the segment's applicability.
   */
  static final int CK_START_INDEX = 0;

  /**
   * Index into the DAF segment double component metadata indicating the encoded SCLK time of the
   * end of the segment's applicability.
   */
  static final int CK_FINISH_INDEX = 1;

  /**
   * This implementation of the <code>CKSegmentFactory</code> disregards any DAF segments supplied
   * to it that it does not support. The resultant CK created using this factory will only contain
   * supported segments in the order they occur.
   * <p>
   * <b>Caution:</b> This could alter the intent of the attitude producer, as segments implemented
   * in the file may mask other segments implemented in the file, for example. Care should be
   * exercised when using this factory.
   * </p>
   */
  public static final CKSegmentFactory DISREGARDING = new CKSegmentFactory() {

    @Override
    public void createAndAdd(DAFSegment segment, List<CKSegment> list)
        throws CKInstantiationException {
      try {
        /*
         * First check to see if the segment in question has angular velocity data.
         */
        if (segment.getIntComponent(CK_AVFLAG_INDEX) == 0) {

          switch (segment.getIntComponent(CK_TYPE_INDEX)) {

            case 2:
              /*
               * This is a type 2 segment. Compute the number of records and the locations of the
               * various tables of interest in the segment.
               */
              int length = segment.getLength();

              /*
               * The following formula is converted directly from CKR02. NINT in FORTRAN is
               * equivalent to RoundingMode.HALF_UP.
               */
              int records =
                  DoubleMath.roundToInt((100.0 * (length) + 1.0) / 1001.0, RoundingMode.HALF_UP);

              list.add(new CKType2(segment.getName(), segment.getIntComponent(CK_INSTRUMENT_INDEX),
                  segment.getIntComponent(CK_REFERENCE_INDEX),
                  segment.getDoubleComponent(CK_START_INDEX),
                  segment.getDoubleComponent(CK_FINISH_INDEX),
                  new DAFCKType2RecordTable(segment, records),
                  new DAFTimeListTable(segment, records, 8 * records),
                  new DAFTimeListTable(segment, records, 9 * records), false));
              return;

            case 3:

              /*
               * This is a type 3 segment. Compute the number of records and the locations of the
               * various tables of interest in the segment.
               */
              double[] values = new double[2];
              segment.get(segment.getLength() - 2, values, 0, 2);
              records = (int) values[1];
              int intervals = (int) values[0];

              list.add(new CKType3(segment.getName(), segment.getIntComponent(CK_INSTRUMENT_INDEX),
                  segment.getIntComponent(CK_REFERENCE_INDEX),
                  segment.getDoubleComponent(CK_START_INDEX),
                  segment.getDoubleComponent(CK_FINISH_INDEX),
                  new DAFQuaternionRecordTable(segment,
                      new DAFTimeListTable(segment, records, 4 * records), 0),
                  new DAFTimeListTable(segment, intervals, 5 * records + (records - 1) / 100)));
              return;

            default:
              return;

          }

        } else {

          switch (segment.getIntComponent(CK_TYPE_INDEX)) {

            case 2:

              /*
               * This is a type 2 segment. Compute the number of records and the locations of the
               * various tables of interest in the segment.
               */
              int length = segment.getLength();

              /*
               * The following formula is converted directly from CKR02. NINT in FORTRAN is
               * equivalent to RoundingMode.HALF_UP.
               */
              int records =
                  DoubleMath.roundToInt((100.0 * (length) + 1.0) / 1001.0, RoundingMode.HALF_UP);

              list.add(new CKType2(segment.getName(), segment.getIntComponent(CK_INSTRUMENT_INDEX),
                  segment.getIntComponent(CK_REFERENCE_INDEX),
                  segment.getDoubleComponent(CK_START_INDEX),
                  segment.getDoubleComponent(CK_FINISH_INDEX),
                  new DAFCKType2RecordTable(segment, records),
                  new DAFTimeListTable(segment, records, 8 * records),
                  new DAFTimeListTable(segment, records, 9 * records), true));
              return;

            case 3:

              /*
               * This is a type 3 segment with angular velocity. Compute the number of records and
               * the locations of the various tables of interest in the segment.
               */
              double[] values = new double[2];
              segment.get(segment.getLength() - 2, values, 0, 2);
              records = (int) values[1];
              int intervals = (int) values[0];

              list.add(
                  new CKType3WithAV(segment.getName(), segment.getIntComponent(CK_INSTRUMENT_INDEX),
                      segment.getIntComponent(CK_REFERENCE_INDEX),
                      segment.getDoubleComponent(CK_START_INDEX),
                      segment.getDoubleComponent(CK_FINISH_INDEX),
                      new DAFQuaternionAVRecordTable(segment,
                          new DAFTimeListTable(segment, records, 7 * records), 0),
                      new DAFTimeListTable(segment, intervals, 8 * records + (records - 1) / 100)));
              return;

            default:
              return;
          }

        }
      } catch (Exception e) {
        throw new CKInstantiationException("Attempt to create C-kernel segment[" + segment.getName()
            + "] at position: " + list.size() + " has failed.", e);
      }
    }
  };

  /**
   * This implementation of the <code>CKSegmentFactory</code> throws an exception whenever an
   * unsupported segment is encountered.
   */
  public static final CKSegmentFactory VALIDATING = new CKSegmentFactory() {

    @Override
    public void createAndAdd(DAFSegment segment, List<CKSegment> list)
        throws CKInstantiationException {

      int priorSize = list.size();
      DISREGARDING.createAndAdd(segment, list);

      if (list.size() != priorSize + 1) {
        throw new CKInstantiationException(
            "Unsupported CK segment encountered: " + segment.getName() + ".");
      }

    }
  };

}
