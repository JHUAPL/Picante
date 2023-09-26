package picante.spice.daf.content;

import java.util.List;
import picante.spice.daf.DAFSegment;
import picante.spice.daf.content.recordtables.DAFChebyshevTripletRecordList;
import picante.spice.kernel.pck.BinaryPCKInstantiationException;
import picante.spice.kernel.pck.PCKSegment;
import picante.spice.kernel.pck.PCKType2;
import picante.spice.kernel.pck.PCKType2Record;

public interface PCKSegmentFactory
    extends DAFContentFactory<PCKSegment, BinaryPCKInstantiationException> {

  static final int PCK_BODY_INDEX = 0;

  static final int PCK_REFERENCE_INDEX = 1;

  static final int PCK_TYPE_INDEX = 2;

  static final int PCK_START_INDEX = 0;

  static final int PCK_FINISH_INDEX = 1;

  public static final PCKSegmentFactory DISREGARDING = new PCKSegmentFactory() {

    @Override
    public void createAndAdd(DAFSegment segment, List<PCKSegment> list)
        throws BinaryPCKInstantiationException {

      double[] values = new double[4];
      int records;
      int recordSize;

      try {
        switch (segment.getIntComponent(PCK_TYPE_INDEX)) {
          case 2:
            segment.get(segment.getLength() - 4, values, 0, 4);
            recordSize = (int) values[2];
            records = (int) values[3];

            list.add(new PCKType2(segment.getName(), segment.getIntComponent(PCK_BODY_INDEX),
                segment.getIntComponent(PCK_REFERENCE_INDEX),
                segment.getDoubleComponent(PCK_START_INDEX),
                segment.getDoubleComponent(PCK_FINISH_INDEX), values[0], values[1],
                new DAFChebyshevTripletRecordList<PCKType2Record>(segment, records, 0,
                    recordSize)));

          default:
            return;
        }
      } catch (Exception e) {
        throw new BinaryPCKInstantiationException(e);
      }

    }
  };

  public static final PCKSegmentFactory VALIDATING = new PCKSegmentFactory() {

    @Override
    public void createAndAdd(DAFSegment segment, List<PCKSegment> list)
        throws BinaryPCKInstantiationException {

      int priorSize = list.size();
      DISREGARDING.createAndAdd(segment, list);
      if (list.size() != priorSize + 1) {
        throw new BinaryPCKInstantiationException(
            "Unsupported PCK segment encountered: " + segment.getName() + ".");
      }

    }
  };

}
