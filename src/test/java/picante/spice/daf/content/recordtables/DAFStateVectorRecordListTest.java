package picante.spice.daf.content.recordtables;

import static picante.junit.AssertTools.assertComponentEquals;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import com.google.common.collect.Lists;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.mechanics.StateVector;

public class DAFStateVectorRecordListTest {

  private static final double TOLERANCE_TIGHT = 1.0E-14;

  private final List<StateVector> svs = Lists.newArrayList();

  private ArrayDAFSegment segment;
  private DAFStateVectorRecordList recordList;

  private ArrayDAFSegment shiftSegment;
  private DAFStateVectorRecordList shiftRecordList;

  @Before
  public void setUp() throws Exception {

    int length = 5;

    svs.add(new StateVector(new UnwritableVectorIJK(3.1, 5.1, 8.3),
        new UnwritableVectorIJK(0.1, 0.2, 0.3)));
    svs.add(new StateVector(new UnwritableVectorIJK(3.1, 5.1, 8.3),
        new UnwritableVectorIJK(0.1, 0.2, 0.3)));
    svs.add(new StateVector(new UnwritableVectorIJK(3.1, 5.1, 8.3),
        new UnwritableVectorIJK(0.1, 0.2, 0.3)));
    svs.add(new StateVector(new UnwritableVectorIJK(3.1, 5.1, 8.3),
        new UnwritableVectorIJK(0.1, 0.2, 0.3)));
    svs.add(new StateVector(new UnwritableVectorIJK(3.1, 5.1, 8.3),
        new UnwritableVectorIJK(0.1, 0.2, 0.3)));

    double[] data = new double[length * 6];
    for (int i = 0; i < length; i++) {

      data[i * 6 + 0] = svs.get(i).getPosition().getI();
      data[i * 6 + 1] = svs.get(i).getPosition().getJ();
      data[i * 6 + 2] = svs.get(i).getPosition().getK();
      data[i * 6 + 3] = svs.get(i).getVelocity().getI();
      data[i * 6 + 4] = svs.get(i).getVelocity().getJ();
      data[i * 6 + 5] = svs.get(i).getVelocity().getK();

    }

    segment = new ArrayDAFSegment(data);
    recordList = new DAFStateVectorRecordList(segment, svs.size(), 0);

    double[] shiftData = new double[length * 6 + 100];
    System.arraycopy(data, 0, shiftData, 37, length * 6);

    shiftSegment = new ArrayDAFSegment(shiftData);
    shiftRecordList = new DAFStateVectorRecordList(shiftSegment, svs.size(), 37);
  }

  @Test
  public void testObtainRecord() {

    StateVector buffer = new StateVector();

    svs.get(0).getPosition();
    recordList.obtainRecord(0, buffer);
    assertComponentEquals(svs.get(0).getPosition(), buffer.getPosition(), TOLERANCE_TIGHT);
    assertComponentEquals(svs.get(0).getVelocity(), buffer.getVelocity(), TOLERANCE_TIGHT);

    svs.get(1).getPosition();
    recordList.obtainRecord(1, buffer);
    assertComponentEquals(svs.get(1).getPosition(), buffer.getPosition(), TOLERANCE_TIGHT);
    assertComponentEquals(svs.get(1).getVelocity(), buffer.getVelocity(), TOLERANCE_TIGHT);

    svs.get(2).getPosition();
    recordList.obtainRecord(2, buffer);
    assertComponentEquals(svs.get(2).getPosition(), buffer.getPosition(), TOLERANCE_TIGHT);
    assertComponentEquals(svs.get(2).getVelocity(), buffer.getVelocity(), TOLERANCE_TIGHT);

    svs.get(3).getPosition();
    recordList.obtainRecord(3, buffer);
    assertComponentEquals(svs.get(3).getPosition(), buffer.getPosition(), TOLERANCE_TIGHT);
    assertComponentEquals(svs.get(3).getVelocity(), buffer.getVelocity(), TOLERANCE_TIGHT);

    svs.get(4).getPosition();
    recordList.obtainRecord(4, buffer);
    assertComponentEquals(svs.get(4).getPosition(), buffer.getPosition(), TOLERANCE_TIGHT);
    assertComponentEquals(svs.get(4).getVelocity(), buffer.getVelocity(), TOLERANCE_TIGHT);

    svs.get(0).getPosition();
    shiftRecordList.obtainRecord(0, buffer);
    assertComponentEquals(svs.get(0).getPosition(), buffer.getPosition(), TOLERANCE_TIGHT);
    assertComponentEquals(svs.get(0).getVelocity(), buffer.getVelocity(), TOLERANCE_TIGHT);

    svs.get(1).getPosition();
    shiftRecordList.obtainRecord(1, buffer);
    assertComponentEquals(svs.get(1).getPosition(), buffer.getPosition(), TOLERANCE_TIGHT);
    assertComponentEquals(svs.get(1).getVelocity(), buffer.getVelocity(), TOLERANCE_TIGHT);

    svs.get(2).getPosition();
    shiftRecordList.obtainRecord(2, buffer);
    assertComponentEquals(svs.get(2).getPosition(), buffer.getPosition(), TOLERANCE_TIGHT);
    assertComponentEquals(svs.get(2).getVelocity(), buffer.getVelocity(), TOLERANCE_TIGHT);

    svs.get(3).getPosition();
    shiftRecordList.obtainRecord(3, buffer);
    assertComponentEquals(svs.get(3).getPosition(), buffer.getPosition(), TOLERANCE_TIGHT);
    assertComponentEquals(svs.get(3).getVelocity(), buffer.getVelocity(), TOLERANCE_TIGHT);

    svs.get(4).getPosition();
    shiftRecordList.obtainRecord(4, buffer);
    assertComponentEquals(svs.get(4).getPosition(), buffer.getPosition(), TOLERANCE_TIGHT);
    assertComponentEquals(svs.get(4).getVelocity(), buffer.getVelocity(), TOLERANCE_TIGHT);

  }

}
