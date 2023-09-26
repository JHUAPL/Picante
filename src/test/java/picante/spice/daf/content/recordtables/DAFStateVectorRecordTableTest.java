package picante.spice.daf.content.recordtables;

import static org.junit.Assert.assertEquals;
import static picante.junit.AssertTools.assertComponentEquals;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import com.google.common.collect.Lists;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.mechanics.StateVector;

public class DAFStateVectorRecordTableTest {

  private static final double TOLERANCE_TIGHT = 1.0E-14;

  private final List<StateVector> svs = Lists.newArrayList();

  private ArrayDAFSegment timeSegment;
  private DAFTimeListTable timeTable;

  private ArrayDAFSegment segment;
  private DAFStateVectorRecordTable table;
  private DAFStateVectorRecordList recordList;

  private ArrayDAFSegment shiftSegment;
  private DAFStateVectorRecordTable shiftTable;
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

    timeSegment = ArrayDAFSegment.createSequentialSegment(500, 504);
    timeTable = new DAFTimeListTable(timeSegment, length, 0);

    segment = new ArrayDAFSegment(data);
    recordList = new DAFStateVectorRecordList(segment, svs.size(), 0);
    table = new DAFStateVectorRecordTable(recordList, timeTable);

    double[] shiftData = new double[length * 6 + 100];
    System.arraycopy(data, 0, shiftData, 37, length * 6);

    shiftSegment = new ArrayDAFSegment(shiftData);
    shiftRecordList = new DAFStateVectorRecordList(shiftSegment, svs.size(), 37);
    shiftTable = new DAFStateVectorRecordTable(shiftRecordList, timeTable);
  }

  @Test
  public void testObtainTime() {

    assertEquals(500.0, table.obtainTime(0), 0.0);
    assertEquals(501.0, table.obtainTime(1), 0.0);
    assertEquals(502.0, table.obtainTime(2), 0.0);
    assertEquals(503.0, table.obtainTime(3), 0.0);
    assertEquals(504.0, table.obtainTime(4), 0.0);

    assertEquals(500.0, shiftTable.obtainTime(0), 0.0);
    assertEquals(501.0, shiftTable.obtainTime(1), 0.0);
    assertEquals(502.0, shiftTable.obtainTime(2), 0.0);
    assertEquals(503.0, shiftTable.obtainTime(3), 0.0);
    assertEquals(504.0, shiftTable.obtainTime(4), 0.0);

  }

  @Test
  public void testObtainRecordIntQuaternion() {

    StateVector buffer = new StateVector();

    for (int i = 0; i < svs.size(); i++) {
      svs.get(i).getPosition();
      table.obtainRecord(0, buffer);
      assertComponentEquals(svs.get(i).getPosition(), buffer.getPosition(), TOLERANCE_TIGHT);
      assertComponentEquals(svs.get(i).getVelocity(), buffer.getVelocity(), TOLERANCE_TIGHT);
    }

    for (int i = 0; i < svs.size(); i++) {
      svs.get(i).getPosition();
      shiftTable.obtainRecord(0, buffer);
      assertComponentEquals(svs.get(i).getPosition(), buffer.getPosition(), TOLERANCE_TIGHT);
      assertComponentEquals(svs.get(i).getVelocity(), buffer.getVelocity(), TOLERANCE_TIGHT);
    }

  }

}
