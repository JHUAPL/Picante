package picante.spice.daf.content.recordtables;

import static org.junit.Assert.assertEquals;
import static picante.junit.AssertTools.assertComponentEquals;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.rotations.AxisAndAngle;
import picante.mechanics.rotations.Quaternion;

public class DAFQuaternionRecordTableTest {

  private static final double TOLERANCE_TIGHT = 1.0E-14;

  private Quaternion[] qs;
  private ArrayDAFSegment segment;
  private ArrayDAFSegment timeSegment;
  private DAFTimeListTable timeTable;
  private DAFQuaternionRecordTable table;
  private ArrayDAFSegment shiftSegment;
  private DAFQuaternionRecordTable shiftTable;

  @Before
  public void setUp() throws Exception {

    qs = new Quaternion[5];
    qs[0] = new Quaternion(-6.5548774000000E-01, -1.5282560000000E-02, 6.9443685000000E-01,
        2.9641142000000E-01);
    qs[1] = new Quaternion(-6.5596575000000E-01, -1.5723490000000E-02, 6.9372869000000E-01,
        2.9698852000000E-01);

    qs[2] = new Quaternion();
    AxisAndAngle aa = new AxisAndAngle(0.5, 0.5, 0.5, Math.toRadians(12.0));
    RotationMatrixIJK r = new RotationMatrixIJK();
    qs[2].setTo(aa.getRotation(r));

    aa.setTo(-0.5, 0, 0.5, Math.toRadians(-25.));
    qs[3] = new Quaternion();
    qs[3].setTo(aa.getRotation(r));

    qs[4] = new Quaternion();
    aa.setTo(0, 0, 1, Math.toRadians(135.0));
    qs[4].setTo(aa.getRotation(r));

    double[] data = new double[5 * 4];
    VectorIJK v = new VectorIJK();
    for (int i = 0; i < 5; i++) {
      data[i * 4 + 0] = qs[i].getScalar();
      qs[i].getVector(v);
      data[i * 4 + 1] = v.getI();
      data[i * 4 + 2] = v.getJ();
      data[i * 4 + 3] = v.getK();
    }

    segment = new ArrayDAFSegment(data);
    timeSegment = ArrayDAFSegment.createSequentialSegment(500, 504);

    timeTable = new DAFTimeListTable(timeSegment, 5, 0);
    table = new DAFQuaternionRecordTable(segment, timeTable, 0);

    double[] shiftData = new double[5 * 4 + 100];
    System.arraycopy(data, 0, shiftData, 37, 20);

    shiftSegment = new ArrayDAFSegment(shiftData);
    shiftTable = new DAFQuaternionRecordTable(shiftSegment, timeTable, 37);
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

    Quaternion q = new Quaternion();
    RotationMatrixIJK em = new RotationMatrixIJK();
    RotationMatrixIJK m = new RotationMatrixIJK();

    qs[0].getRotation(em);
    table.obtainRecord(0, q).getRotation(m);
    assertComponentEquals(em, m, TOLERANCE_TIGHT);

    qs[1].getRotation(em);
    table.obtainRecord(1, q).getRotation(m);
    assertComponentEquals(em, m, TOLERANCE_TIGHT);

    qs[2].getRotation(em);
    table.obtainRecord(2, q).getRotation(m);
    assertComponentEquals(em, m, TOLERANCE_TIGHT);

    qs[3].getRotation(em);
    table.obtainRecord(3, q).getRotation(m);
    assertComponentEquals(em, m, TOLERANCE_TIGHT);

    qs[4].getRotation(em);
    table.obtainRecord(4, q).getRotation(m);
    assertComponentEquals(em, m, TOLERANCE_TIGHT);

    qs[0].getRotation(em);
    shiftTable.obtainRecord(0, q).getRotation(m);
    assertComponentEquals(em, m, TOLERANCE_TIGHT);

    qs[1].getRotation(em);
    shiftTable.obtainRecord(1, q).getRotation(m);
    assertComponentEquals(em, m, TOLERANCE_TIGHT);

    qs[2].getRotation(em);
    shiftTable.obtainRecord(2, q).getRotation(m);
    assertComponentEquals(em, m, TOLERANCE_TIGHT);

    qs[3].getRotation(em);
    shiftTable.obtainRecord(3, q).getRotation(m);
    assertComponentEquals(em, m, TOLERANCE_TIGHT);

    qs[4].getRotation(em);
    shiftTable.obtainRecord(4, q).getRotation(m);
    assertComponentEquals(em, m, TOLERANCE_TIGHT);

  }
}
