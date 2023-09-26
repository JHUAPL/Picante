package picante.spice.daf.content.recordtables;

import static org.junit.Assert.assertEquals;
import static picante.junit.AssertTools.assertComponentEquals;
import java.util.ArrayList;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.rotations.AxisAndAngle;
import picante.mechanics.rotations.Quaternion;
import picante.mechanics.rotations.WrapperWithRate;

public class DAFQuaternionAVRecordTableTest {

  private static final double TOLERANCE_TIGHT = 1.0E-14;

  private List<WrapperWithRate<Quaternion>> wrs;
  private ArrayDAFSegment segment;
  private ArrayDAFSegment timeSegment;
  private DAFTimeListTable timeTable;
  private DAFQuaternionAVRecordTable table;
  private ArrayDAFSegment shiftSegment;
  private DAFQuaternionAVRecordTable shiftTable;

  @Before
  public void setUp() throws Exception {

    wrs = new ArrayList<WrapperWithRate<Quaternion>>(4);

    wrs.add(
        new WrapperWithRate<Quaternion>(new Quaternion(-6.5548774000000E-01, -1.5282560000000E-02,
            6.9443685000000E-01, 2.9641142000000E-01), new VectorIJK(0.04, 0.03, 0.2)));

    wrs.add(new WrapperWithRate<Quaternion>(new Quaternion(-6.5596575000000E-01,
        -1.5723490000000E-02, 6.9372869000000E-01, 2.9698852000000E-01), new VectorIJK(-1, 1, 0)));

    AxisAndAngle aa = new AxisAndAngle(0.5, 0.05, 0.5, Math.toRadians(27.0));
    RotationMatrixIJK r = new RotationMatrixIJK();
    aa.getRotation(r);

    wrs.add(new WrapperWithRate<Quaternion>(new Quaternion(r), new VectorIJK(1, -1, 0)));

    aa.setTo(1.0, 0.5, 0.24, Math.toRadians(42.0));
    aa.getRotation(r);

    wrs.add(new WrapperWithRate<Quaternion>(new Quaternion(r), new VectorIJK(-0.5, 0.02, 1.0)));

    double[] data = new double[4 * 7];
    VectorIJK v = new VectorIJK();
    for (int i = 0; i < 4; i++) {
      WrapperWithRate<Quaternion> wrapper = wrs.get(i);
      data[i * 7 + 0] = wrapper.getRotation().getScalar();
      wrapper.getRotation().getVector(v);
      data[i * 7 + 1] = v.getI();
      data[i * 7 + 2] = v.getJ();
      data[i * 7 + 3] = v.getK();
      VectorIJK av = wrapper.getRate();
      data[i * 7 + 4] = av.getI();
      data[i * 7 + 5] = av.getJ();
      data[i * 7 + 6] = av.getK();
    }

    segment = new ArrayDAFSegment(data);
    timeSegment = ArrayDAFSegment.createSequentialSegment(-314, -311);

    timeTable = new DAFTimeListTable(timeSegment, 4, 0);
    table = new DAFQuaternionAVRecordTable(segment, timeTable, 0);

    double[] shiftData = new double[7 * 4 + 200];
    System.arraycopy(data, 0, shiftData, 102, 28);

    shiftSegment = new ArrayDAFSegment(shiftData);
    shiftTable = new DAFQuaternionAVRecordTable(shiftSegment, timeTable, 102);
  }

  @Test
  public void testObtainTime() {

    assertEquals(-314.0, table.obtainTime(0), 0.0);
    assertEquals(-313.0, table.obtainTime(1), 0.0);
    assertEquals(-312.0, table.obtainTime(2), 0.0);
    assertEquals(-311.0, table.obtainTime(3), 0.0);

    assertEquals(-314.0, shiftTable.obtainTime(0), 0.0);
    assertEquals(-313.0, shiftTable.obtainTime(1), 0.0);
    assertEquals(-312.0, shiftTable.obtainTime(2), 0.0);
    assertEquals(-311.0, shiftTable.obtainTime(3), 0.0);

  }

  @Test
  public void testObtainRecordIntWrapperWithRateOfQuaternion() {

    RotationMatrixIJK em = new RotationMatrixIJK();
    RotationMatrixIJK m = new RotationMatrixIJK();

    WrapperWithRate<Quaternion> wrapper = new WrapperWithRate<Quaternion>(new Quaternion());
    WrapperWithRate<Quaternion> eWrapper;

    eWrapper = wrs.get(0);
    table.obtainRecord(0, wrapper);
    assertComponentEquals(eWrapper.getRate(), wrapper.getRate(), TOLERANCE_TIGHT);
    eWrapper.getRotation(em);
    wrapper.getRotation(m);
    assertComponentEquals(em, m, TOLERANCE_TIGHT);

    eWrapper = wrs.get(1);
    table.obtainRecord(1, wrapper);
    assertComponentEquals(eWrapper.getRate(), wrapper.getRate(), TOLERANCE_TIGHT);
    eWrapper.getRotation(em);
    wrapper.getRotation(m);
    assertComponentEquals(em, m, TOLERANCE_TIGHT);

    eWrapper = wrs.get(2);
    table.obtainRecord(2, wrapper);
    assertComponentEquals(eWrapper.getRate(), wrapper.getRate(), TOLERANCE_TIGHT);
    eWrapper.getRotation(em);
    wrapper.getRotation(m);
    assertComponentEquals(em, m, TOLERANCE_TIGHT);

    eWrapper = wrs.get(3);
    table.obtainRecord(3, wrapper);
    assertComponentEquals(eWrapper.getRate(), wrapper.getRate(), TOLERANCE_TIGHT);
    eWrapper.getRotation(em);
    wrapper.getRotation(m);
    assertComponentEquals(em, m, TOLERANCE_TIGHT);

    eWrapper = wrs.get(0);
    shiftTable.obtainRecord(0, wrapper);
    assertComponentEquals(eWrapper.getRate(), wrapper.getRate(), TOLERANCE_TIGHT);
    eWrapper.getRotation(em);
    wrapper.getRotation(m);
    assertComponentEquals(em, m, TOLERANCE_TIGHT);

    eWrapper = wrs.get(1);
    shiftTable.obtainRecord(1, wrapper);
    assertComponentEquals(eWrapper.getRate(), wrapper.getRate(), TOLERANCE_TIGHT);
    eWrapper.getRotation(em);
    wrapper.getRotation(m);
    assertComponentEquals(em, m, TOLERANCE_TIGHT);

    eWrapper = wrs.get(2);
    shiftTable.obtainRecord(2, wrapper);
    assertComponentEquals(eWrapper.getRate(), wrapper.getRate(), TOLERANCE_TIGHT);
    eWrapper.getRotation(em);
    wrapper.getRotation(m);
    assertComponentEquals(em, m, TOLERANCE_TIGHT);

    eWrapper = wrs.get(3);
    shiftTable.obtainRecord(3, wrapper);
    assertComponentEquals(eWrapper.getRate(), wrapper.getRate(), TOLERANCE_TIGHT);
    eWrapper.getRotation(em);
    wrapper.getRotation(m);
    assertComponentEquals(em, m, TOLERANCE_TIGHT);



  }

}
