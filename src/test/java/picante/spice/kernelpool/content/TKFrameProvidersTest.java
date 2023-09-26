package picante.spice.kernelpool.content;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.fail;
import static picante.junit.AssertTools.assertRotationAngleEquals;
import static picante.spice.kernelpool.content.TKFrameProviders.ANGLES;
import static picante.spice.kernelpool.content.TKFrameProviders.MATRIX;
import static picante.spice.kernelpool.content.TKFrameProviders.QUATERNION;
import static picante.spice.kernelpool.content.TKFrameProviders.getFrameKeyword;
import static picante.spice.kernelpool.content.TKFrameProviders.getProvider;
import static picante.spice.kernelpool.content.TKFrameProviders.getRelativeFrameCode;
import java.util.Arrays;
import java.util.Map;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import com.google.common.collect.Maps;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.rotations.AxisAndAngle;
import picante.mechanics.rotations.EulerAngles;
import picante.mechanics.rotations.Quaternion;
import picante.spice.kernel.tk.fk.FKInstantiationException;
import picante.spice.kernelpool.BasicKernelPool;

public class TKFrameProvidersTest {

  private static final double TOLERANCE = 1E-14;

  @Before
  public void setUp() throws Exception {}

  @Test(expected = KernelPoolValidationException.class)
  public void testGetMatrixMatrixLeftHandedRotationException() throws Exception {
    BasicKernelPool pool = new BasicKernelPool();
    pool.addDoubles("TKFRAME_1000_MATRIX",
        Arrays.asList(1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, -1.0));
    MATRIX.getMatrix(1000, "NOTPRESENT", new KernelPoolValidatingRetriever(pool));
  }

  /*
   * This test is invalid, since SPICE accepts matrices in the FK system via the TKFRAME_*_MATRIX
   * specification that are this far out of a rotation.
   */
  // @Test(expected = KernelPoolValidationException.class)
  // public void testGetMatrixMatrixInvalidRotationException() throws Exception {
  // BasicKernelPool pool = new BasicKernelPool();
  // pool.addDoubles("TKFRAME_1000_MATRIX",
  // Arrays.asList(1.0, 0.0, 0.0, 0.0, 1000.0, 0.0, 0.0, 0.0, 1.0));
  // MATRIX.getMatrix(1000, "NOTPRESENT", new KernelPoolValidatingRetriever(pool));
  // }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetMatrixMatrixLengthException() throws Exception {
    BasicKernelPool pool = new BasicKernelPool();
    pool.addDoubles("TKFRAME_1000_MATRIX", Arrays.asList(1.0, 2.0, 3.0, 4.0));
    MATRIX.getMatrix(1000, "NOTPRESENT", new KernelPoolValidatingRetriever(pool));
  }

  @Test
  public void testGetMatrixMatrix() throws Exception {

    RotationMatrixIJK rotation =
        new AxisAndAngle(VectorIJK.K, Math.toRadians(10.0)).getRotation(new RotationMatrixIJK());

    BasicKernelPool pool = new BasicKernelPool();
    pool.addDoubles("TKFRAME_NAME_MATRIX",
        Arrays.asList(rotation.getII(), rotation.getJI(), rotation.getKI(), rotation.getIJ(),
            rotation.getJJ(), rotation.getKJ(), rotation.getIK(), rotation.getJK(),
            rotation.getKK()));

    RotationMatrixIJK result =
        MATRIX.getMatrix(1000, "NAME", new KernelPoolValidatingRetriever(pool));
    assertRotationAngleEquals(rotation, result, TOLERANCE);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetMatrixQuaternionLengthException() throws Exception {
    BasicKernelPool pool = new BasicKernelPool();
    pool.addDoubles("TKFRAME_NAME_Q", Arrays.asList(1.0, 2.0, 3.0, 4.0, 5.0));

    QUATERNION.getMatrix(1000, "NAME", new KernelPoolValidatingRetriever(pool));
  }

  @Test
  public void testGetMatrixQuaternion() throws Exception {

    BasicKernelPool pool = new BasicKernelPool();
    pool.addDoubles("TKFRAME_1000_Q", Arrays.asList(1.0, 2.0, 3.0, 4.0));

    RotationMatrixIJK result =
        QUATERNION.getMatrix(1000, "NOTPRESENT", new KernelPoolValidatingRetriever(pool));
    assertRotationAngleEquals(
        new Quaternion(1.0, 2.0, 3.0, 4.0).getRotation(new RotationMatrixIJK()), result, TOLERANCE);
  }

  @Test
  public void testGetMatrixAnglesAAA() throws Exception {

    BasicKernelPool pool = new BasicKernelPool();

    pool.addDoubles("TKFRAME_1000_ANGLES", Arrays.asList(10.0, 20.0, 30.0));
    pool.addIntegers("TKFRAME_1000_AXES", Arrays.asList(1, 1, 1));

    RotationMatrixIJK result =
        ANGLES.getMatrix(1000, "NOTPRESENT", new KernelPoolValidatingRetriever(pool));
    assertRotationAngleEquals(new EulerAngles.JIJ(0, 60, 0).getRotation(new RotationMatrixIJK()),
        result, TOLERANCE);

  }

  @Test
  public void testGetMatrixAnglesAAB() throws Exception {

    BasicKernelPool pool = new BasicKernelPool();

    pool.addDoubles("TKFRAME_1000_ANGLES", Arrays.asList(10.0, 20.0, 30.0));
    pool.addIntegers("TKFRAME_1000_AXES", Arrays.asList(1, 1, 2));

    RotationMatrixIJK result =
        ANGLES.getMatrix(1000, "NOTPRESENT", new KernelPoolValidatingRetriever(pool));
    assertRotationAngleEquals(new EulerAngles.JIJ(0, 30, 30).getRotation(new RotationMatrixIJK()),
        result, TOLERANCE);


  }

  @Test
  public void testGetMatrixAnglesABB() throws Exception {
    BasicKernelPool pool = new BasicKernelPool();

    pool.addDoubles("TKFRAME_1000_ANGLES", Arrays.asList(10.0, 20.0, 30.0));
    pool.addIntegers("TKFRAME_1000_AXES", Arrays.asList(2, 1, 1));

    RotationMatrixIJK result =
        ANGLES.getMatrix(1000, "NOTPRESENT", new KernelPoolValidatingRetriever(pool));
    assertRotationAngleEquals(new EulerAngles.JIJ(10, 50, 0).getRotation(new RotationMatrixIJK()),
        result, TOLERANCE);

  }

  @Ignore
  @Test(expected = KernelPoolValidationException.class)
  public void testGetMatrixAnglesAxesLengthException() throws Exception {

  }

  @Ignore
  @Test(expected = KernelPoolValidationException.class)
  public void testGetMatrixAnglesAnglesLengthException() throws Exception {

  }

  @Ignore
  @Test(expected = KernelPoolValidationException.class)
  public void testGetMatrixAnglesInvalidAxisValueException() throws Exception {

  }

  @Ignore
  @Test(expected = KernelPoolValidationException.class)
  public void testGetMatrixAnglesInvalidUnitsException() throws Exception {

  }

  @Test
  public void testGetFrameKeyword() {

    BasicKernelPool pool = new BasicKernelPool();
    pool.addStrings("TKFRAME_1000_STRING", Arrays.asList("TEST"));
    pool.addStrings("TKFRAME_-1000_STRING", Arrays.asList("TEST"));
    pool.addStrings("TKFRAME_miXeD_STRING", Arrays.asList("TEST"));
    pool.addStrings("TKFRAME_UPPER_STRING", Arrays.asList("TEST"));

    pool.addDoubles("TKFRAME_1000_DOUBLE", Arrays.asList(0.0));
    pool.addDoubles("TKFRAME_-1000_DOUBLE", Arrays.asList(0.0));
    pool.addDoubles("TKFRAME_miXeD_DOUBLE", Arrays.asList(0.0));
    pool.addDoubles("TKFRAME_UPPER_DOUBLE", Arrays.asList(0.0));

    KernelPoolValidatingRetriever retriever = new KernelPoolValidatingRetriever(pool);

    /*
     * First determine that names are preferred, case sensitively, and then validate the number
     * fallbacks.
     */
    assertEquals("TKFRAME_miXeD_STRING",
        getFrameKeyword("TKFRAME_%s_STRING", 1000, "miXeD", retriever));
    assertEquals("TKFRAME_miXeD_STRING",
        getFrameKeyword("TKFRAME_%s_STRING", -1000, "miXeD", retriever));
    assertEquals("TKFRAME_miXeD_STRING",
        getFrameKeyword("TKFRAME_%s_STRING", 0, "miXeD", retriever));

    assertEquals("TKFRAME_UPPER_STRING",
        getFrameKeyword("TKFRAME_%s_STRING", 1000, "UPPER", retriever));
    assertEquals("TKFRAME_UPPER_STRING",
        getFrameKeyword("TKFRAME_%s_STRING", -1000, "UPPER", retriever));
    assertEquals("TKFRAME_UPPER_STRING",
        getFrameKeyword("TKFRAME_%s_STRING", 0, "UPPER", retriever));

    assertEquals("TKFRAME_1000_STRING",
        getFrameKeyword("TKFRAME_%s_STRING", 1000, "NOTPRESENT", retriever));
    assertEquals("TKFRAME_-1000_STRING",
        getFrameKeyword("TKFRAME_%s_STRING", -1000, "NOTPRESENT", retriever));

    assertEquals("TKFRAME_miXeD_DOUBLE",
        getFrameKeyword("TKFRAME_%s_DOUBLE", 1000, "miXeD", retriever));
    assertEquals("TKFRAME_miXeD_DOUBLE",
        getFrameKeyword("TKFRAME_%s_DOUBLE", -1000, "miXeD", retriever));
    assertEquals("TKFRAME_miXeD_DOUBLE",
        getFrameKeyword("TKFRAME_%s_DOUBLE", 0, "miXeD", retriever));

    assertEquals("TKFRAME_UPPER_DOUBLE",
        getFrameKeyword("TKFRAME_%s_DOUBLE", 1000, "UPPER", retriever));
    assertEquals("TKFRAME_UPPER_DOUBLE",
        getFrameKeyword("TKFRAME_%s_DOUBLE", -1000, "UPPER", retriever));
    assertEquals("TKFRAME_UPPER_DOUBLE",
        getFrameKeyword("TKFRAME_%s_DOUBLE", 0, "UPPER", retriever));

    assertEquals("TKFRAME_1000_DOUBLE",
        getFrameKeyword("TKFRAME_%s_DOUBLE", 1000, "NOTPRESENT", retriever));
    assertEquals("TKFRAME_-1000_DOUBLE",
        getFrameKeyword("TKFRAME_%s_DOUBLE", -1000, "NOTPRESENT", retriever));

  }

  @Ignore
  @Test
  public void testCreateFunction() {
    fail("Not yet implemented");
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetRelativeFrameCodeTypeException() throws Exception {

    BasicKernelPool pool = new BasicKernelPool();
    pool.addDoubles("TKFRAME_NAME_RELATIVE", Arrays.asList(10.0));
    pool.addStrings("TKFRAME_1000_RELATIVE", Arrays.asList("ALTERNATE_FRAME"));

    KernelPoolValidatingRetriever retriever = new KernelPoolValidatingRetriever(pool);

    Map<String, Integer> frameCodeMap = Maps.newHashMap();
    frameCodeMap.put("FRAME", 100);
    frameCodeMap.put("ALTERNATE_FRAME", -100);

    getRelativeFrameCode(1000, "NAME", retriever, frameCodeMap);
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetRelativeFrameCodeTypeOverlayException() throws Exception {

    BasicKernelPool pool = new BasicKernelPool();
    pool.addDoubles("TKFRAME_1000_RELATIVE", Arrays.asList(10.0));

    KernelPoolValidatingRetriever retriever = new KernelPoolValidatingRetriever(pool);

    Map<String, Integer> frameCodeMap = Maps.newHashMap();
    frameCodeMap.put("FRAME", 100);
    frameCodeMap.put("ALTERNATE_FRAME", -100);

    getRelativeFrameCode(1000, "NOTPRESENT", retriever, frameCodeMap);
  }

  @Test(expected = FKInstantiationException.class)
  public void testGetRelativeFrameCodeMissingRelativeFrameDefinitionException() throws Exception {

    BasicKernelPool pool = new BasicKernelPool();
    pool.addStrings("TKFRAME_NAME_RELATIVE", Arrays.asList("FRAME"));
    pool.addStrings("TKFRAME_1000_RELATIVE", Arrays.asList("ALTERNATE_FRAME"));
    pool.addStrings("TKFRAME_LC_NAME_RELATIVE", Arrays.asList("frame"));
    pool.addStrings("TKFRAME_-1000_RELATIVE", Arrays.asList("alternate_Frame"));

    KernelPoolValidatingRetriever retriever = new KernelPoolValidatingRetriever(pool);

    Map<String, Integer> frameCodeMap = Maps.newHashMap();
    frameCodeMap.put("FRAME", 100);

    getRelativeFrameCode(1000, "NOTPRESENT", retriever, frameCodeMap);
  }

  @Test
  public void testGetRelativeFrameCode() throws Exception {

    BasicKernelPool pool = new BasicKernelPool();
    pool.addStrings("TKFRAME_NAME_RELATIVE", Arrays.asList("FRAME"));
    pool.addStrings("TKFRAME_1000_RELATIVE", Arrays.asList("ALTERNATE_FRAME"));
    pool.addStrings("TKFRAME_LC_NAME_RELATIVE", Arrays.asList("frame"));
    pool.addStrings("TKFRAME_-1000_RELATIVE", Arrays.asList("alternate_Frame"));

    KernelPoolValidatingRetriever retriever = new KernelPoolValidatingRetriever(pool);

    Map<String, Integer> frameCodeMap = Maps.newHashMap();
    frameCodeMap.put("FRAME", 100);
    frameCodeMap.put("ALTERNATE_FRAME", -100);

    assertEquals(100, getRelativeFrameCode(1000, "NAME", retriever, frameCodeMap));
    assertEquals(-100, getRelativeFrameCode(1000, "NOTPRESENT", retriever, frameCodeMap));
    assertEquals(100, getRelativeFrameCode(1000, "LC_NAME", retriever, frameCodeMap));
    assertEquals(-100, getRelativeFrameCode(-1000, "NOTPRESENT", retriever, frameCodeMap));

  }

  @Test(expected = FKInstantiationException.class)
  public void testGetProviderUnknownSpecificationException() throws Exception {
    BasicKernelPool pool = new BasicKernelPool();
    pool.addStrings("TKFRAME_-1000_SPEC", Arrays.asList("NOT_SUPPORTED"));
    getProvider(-1000, "NOTPRESENT", new KernelPoolValidatingRetriever(pool));
  }

  @Test(expected = KernelPoolValidationException.class)
  public void testGetProviderBadTypeException() throws Exception {
    BasicKernelPool pool = new BasicKernelPool();
    pool.addDoubles("TKFRAME_NAME_SPEC", Arrays.asList(100.0));
    pool.addStrings("TKFRAME_1000_SPEC", Arrays.asList("ANGLES"));
    getProvider(1000, "NAME", new KernelPoolValidatingRetriever(pool));
  }

  @Test
  public void testGetProvider() throws Exception {
    BasicKernelPool pool = new BasicKernelPool();
    pool.addStrings("TKFRAME_ANGLES_SPEC", Arrays.asList("ANGLES"));
    pool.addStrings("TKFRAME_100_SPEC", Arrays.asList("QUATERNION"));
    pool.addStrings("TKFRAME_MATRIX_SPEC", Arrays.asList("MATRIX"));
    pool.addStrings("TKFRAME_200_SPEC", Arrays.asList("angles"));
    pool.addStrings("TKFRAME_300_SPEC", Arrays.asList("quaternion"));
    pool.addStrings("TKFRAME_400_SPEC", Arrays.asList("matrix"));

    KernelPoolValidatingRetriever retriever = new KernelPoolValidatingRetriever(pool);

    assertSame(ANGLES, getProvider(100, "ANGLES", retriever));
    assertSame(QUATERNION, getProvider(100, "NOTPRESENT", retriever));
    assertSame(MATRIX, getProvider(100, "MATRIX", retriever));

    assertSame(ANGLES, getProvider(200, "NOTPRESENT", retriever));
    assertSame(QUATERNION, getProvider(300, "NOTPRESENT", retriever));
    assertSame(MATRIX, getProvider(400, "NOTPRESENT", retriever));

  }

}
