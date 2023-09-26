package picante.spice.provided;

import static org.junit.Assert.assertEquals;
import static picante.junit.AssertTools.assertComponentEquals;
import static picante.junit.AssertTools.assertEquivalentMatrix;
import static picante.junit.AssertTools.assertEquivalentStateTransform;
import static picante.spice.provided.InertialFrames.B1950;
import static picante.spice.provided.InertialFrames.DE102;
import static picante.spice.provided.InertialFrames.DE108;
import static picante.spice.provided.InertialFrames.DE111;
import static picante.spice.provided.InertialFrames.DE114;
import static picante.spice.provided.InertialFrames.DE118;
import static picante.spice.provided.InertialFrames.DE122;
import static picante.spice.provided.InertialFrames.DE125;
import static picante.spice.provided.InertialFrames.DE130;
import static picante.spice.provided.InertialFrames.DE140;
import static picante.spice.provided.InertialFrames.DE142;
import static picante.spice.provided.InertialFrames.DE143;
import static picante.spice.provided.InertialFrames.DE200;
import static picante.spice.provided.InertialFrames.DE202;
import static picante.spice.provided.InertialFrames.DE96;
import static picante.spice.provided.InertialFrames.ECLIPB1950;
import static picante.spice.provided.InertialFrames.ECLIPJ2000;
import static picante.spice.provided.InertialFrames.FK4;
import static picante.spice.provided.InertialFrames.GALACTIC;
import static picante.spice.provided.InertialFrames.MARSIAU;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.Coverage;
import picante.mechanics.StateTransform;

public class InertialFramesTest {

  private static final double TIGHT_TOLERANCE = 1.0E-16;

  private RotationMatrixIJK matrix;
  private StateTransform transform;

  @Before
  public void setUp() {
    matrix = new RotationMatrixIJK();
    transform = new StateTransform();
  }

  private void testInertialFrame(InertialFrames frame, int spiceIdCode,
      RotationMatrixIJK expected) {
    assertEquals(spiceIdCode, frame.getFromID());
    assertEquals(1, frame.getToID());
    assertEquals(Coverage.ALL_TIME, frame.getCoverage());
    assertEquivalentMatrix(expected, frame.getTransform(0.0, matrix));
    assertEquivalentStateTransform(new StateTransform(expected, MatrixIJK.ZEROS),
        frame.getStateTransform(0.0, transform));
  }

  private void testInertialFrame(InertialFrames frame, int spiceIdCode, RotationMatrixIJK expected,
      double tolerance) {
    assertEquals(spiceIdCode, frame.getFromID());
    assertEquals(1, frame.getToID());
    assertEquals(Coverage.ALL_TIME, frame.getCoverage());
    assertComponentEquals(expected, frame.getTransform(0.0, matrix), tolerance);
    assertComponentEquals(new StateTransform(expected, MatrixIJK.ZEROS),
        frame.getStateTransform(0.0, transform), tolerance);
  }

  @Test
  public void testB1950() {
    testInertialFrame(B1950, 2,
        new RotationMatrixIJK(0.9999257079523629, 0.011178938126427692, 0.0048590038414544285,
            -0.011178938137770135, 0.9999375133499887, -0.000027157926258510777,
            -0.00485900381535927, -0.00002716259471424704, 0.9999881946023742));
    assertEquals("B1950", B1950.getFrameName());
  }

  @Test
  public void testFK4() {
    testInertialFrame(FK4, 3,
        new RotationMatrixIJK(0.9999256794956877, 0.011181483239171794, 0.004859003772314385,
            -0.01118148322046629, 0.9999374848933135, -0.000027170293744002025,
            -0.00485900381535927, -0.00002716259471424704, 0.9999881946023742));
    assertEquals("FK4", FK4.getFrameName());
  }

  @Test
  public void testDE118() {
    testInertialFrame(DE118, 4,
        new RotationMatrixIJK(0.9999256791406158, 0.011181514992482716, 0.004859003771451581,
            -0.011181514973402328, 0.9999374845382416, -0.00002717044804310561,
            -0.00485900381535927, -0.00002716259471424704, 0.9999881946023742));
    assertEquals("DE-118", DE118.getFrameName());
  }

  @Test
  public void testDE96() {
    testInertialFrame(DE96, 5,
        new RotationMatrixIJK(0.999925685691664, 0.011180929131774818, 0.00485900378736984,
            -0.011180929119611181, 0.9999374910892899, -0.0000271676011657472, -0.00485900381535927,
            -0.00002716259471424704, 0.9999881946023742));
    assertEquals("DE-96", DE96.getFrameName());
  }

  @Test
  public void testDE102() {
    testInertialFrame(DE102, 6,
        new RotationMatrixIJK(0.9999257005867707, 0.011179596947047827, 0.004859003823560054,
            -0.011179596950612145, 0.9999375059843965, -0.00002716112767048625,
            -0.00485900381535927, -0.00002716259471424704, 0.9999881946023742));
    assertEquals("DE-102", DE102.getFrameName());
  }

  @Test
  public void testDE108() {
    testInertialFrame(DE108, 7,
        new RotationMatrixIJK(0.9999256820706058, 0.011181252967069356, 0.004859003778571207,
            -0.011181252951082476, 0.9999374874682316, -0.00002716917478103625,
            -0.00485900381535927, -0.00002716259471424704, 0.9999881946023742));
    assertEquals("DE-108", DE108.getFrameName());
  }

  @Test
  public void testDE111() {
    testInertialFrame(DE111, 8,
        new RotationMatrixIJK(0.9999256760804512, 0.011181788652696218, 0.0048590037640154635,
            -0.011181788630384961, 0.999937481478077, -0.000027171777842249146,
            -0.00485900381535927, -0.00002716259471424704, 0.9999881946023742));
    assertEquals("DE-111", DE111.getFrameName());
  }

  @Test
  public void testDE114() {
    testInertialFrame(DE114, 9,
        new RotationMatrixIJK(0.9999256779832373, 0.01118161849373274, 0.004859003768639204,
            -0.011181618473430402, 0.9999374833808631, -0.000027170950987511774,
            -0.00485900381535927, -0.00002716259471424704, 0.9999881946023742));
    assertEquals("DE-114", DE114.getFrameName());
  }

  @Test
  public void testDE122() {
    testInertialFrame(DE122, 10,
        new RotationMatrixIJK(0.9999256791379054, 0.011181515234874402, 0.004859003771444995,
            -0.011181515215791154, 0.9999374845355312, -0.000027170449220961366,
            -0.00485900381535927, -0.00002716259471424704, 0.9999881946023742));
    assertEquals("DE-122", DE122.getFrameName());
  }

  @Test
  public void testDE125() {
    testInertialFrame(DE125, 11,
        new RotationMatrixIJK(0.9999256767635061, 0.011181727569991418, 0.004859003765675284,
            -0.011181727548401311, 0.9999374821611318, -0.000027171481022599924,
            -0.00485900381535927, -0.00002716259471424704, 0.9999881946023742));
    assertEquals("DE-125", DE125.getFrameName());
  }

  @Test
  public void testDE130() {
    testInertialFrame(DE130, 12,
        new RotationMatrixIJK(0.9999256795119504, 0.011181481784821676, 0.004859003772353902,
            -0.011181481766133343, 0.9999374849095762, -0.000027170286676867506,
            -0.00485900381535927, -0.00002716259471424704, 0.9999881946023742));
    assertEquals("DE-130", DE130.getFrameName());
  }

  @Test
  public void testGALACTIC() {
    testInertialFrame(GALACTIC, 13,
        new RotationMatrixIJK(-0.054875539395742516, -0.8734371047275961, -0.4838349917700252,
            0.49410945362774383, -0.44482959429757496, 0.7469822486998919, -0.8676661356833737,
            -0.19807638961301985, 0.4559837945214199));
    assertEquals("GALACTIC", GALACTIC.getFrameName());
  }

  @Test
  public void testDE200() {
    testInertialFrame(DE200, 14, new RotationMatrixIJK(RotationMatrixIJK.IDENTITY));
    assertEquals("DE-200", DE200.getFrameName());
  }

  @Test
  public void testDE202() {
    testInertialFrame(DE202, 15, new RotationMatrixIJK(RotationMatrixIJK.IDENTITY));
    assertEquals("DE-202", DE202.getFrameName());
  }

  @Test
  public void testMARSIAU() {

    /*
     * Due to the tiny (k,i)th element of the matrix, simply equivalence testing fails. Validate
     * against a tight tolerance instead.
     */
    testInertialFrame(MARSIAU, 16,
        new RotationMatrixIJK(0.673257747460025, 0.739407874914146, -3.694654858420616E-17,
            -0.5896308378262533, 0.536880310821634, 0.6034028562547383, 0.44616082366044196,
            -0.40624564781301037, 0.7974365135003686),
        TIGHT_TOLERANCE);
    assertEquals("MARSIAU", MARSIAU.getFrameName());
  }

  @Test
  public void testECLIPJ2000() {
    testInertialFrame(ECLIPJ2000, 17, new RotationMatrixIJK(1., 0., 0., 0., 0.9174820620691818,
        0.3977771559319137, 0., -0.3977771559319137, 0.9174820620691818));
    assertEquals("ECLIPJ2000", ECLIPJ2000.getFrameName());
  }

  @Test
  public void testECLIPB1950() {

    /*
     * Due to the relatively tiny (i,k)th element of the matrix, simply equivalence testing fails.
     * Validate against a tight tolerance instead.
     */
    testInertialFrame(ECLIPB1950, 18,
        new RotationMatrixIJK(0.9999257079523629, 0.011178938126427692, 0.0048590038414544285,
            -0.012189277138214924, 0.9173688178789828, 0.39785157220522016, -0.00000994050092035086,
            -0.3978812427417045, 0.9174369278459982),
        TIGHT_TOLERANCE);
    assertEquals("ECLIPB1950", ECLIPB1950.getFrameName());
  }

  @Test
  public void testDE140() {
    testInertialFrame(DE140, 19,
        new RotationMatrixIJK(0.9999256765384668, 0.01118177011980248, 0.004858952158380056,
            -0.011181770179728694, 0.9999374816848701, -0.000027154519585747306,
            -0.004858952020473538, -0.00002717918498144707, 0.9999881948535966));
    assertEquals("DE-140", DE140.getFrameName());
  }

  @Test
  public void testDE142() {
    testInertialFrame(DE142, 20,
        new RotationMatrixIJK(0.9999256765402605, 0.011181769732063588, 0.004858952681545991,
            -0.011181769790785997, 0.9999374816892125, -0.000027154769316986656,
            -0.004858952546409775, -0.000027178939228786992, 0.9999881948510477));

    assertEquals("DE-142", DE142.getFrameName());
  }

  @Test
  public void testDE143() {
    testInertialFrame(DE143, 21,
        new RotationMatrixIJK(0.999925676543585, 0.011181774307743057, 0.004858941467468586,
            -0.011181774330053015, 0.9999374816382502, -0.000027162211525057475,
            -0.004858941416127174, -0.0000271713942365573, 0.9999881949053349),
        TIGHT_TOLERANCE);

    assertEquals("DE-143", DE143.getFrameName());
  }
}
