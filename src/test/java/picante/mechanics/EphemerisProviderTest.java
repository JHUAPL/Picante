package picante.mechanics;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static picante.junit.AssertTools.assertComponentRelativeEquality;
import static picante.junit.AssertTools.assertEquivalentStateVector;
import static picante.junit.AssertTools.assertEquivalentVector;
import static picante.mechanics.EphemerisTestCodes.E_AA;
import static picante.mechanics.EphemerisTestCodes.E_AB;
import static picante.mechanics.EphemerisTestCodes.E_AC;
import static picante.mechanics.EphemerisTestCodes.E_BA;
import static picante.mechanics.EphemerisTestCodes.E_BB;
import static picante.mechanics.EphemerisTestCodes.E_R;
import static picante.mechanics.EphemerisTestCodes.E_UNK;
import static picante.mechanics.EphemerisTestCodes.E_XA;
import static picante.mechanics.EphemerisTestCodes.E_XB;
import static picante.mechanics.FrameTestCodes.AA;
import static picante.mechanics.FrameTestCodes.AB;
import static picante.mechanics.FrameTestCodes.AC;
import static picante.mechanics.FrameTestCodes.BA;
import static picante.mechanics.FrameTestCodes.BB;
import static picante.mechanics.FrameTestCodes.R;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;
import picante.math.intervals.Interval;
import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.Coverage;
import picante.mechanics.EphemerisEvaluationException;
import picante.mechanics.EphemerisID;
import picante.mechanics.EphemerisProvider;
import picante.mechanics.FrameID;
import picante.mechanics.FrameProvider;
import picante.mechanics.FrameTransformFunction;
import picante.mechanics.PositionVectorFunction;
import picante.mechanics.SourceException;
import picante.mechanics.StateTransform;
import picante.mechanics.StateVector;
import picante.mechanics.StateVectorFunction;

@SuppressWarnings("unused")
public abstract class EphemerisProviderTest {

  /**
   * Create an ephemeris provider, installing the appropriate sources by whatever mechanism is
   * required in the particular implementation of the interface.
   * 
   * @param ephemerisSources a list of ephemeris sources to load in priority order
   * @param frameSources a list of frame sources to supply to a frame provider supporting the
   *        ephemeris provider
   * 
   * @return a newly created implementation of the <code>EphemerisProvider</code> interface
   */

  public abstract EphemerisProvider createProvider(
      List<? extends PositionVectorFunction> ephemerisSources,
      List<? extends FrameTransformFunction> frameSources);

  private static final double LOOSE_TOLERANCE = 1.0E-13;
  private static final double TOLERANCE = 1.0E-14;
  private static final double TIGHT_TOLERANCE = 1.0E-15;

  protected VectorIJK vector = new VectorIJK();
  protected StateVector state = new StateVector();

  protected VectorIJK testVector = new VectorIJK();
  protected StateVector testState = new StateVector();

  protected VectorIJK altVector = new VectorIJK();
  protected StateVector altState = new StateVector();

  protected EphemerisProvider staticTreePureStateVectorProvider;
  protected Map<EphemerisCodePair, List<ConstantPositionVectorFunction>> staticTreePureStateVectorContent;
  protected List<PositionVectorFunction> staticTreePureStateVectorSourceList;
  protected Set<EphemerisTestCodes> staticTreePureStateVectorCodes;
  protected List<FrameTransformFunction> staticTreePureStateVectorProviderFrameSources;

  protected EphemerisProvider staticTreePurePositionProvider;
  protected Map<EphemerisCodePair, List<ConstantPositionVectorFunction>> staticTreePurePositionContent;
  protected List<PositionVectorFunction> staticTreePurePositionSourceList;
  protected Set<EphemerisTestCodes> staticTreePurePositionCodes;
  protected List<FrameTransformFunction> staticTreePurePositionProviderFrameSources;

  protected EphemerisProvider staticTreeMixedProvider;
  protected Map<EphemerisCodePair, List<ConstantPositionVectorFunction>> staticTreeMixedContent;
  protected List<PositionVectorFunction> staticTreeMixedSourceList;
  protected Set<EphemerisTestCodes> staticTreeMixedCodes;
  protected List<FrameTransformFunction> staticTreeMixedFrameSources;

  protected EphemerisProvider staticTreeRotatedProvider;
  protected Map<EphemerisCodePair, List<ConstantPositionVectorFunction>> staticTreeRotatedContent;
  protected List<PositionVectorFunction> staticTreeRotatedSourceList;
  protected Set<EphemerisTestCodes> staticTreeRotatedCodes;
  protected List<FrameTransformFunction> staticTreeRotatedFrameSources;

  protected double time = 0.0;

  @Before
  public void setUp() throws Exception {
    clearBuffers();
    populateStaticTreePureStateVectorProvider();
    populateStaticTreePurePositionProvider();
    populateStaticTreeMixedProvider();
    populateStaticTreeRotatedProvider();
  }

  private void clearBuffers() {
    vector.setTo(VectorIJK.ZERO);
    state.setTo(StateVector.ZERO);
    testVector.setTo(VectorIJK.ZERO);
    testState.setTo(StateVector.ZERO);
    altVector.setTo(VectorIJK.ZERO);
    altState.setTo(StateVector.ZERO);
  }

  protected void addContentToSet(ConstantPositionVectorFunction function,
      Set<EphemerisTestCodes> codeSet) {
    codeSet.add(function.getObserverID());
    codeSet.add(function.getTargetID());
  }

  protected void addContentToMap(ConstantPositionVectorFunction function,
      Map<EphemerisCodePair, List<ConstantPositionVectorFunction>> map) {
    EphemerisCodePair pair =
        new EphemerisCodePair(function.getTargetID(), function.getObserverID());

    if (map.containsKey(pair)) {
      map.get(pair).add(function);
      return;
    }

    LinkedList<ConstantPositionVectorFunction> newList =
        new LinkedList<ConstantPositionVectorFunction>();
    newList.add(function);

    map.put(pair, newList);
  }

  private ConstantPositionVectorFunction getContentFromMap(EphemerisTestCodes targetID,
      EphemerisTestCodes observerID, double time,
      Map<EphemerisCodePair, List<ConstantPositionVectorFunction>> map) {

    EphemerisCodePair pair = new EphemerisCodePair(targetID, observerID);

    if (map.containsKey(pair)) {
      List<ConstantPositionVectorFunction> functions = map.get(pair);

      ListIterator<ConstantPositionVectorFunction> iterator =
          functions.listIterator(functions.size());

      while (iterator.hasPrevious()) {
        ConstantPositionVectorFunction function = iterator.previous();
        if (function.getCoverage().contains(time)) {
          return function;
        }
      }
    }

    return null;
  }

  private ConstantStateVectorFunction getStateContentFromMap(EphemerisTestCodes targetID,
      EphemerisTestCodes observerID, double time,
      Map<EphemerisCodePair, List<ConstantPositionVectorFunction>> map) {

    EphemerisCodePair pair = new EphemerisCodePair(targetID, observerID);

    if (map.containsKey(pair)) {
      List<ConstantPositionVectorFunction> functions = map.get(pair);

      ListIterator<ConstantPositionVectorFunction> iterator =
          functions.listIterator(functions.size());

      while (iterator.hasPrevious()) {
        ConstantPositionVectorFunction function = iterator.previous();
        if ((function.getCoverage().contains(time))
            && (function instanceof ConstantStateVectorFunction)) {
          return (ConstantStateVectorFunction) function;
        }
      }

    }
    return null;
  }

  protected VectorIJK computeChain(VectorIJK buffer, double time, FrameID desiredFrame,
      FrameProvider provider, Map<EphemerisCodePair, List<ConstantPositionVectorFunction>> map,
      EphemerisTestCodes... codes) {

    EphemerisTestCodes targetID = codes[0];
    EphemerisTestCodes observerID;

    buffer.setTo(VectorIJK.ZERO);

    VectorIJK tmp = new VectorIJK();
    RotationMatrixIJK matrix = new RotationMatrixIJK();
    CoverageInterval interval = new CoverageInterval();

    for (int i = 1; i < codes.length; i++) {
      observerID = codes[i];

      PositionVectorFunction function = getContentFromMap(targetID, observerID, time, map);
      function.getPosition(time, tmp);

      interval.setTimes(time, time);

      provider.createFrameTransformFunction(function.getFrameID(), desiredFrame, interval)
          .getTransform(time, matrix);

      matrix.mxv(tmp, tmp);
      VectorIJK.add(tmp, buffer, buffer);

      targetID = observerID;
    }

    return buffer;
  }

  protected StateVector computeChain(StateVector buffer, double time, FrameID desiredFrame,
      FrameProvider provider, Map<EphemerisCodePair, List<ConstantPositionVectorFunction>> map,
      EphemerisTestCodes... codes) {

    EphemerisTestCodes targetID = codes[0];
    EphemerisTestCodes observerID;

    buffer.setTo(StateVector.ZERO);

    StateVector tmp = new StateVector();
    StateTransform matrix = new StateTransform();
    CoverageInterval interval = new CoverageInterval();

    for (int i = 1; i < codes.length; i++) {
      observerID = codes[i];

      StateVectorFunction function = getStateContentFromMap(targetID, observerID, time, map);
      function.getState(time, tmp);

      interval.setTimes(time, time);

      provider.createStateTransformFunction(function.getFrameID(), desiredFrame, interval)
          .getStateTransform(time, matrix);

      matrix.mxv(tmp, tmp);
      StateVector.add(tmp, buffer, buffer);

      targetID = observerID;
    }

    return buffer;
  }

  protected List<FrameTransformFunction> createStaticFrameSources() {

    List<FrameTransformFunction> sources = new LinkedList<>();
    List<ConstantStateTransformFunction> functions =
        new LinkedList<ConstantStateTransformFunction>();

    functions.add(new ConstantStateTransformFunction(AA, AB, Coverage.ALL_TIME,
        new StateTransform(
            new RotationMatrixIJK(0.23262501680698122, -0.11345880161244597, 0.9659258262890683,
                0.6851945196756121, 0.7239556287790326, -0.0799794834045749, -0.6902130625843436,
                0.6804523112576097, 0.2461515393860416),
            new MatrixIJK(0.0060232452411852384, -0.005938066617125284, -0.0021480774105694634,
                0.012046490482370477, -0.011876133234250567, -0.004296154821138927,
                0.013988936617854983, 0.011645294533787818, 0.007033387679941524))));

    functions.add(new ConstantStateTransformFunction(AB, AC, Coverage.ALL_TIME,
        new StateTransform(
            new RotationMatrixIJK(0.46865981337225204, -0.5847388119079097, 0.6621468879171843,
                0.5103093929582195, 0.7910490458898228, 0.33738069069734045, -0.7210702481648369,
                0.17978300488457546, 0.6691306063588582),
            new MatrixIJK(0.019362795617574476, 0.0753076627264154, 0.052799083840262336,
                -0.01572818413834173, 0.04475247691123446, -0.0811402810436, 0.0014538445916930972,
                0.04802405585505995, -0.011336478881335063))));

    functions.add(new ConstantStateTransformFunction(AC, R, Coverage.ALL_TIME,
        new StateTransform(
            new RotationMatrixIJK(0.08348014257622213, -0.0629067995414709, 0.9945218953682733,
                0.6291677054416244, 0.7772610183154323, -0.0036479907591269655, -0.7727736177075243,
                0.6260255937090003, 0.10446478735209536),
            new MatrixIJK(-0.037955936010262696, 0.008286651706940469, 0.0037101784332086787,
                0.0014570033479893777, -0.0010979307738907262, 0.017357681557350912,
                -0.0029140066959787553, 0.0021958615477814525, -0.034715363114701824))));

    functions.add(new ConstantStateTransformFunction(BA, BB, Coverage.ALL_TIME,
        new StateTransform(
            new RotationMatrixIJK(0.9781476007338057, -0.20636194860240742, 0.02533806124621436,
                0.05029839035996436, 0.11662217487111268, -0.991901880356887, 0.20173582504328824,
                0.9715009081299661, 0.12445337438878395),
            new MatrixIJK(0.009246054318345706, 0.04781450720044231, 0.03248429394833022,
                0.03617070740362956, 0.045465125418033596, 0.007179712337132024,
                -0.0538493061705677, 0.004698763564817432, 0.050609163222396385))));

    functions.add(new ConstantStateTransformFunction(BB, R, Coverage.ALL_TIME,
        new StateTransform(
            new RotationMatrixIJK(-0.7009436628498646, -0.6793378264128004, 0.2172052004795177,
                0.6582394338705277, -0.7334323383185172, -0.16969930113715712, 0.27458847246092255,
                0.024023378475231092, 0.9612616959383189),
            new MatrixIJK(-0.7466815019357148, 0.9010880847202829, 0.40865298683151163,
                -0.808439505815695, -0.8257748557786994, 0.43313802873181917, 0.03192084357758847,
                0.27028827309353176, -0.015873235378095915))));

    sources.addAll(functions);
    return sources;
  }

  @Test
  public void testGetSourcesInLoadOrder() {

    List<PositionVectorFunction> returnedList;

    returnedList = staticTreePureStateVectorProvider.getEphemerisSourcesInLoadOrder();
    assertEquals(staticTreePureStateVectorSourceList, returnedList);

    returnedList = staticTreePurePositionProvider.getEphemerisSourcesInLoadOrder();
    assertEquals(staticTreePurePositionSourceList, returnedList);

  }

  @Test
  public void testGetKnownObjects() {
    HashSet<EphemerisID> set = new HashSet<EphemerisID>();
    Set<EphemerisID> returnedSet;

    returnedSet = staticTreePureStateVectorProvider.getKnownObjects(set);
    assertSame(returnedSet, set);
    assertEquals(staticTreePureStateVectorCodes, returnedSet);
    set.clear();

    returnedSet = staticTreePurePositionProvider.getKnownObjects(set);
    assertSame(returnedSet, set);
    assertEquals(staticTreePurePositionCodes, returnedSet);
    set.clear();
  }

  @Test
  public void testGetKnownObjectsUnion() {

    HashSet<EphemerisID> set = new HashSet<EphemerisID>();
    Set<EphemerisID> returnedSet;
    HashSet<EphemerisID> testSet = new HashSet<EphemerisID>();

    set.add(E_UNK);
    testSet.addAll(staticTreePureStateVectorCodes);
    testSet.add(E_UNK);
    returnedSet = staticTreePureStateVectorProvider.getKnownObjects(set);
    assertSame(returnedSet, set);
    assertEquals(testSet, returnedSet);
    set.clear();
    testSet.clear();

    set.add(E_UNK);
    testSet.addAll(staticTreePurePositionCodes);
    testSet.add(E_UNK);
    returnedSet = staticTreePurePositionProvider.getKnownObjects(set);
    assertSame(returnedSet, set);
    assertEquals(testSet, returnedSet);
    set.clear();
    testSet.clear();

  }

  @Test
  public void testIsAwareOf() {
    for (EphemerisTestCodes code : staticTreePureStateVectorCodes) {
      assertTrue(staticTreePureStateVectorProvider.isAwareOf(code));
    }
    assertFalse(staticTreePureStateVectorProvider.isAwareOf(E_UNK));

    for (EphemerisTestCodes code : staticTreePurePositionCodes) {
      assertTrue(staticTreePurePositionProvider.isAwareOf(code));
    }
    assertFalse(staticTreePurePositionProvider.isAwareOf(E_UNK));
  }

  @Test
  public void testGetUnwritableFrameProvider() {

    FrameProvider provider = staticTreePureStateVectorProvider.getFrameProvider();
    List<FrameTransformFunction> sources = provider.getFrameSourcesInLoadOrder();

    assertEquals(staticTreePureStateVectorProviderFrameSources, sources);

    provider = staticTreePurePositionProvider.getFrameProvider();
    sources = provider.getFrameSourcesInLoadOrder();

    assertEquals(staticTreePurePositionProviderFrameSources, sources);
  }

  private void populateStaticTreePureStateVectorProvider() throws SourceException {

    Set<EphemerisTestCodes> codeSet = new HashSet<EphemerisTestCodes>();
    List<PositionVectorFunction> sources = new LinkedList<>();
    List<ConstantPositionVectorFunction> functions = new LinkedList<>();
    Map<EphemerisCodePair, List<ConstantPositionVectorFunction>> map =
        new HashMap<EphemerisCodePair, List<ConstantPositionVectorFunction>>();
    List<FrameTransformFunction> frameSources;

    ConstantStateVectorFunction function = new ConstantStateVectorFunction(E_AA, E_AB, R,
        Coverage.ALL_TIME, new StateVector(new VectorIJK(10, 20, 30), new VectorIJK(1, 2, 5)));

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    function = new ConstantStateVectorFunction(E_AB, E_AC, R, Coverage.ALL_TIME,
        new StateVector(new VectorIJK(-1, Math.PI, 12.7), new VectorIJK(-1, 17, 32)));

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    function = new ConstantStateVectorFunction(E_AC, E_R, R, Coverage.ALL_TIME,
        new StateVector(new VectorIJK(-1.5, 47.2, -345.1), new VectorIJK(2.23, 1.01, 2.3)));

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    function = new ConstantStateVectorFunction(E_BA, E_BB, R, Coverage.ALL_TIME,
        new StateVector(new VectorIJK(27.2, 12314.1, -4501.123), new VectorIJK(6.28, 31.7, 16.24)));

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    function = new ConstantStateVectorFunction(E_BB, E_R, R, Coverage.ALL_TIME,
        new StateVector(new VectorIJK(27.2, 12314.1, -4501.123), new VectorIJK(6.28, 31.7, 16.24)));

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    sources.addAll(functions);
    functions.clear();

    function = new ConstantStateVectorFunction(E_XB, E_XA, R, Coverage.ALL_TIME, new StateVector());

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    sources.addAll(functions);
    functions.clear();

    frameSources = createStaticFrameSources();
    staticTreePureStateVectorProvider = createProvider(sources, frameSources);
    staticTreePureStateVectorProviderFrameSources = frameSources;
    staticTreePureStateVectorContent = map;
    staticTreePureStateVectorSourceList = sources;
    staticTreePureStateVectorCodes = codeSet;
  }

  /**
   * Test exercising that the provider responds properly to requests for a position function
   * involving a code unknown to the provider.
   */
  @Test
  public void testTimeForwardStaticTreePureStateVectorProviderUnknownPosition() {

    /*
     * There are two possible outcomes, the first is that the provider throws an exception when
     * attempting to create the function, the other is a runtime evaluation exception gets generated
     * when asking for a transform. Handle both cases.
     */
    try {
      PositionVectorFunction function = staticTreePureStateVectorProvider
          .createPositionVectorFunction(E_AA, E_UNK, R, Coverage.ALL_TIME);

      assertEquals(E_AA, function.getTargetID());
      assertEquals(E_UNK, function.getObserverID());
      assertEquals(R, function.getFrameID());

      time += 5;

      try {
        function.getPosition(time, vector);
      } catch (EphemerisEvaluationException e) {
        return;
      }

    } catch (SourceException e) {
      return;
    }

    fail("Expected source exception not thrown.");

  }

  /**
   * Test exercising that the provider responds properly to requests for a body state function
   * involving a code unknown to the provider.
   */
  @Test
  public void testTimeForwardStaticTreePureStateVectorProviderUnknownState() {

    /*
     * There are two possible outcomes, the first is that the provider throws an exception when
     * attempting to create the function, the other is a runtime evaluation exception gets generated
     * when asking for a transform. Handle both cases.
     */
    try {
      StateVectorFunction function = staticTreePureStateVectorProvider
          .createStateVectorFunction(E_AA, E_UNK, R, Coverage.ALL_TIME);

      assertEquals(E_AA, function.getTargetID());
      assertEquals(E_UNK, function.getObserverID());
      assertEquals(R, function.getFrameID());

      time += 5;

      try {
        function.getState(time, state);
      } catch (EphemerisEvaluationException e) {
        return;
      }

    } catch (SourceException e) {
      return;
    }

    fail("Expected source exception not thrown.");

  }

  /**
   * Test exercising that the provider responds to requests for a position vector from a state
   * function involving a code unknown to the provider.
   */
  @Test
  public void testTimeForwardStaticTreePureStateVectorProviderUnknownPositionFromState() {

    /*
     * There are two possible outcomes, the first is that the provider throws an exception when
     * attempting to create the function, the other is a runtime evaluation exception gets generated
     * when asking for a transform. Handle both cases.
     */
    try {
      StateVectorFunction function = staticTreePureStateVectorProvider
          .createStateVectorFunction(E_AA, E_UNK, R, Coverage.ALL_TIME);

      assertEquals(E_AA, function.getTargetID());
      assertEquals(E_UNK, function.getObserverID());
      assertEquals(R, function.getFrameID());

      time += 5;

      try {
        function.getPosition(time, vector);
      } catch (EphemerisEvaluationException e) {
        return;
      }

    } catch (SourceException e) {
      return;
    }

    fail("Expected source exception not thrown.");

  }

  /**
   * This test exercises whether the provider properly handles the loopback exceptional case, where
   * two equivalent codes are used as the target and observer ID arguments.
   * 
   * @throws SourceException
   */
  @Test
  public void testTimeForwardStateTreePureStateVectorProviderLoopbackLinkPosition()
      throws SourceException {

    PositionVectorFunction function = staticTreePureStateVectorProvider
        .createPositionVectorFunction(E_AA, E_AA, R, Coverage.ALL_TIME);

    assertEquals(E_AA, function.getTargetID());
    assertEquals(E_AA, function.getObserverID());
    assertEquals(R, function.getFrameID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;
    VectorIJK returned = function.getPosition(time, vector);
    assertSame(returned, vector);
    assertEquivalentVector(VectorIJK.ZERO, vector);
  }

  /**
   * This test exercises whether the provider properly creates a position vector function that links
   * forward through a single node in the tree loaded into the provider.
   * 
   * @throws SourceException
   */
  @Test
  public void testTimeForwardStaticTreePureStateVectorProviderSingleForwardLinkPosition()
      throws SourceException {

    PositionVectorFunction function = staticTreePureStateVectorProvider
        .createPositionVectorFunction(E_AA, E_AB, R, Coverage.ALL_TIME);

    assertEquals(E_AA, function.getTargetID());
    assertEquals(E_AB, function.getObserverID());
    assertEquals(R, function.getFrameID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;
    VectorIJK returned = function.getPosition(time, vector);
    assertSame(returned, vector);

    /*
     * Testing this is simple as it is merely a single node. Retrieve it from the map.
     */
    ConstantPositionVectorFunction testFunction =
        getContentFromMap(E_AA, E_AB, time, staticTreePureStateVectorContent);

    testFunction.getPosition(time, testVector);
    assertEquivalentVector(testVector, vector);
  }

  @Test
  public void testTimeForwardStaticTreePureStateVectorProviderSingleBackwardLinkPosition()
      throws SourceException {

    PositionVectorFunction function = staticTreePureStateVectorProvider
        .createPositionVectorFunction(E_AB, E_AA, R, Coverage.ALL_TIME);

    assertEquals(E_AB, function.getTargetID());
    assertEquals(E_AA, function.getObserverID());
    assertEquals(R, function.getFrameID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;
    VectorIJK returned = function.getPosition(time, vector);
    assertSame(returned, vector);

    /*
     * Testing this is simple, as it is merely a single node. Retrieve it from the map.
     */
    ConstantPositionVectorFunction testFunction =
        getContentFromMap(E_AA, E_AB, time, staticTreePureStateVectorContent);

    testFunction.getPosition(time, testVector);
    assertEquivalentVector(testVector.negate(), vector);
  }

  /**
   * This test exercises whether the provider properly connects a leaf node to the top of its
   * section of the frame tree.
   */
  @Test
  public void testTimeForwardStaticTreePureStateVectorProviderSingleLeafForwardLinkPosition()
      throws SourceException {

    PositionVectorFunction function = staticTreePureStateVectorProvider
        .createPositionVectorFunction(E_AA, E_R, R, Coverage.ALL_TIME);

    assertEquals(E_AA, function.getTargetID());
    assertEquals(E_R, function.getObserverID());
    assertEquals(R, function.getFrameID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;
    VectorIJK returned = function.getPosition(time, vector);
    assertSame(returned, vector);

    computeChain(testVector, time, R, staticTreePureStateVectorProvider.getFrameProvider(),
        staticTreePureStateVectorContent, E_AA, E_AB, E_AC, E_R);
    assertEquivalentVector(testVector, vector);

  }

  /**
   * This test exercises whether the provider properly connects the root node of a section of the
   * ephemeris tree to one of its links.
   * 
   * @throws SourceException
   */
  @Test
  public void testTimeForwardStaticTreePureStateVectorProviderSingleLeafBackwardLinkPosition()
      throws SourceException {

    PositionVectorFunction function = staticTreePureStateVectorProvider
        .createPositionVectorFunction(E_R, E_AA, R, Coverage.ALL_TIME);

    assertEquals(E_R, function.getTargetID());
    assertEquals(E_AA, function.getObserverID());
    assertEquals(R, function.getFrameID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;

    VectorIJK returned = function.getPosition(time, vector);
    assertSame(returned, vector);

    computeChain(testVector, time, R, staticTreePureStateVectorProvider.getFrameProvider(),
        staticTreePureStateVectorContent, E_AA, E_AB, E_AC, E_R);
    assertEquivalentVector(testVector.negate(), vector);
  }

  /**
   * This test exercises whether the provider properly connects two leaf nodes sharing a common
   * root.
   * 
   * @throws SourceException
   */
  @Test
  public void testTimeForwardStaticTreePureStateVectorProviderFullLinkPosition()
      throws SourceException {

    PositionVectorFunction function = staticTreePureStateVectorProvider
        .createPositionVectorFunction(E_AA, E_BA, R, Coverage.ALL_TIME);

    assertEquals(E_AA, function.getTargetID());
    assertEquals(E_BA, function.getObserverID());
    assertEquals(R, function.getFrameID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;

    VectorIJK returned = function.getPosition(time, vector);
    assertSame(returned, vector);

    computeChain(testVector, time, R, staticTreePureStateVectorProvider.getFrameProvider(),
        staticTreePureStateVectorContent, E_AA, E_AB, E_AC, E_R);
    computeChain(altVector, time, R, staticTreePureStateVectorProvider.getFrameProvider(),
        staticTreePureStateVectorContent, E_BA, E_BB, E_R);
    VectorIJK.subtract(testVector, altVector, testVector);

    assertEquivalentVector(testVector, vector);
  }

  /**
   * This test exercises whether crossing a broken link in the ephemeris tree results in either of
   * the two acceptable exceptions being generated.
   */
  @Test
  public void testTimeForwardStaticTreePureStateVectorProviderBrokenLinkPosition() {

    try {
      PositionVectorFunction function = staticTreePureStateVectorProvider
          .createPositionVectorFunction(E_AA, E_XB, R, Coverage.ALL_TIME);

      assertEquals(E_AA, function.getTargetID());
      assertEquals(E_XB, function.getObserverID());

      time += 5;

      try {
        function.getPosition(time, vector);
      } catch (EphemerisEvaluationException e) {
        return;
      }

    } catch (SourceException e) {
      return;
    }
    fail("Expected source exception not thrown.");
  }

  /**
   * This test exercises whether the provider properly handles the loopback exceptional case, where
   * two equivalent codes are used as the target and observer ID arguments.
   * 
   * @throws SourceException
   */
  @Test
  public void testTimeForwardStateTreePureStateVectorProviderLoopbackLinkState()
      throws SourceException {

    StateVectorFunction function = staticTreePureStateVectorProvider.createStateVectorFunction(E_AA,
        E_AA, R, Coverage.ALL_TIME);

    assertEquals(E_AA, function.getTargetID());
    assertEquals(E_AA, function.getObserverID());
    assertEquals(R, function.getFrameID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;
    VectorIJK retVector = function.getPosition(time, vector);
    assertSame(retVector, vector);
    assertEquivalentVector(VectorIJK.ZERO, vector);

    time += 5;
    StateVector retState = function.getState(time, state);
    assertSame(retState, state);
    assertEquivalentStateVector(StateVector.ZERO, state);

  }

  /**
   * This test exercises whether the provider properly creates a position vector function that links
   * forward through a single node in the tree loaded into the provider.
   * 
   * @throws SourceException
   */
  @Test
  public void testTimeForwardStaticTreePureStateVectorProviderSingleForwardLinkState()
      throws SourceException {

    StateVectorFunction function = staticTreePureStateVectorProvider.createStateVectorFunction(E_AA,
        E_AB, R, Coverage.ALL_TIME);

    assertEquals(E_AA, function.getTargetID());
    assertEquals(E_AB, function.getObserverID());
    assertEquals(R, function.getFrameID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;
    VectorIJK retVector = function.getPosition(time, vector);
    assertSame(retVector, vector);

    /*
     * Testing this is simple as it is merely a single node. Retrieve it from the map.
     */
    ConstantPositionVectorFunction testFunction =
        getContentFromMap(E_AA, E_AB, time, staticTreePureStateVectorContent);

    testFunction.getPosition(time, testVector);
    assertEquivalentVector(testVector, vector);

    time += 5;
    StateVector retState = function.getState(time, state);
    assertSame(retState, state);

    ConstantStateVectorFunction testStateFunction =
        getStateContentFromMap(E_AA, E_AB, time, staticTreePureStateVectorContent);

    testStateFunction.getState(time, testState);
    assertEquivalentStateVector(testState, state);
  }

  @Test
  public void testTimeForwardStaticTreePureStateVectorProviderSingleBackwardLinkState()
      throws SourceException {

    StateVectorFunction function = staticTreePureStateVectorProvider.createStateVectorFunction(E_AB,
        E_AA, R, Coverage.ALL_TIME);

    assertEquals(E_AB, function.getTargetID());
    assertEquals(E_AA, function.getObserverID());
    assertEquals(R, function.getFrameID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;
    VectorIJK retVector = function.getPosition(time, vector);
    assertSame(retVector, vector);

    /*
     * Testing this is simple, as it is merely a single node. Retrieve it from the map.
     */
    ConstantPositionVectorFunction testFunction =
        getContentFromMap(E_AA, E_AB, time, staticTreePureStateVectorContent);

    testFunction.getPosition(time, testVector);
    assertEquivalentVector(testVector.negate(), vector);

    StateVector retState = function.getState(time, state);
    assertSame(retState, state);

    ConstantStateVectorFunction testStateFunction =
        getStateContentFromMap(E_AA, E_AB, time, staticTreePureStateVectorContent);

    testStateFunction.getState(time, testState);
    assertEquivalentStateVector(testState.negate(), state);

  }

  /**
   * This test exercises whether the provider properly connects a leaf node to the top of its
   * section of the frame tree.
   */
  @Test
  public void testTimeForwardStaticTreePureStateVectorProviderSingleLeafForwardLinkState()
      throws SourceException {

    StateVectorFunction function = staticTreePureStateVectorProvider.createStateVectorFunction(E_AA,
        E_R, R, Coverage.ALL_TIME);

    assertEquals(E_AA, function.getTargetID());
    assertEquals(E_R, function.getObserverID());
    assertEquals(R, function.getFrameID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;
    VectorIJK retVector = function.getPosition(time, vector);
    assertSame(retVector, vector);

    computeChain(testVector, time, R, staticTreePureStateVectorProvider.getFrameProvider(),
        staticTreePureStateVectorContent, E_AA, E_AB, E_AC, E_R);
    assertEquivalentVector(testVector, vector);

    time += 5;
    StateVector retState = function.getState(time, state);
    assertSame(retState, state);

    computeChain(testState, time, R, staticTreePureStateVectorProvider.getFrameProvider(),
        staticTreePureStateVectorContent, E_AA, E_AB, E_AC, E_R);

    assertEquivalentStateVector(testState, state);

  }

  /**
   * This test exercises whether the provider properly connects the root node of a section of the
   * ephemeris tree to one of its links.
   * 
   * @throws SourceException
   */
  @Test
  public void testTimeForwardStaticTreePureStateVectorProviderSingleLeafBackwardLinkState()
      throws SourceException {

    StateVectorFunction function = staticTreePureStateVectorProvider.createStateVectorFunction(E_R,
        E_AA, R, Coverage.ALL_TIME);

    assertEquals(E_R, function.getTargetID());
    assertEquals(E_AA, function.getObserverID());
    assertEquals(R, function.getFrameID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;

    VectorIJK retVector = function.getPosition(time, vector);
    assertSame(retVector, vector);

    computeChain(testVector, time, R, staticTreePureStateVectorProvider.getFrameProvider(),
        staticTreePureStateVectorContent, E_AA, E_AB, E_AC, E_R);
    assertEquivalentVector(testVector.negate(), vector);

    time += 5;

    StateVector retState = function.getState(time, state);
    assertSame(retState, state);

    computeChain(testState, time, R, staticTreePureStateVectorProvider.getFrameProvider(),
        staticTreePureStateVectorContent, E_AA, E_AB, E_AC, E_R);
  }

  /**
   * This test exercises whether the provider properly connects two leaf nodes sharing a common
   * root.
   * 
   * @throws SourceException
   */
  @Test
  public void testTimeForwardStaticTreePureStateVectorProviderFullLinkState()
      throws SourceException {

    StateVectorFunction function = staticTreePureStateVectorProvider.createStateVectorFunction(E_AA,
        E_BA, R, Coverage.ALL_TIME);

    assertEquals(E_AA, function.getTargetID());
    assertEquals(E_BA, function.getObserverID());
    assertEquals(R, function.getFrameID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;

    VectorIJK retVector = function.getPosition(time, vector);
    assertSame(retVector, vector);

    computeChain(testVector, time, R, staticTreePureStateVectorProvider.getFrameProvider(),
        staticTreePureStateVectorContent, E_AA, E_AB, E_AC, E_R);
    computeChain(altVector, time, R, staticTreePureStateVectorProvider.getFrameProvider(),
        staticTreePureStateVectorContent, E_BA, E_BB, E_R);
    VectorIJK.subtract(testVector, altVector, testVector);

    assertEquivalentVector(testVector, vector);

    time += 5;

    StateVector retState = function.getState(time, state);
    assertSame(retState, state);

    computeChain(testState, time, R, staticTreePureStateVectorProvider.getFrameProvider(),
        staticTreePureStateVectorContent, E_AA, E_AB, E_AC, E_R);
    computeChain(altState, time, R, staticTreePureStateVectorProvider.getFrameProvider(),
        staticTreePureStateVectorContent, E_BA, E_BB, E_R);
    StateVector.subtract(testState, altState, testState);

    assertEquivalentStateVector(testState, state);

  }

  /**
   * This test exercises whether crossing a broken link in the ephemeris tree results in either of
   * the two acceptable exceptions being generated.
   */
  @Test
  public void testTimeForwardStaticTreePureStateVectorProviderBrokenLinkState() {

    try {
      StateVectorFunction function = staticTreePureStateVectorProvider
          .createStateVectorFunction(E_AA, E_XB, R, Coverage.ALL_TIME);

      assertEquals(E_AA, function.getTargetID());
      assertEquals(E_XB, function.getObserverID());

      time += 5;

      try {
        function.getState(time, state);
      } catch (EphemerisEvaluationException e) {
        return;
      }

    } catch (SourceException e) {
      return;
    }
    fail("Expected source exception not thrown.");
  }

  /**
   * This test exercises whether crossing a broken link in the ephemeris tree results in either of
   * the two acceptable exceptions being generated.
   */
  @Test
  public void testTimeForwardStaticTreePureStateVectorProviderBrokenLinkPositionFromState() {

    try {
      StateVectorFunction function = staticTreePureStateVectorProvider
          .createStateVectorFunction(E_AA, E_XB, R, Coverage.ALL_TIME);

      assertEquals(E_AA, function.getTargetID());
      assertEquals(E_XB, function.getObserverID());

      time += 5;

      try {
        function.getPosition(time, vector);
      } catch (EphemerisEvaluationException e) {
        return;
      }

    } catch (SourceException e) {
      return;
    }
    fail("Expected source exception not thrown.");
  }

  private void populateStaticTreePurePositionProvider() throws SourceException {

    Set<EphemerisTestCodes> codeSet = new HashSet<EphemerisTestCodes>();
    List<PositionVectorFunction> sources = new LinkedList<>();
    List<ConstantPositionVectorFunction> functions =
        new LinkedList<ConstantPositionVectorFunction>();
    Map<EphemerisCodePair, List<ConstantPositionVectorFunction>> map =
        new HashMap<EphemerisCodePair, List<ConstantPositionVectorFunction>>();
    List<FrameTransformFunction> frameSources;

    ConstantPositionVectorFunction function = new ConstantPositionVectorFunction(E_AA, E_AB, R,
        Coverage.ALL_TIME, new VectorIJK(10, 20, 30));

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    function = new ConstantPositionVectorFunction(E_AB, E_AC, R, Coverage.ALL_TIME,
        new VectorIJK(12.7, -1, Math.PI));

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    function = new ConstantPositionVectorFunction(E_AC, E_R, R, Coverage.ALL_TIME,
        new VectorIJK(47.2, -1.5, -345.1));

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    function = new ConstantPositionVectorFunction(E_BA, E_BB, R, Coverage.ALL_TIME,
        new VectorIJK(1227.2, 24.1, -4501.123));

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    function = new ConstantPositionVectorFunction(E_BB, E_R, R, Coverage.ALL_TIME,
        new VectorIJK(2247.2, 124.1, -401.123));

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    sources.addAll(functions);
    functions.clear();

    function =
        new ConstantPositionVectorFunction(E_XB, E_XA, R, Coverage.ALL_TIME, new VectorIJK());

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    sources.addAll(functions);
    functions.clear();

    frameSources = createStaticFrameSources();
    staticTreePurePositionProvider = createProvider(sources, frameSources);
    staticTreePurePositionProviderFrameSources = frameSources;
    staticTreePurePositionContent = map;
    staticTreePurePositionSourceList = sources;
    staticTreePurePositionCodes = codeSet;
  }

  /**
   * This test exercises whether the provider properly handles the loopback exceptional case, where
   * two equivalent codes are used as the target and observer ID arguments.
   * 
   * @throws SourceException
   */
  @Test
  public void testTimeForwardStateTreePurePositionProviderLoopbackLink() throws SourceException {

    PositionVectorFunction function = staticTreePurePositionProvider
        .createPositionVectorFunction(E_AA, E_AA, R, Coverage.ALL_TIME);

    assertEquals(E_AA, function.getTargetID());
    assertEquals(E_AA, function.getObserverID());
    assertEquals(R, function.getFrameID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;
    VectorIJK returned = function.getPosition(time, vector);
    assertSame(returned, vector);
    assertEquivalentVector(VectorIJK.ZERO, vector);
  }

  /**
   * This test exercises whether the provider properly creates a position vector function that links
   * forward through a single node in the tree loaded into the provider.
   * 
   * @throws SourceException
   */
  @Test
  public void testTimeForwardStaticTreePurePositionProviderSingleForwardLink()
      throws SourceException {

    PositionVectorFunction function = staticTreePurePositionProvider
        .createPositionVectorFunction(E_AA, E_AB, R, Coverage.ALL_TIME);

    assertEquals(E_AA, function.getTargetID());
    assertEquals(E_AB, function.getObserverID());
    assertEquals(R, function.getFrameID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;
    VectorIJK returned = function.getPosition(time, vector);
    assertSame(returned, vector);

    /*
     * Testing this is simple as it is merely a single node. Retrieve it from the map.
     */
    ConstantPositionVectorFunction testFunction =
        getContentFromMap(E_AA, E_AB, time, staticTreePurePositionContent);

    testFunction.getPosition(time, testVector);
    assertEquivalentVector(testVector, vector);
  }

  @Test
  public void testTimeForwardStaticTreePurePositionProviderSingleBackwardLink()
      throws SourceException {

    PositionVectorFunction function = staticTreePurePositionProvider
        .createPositionVectorFunction(E_AB, E_AA, R, Coverage.ALL_TIME);

    assertEquals(E_AB, function.getTargetID());
    assertEquals(E_AA, function.getObserverID());
    assertEquals(R, function.getFrameID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;
    VectorIJK returned = function.getPosition(time, vector);
    assertSame(returned, vector);

    /*
     * Testing this is simple, as it is merely a single node. Retrieve it from the map.
     */
    ConstantPositionVectorFunction testFunction =
        getContentFromMap(E_AA, E_AB, time, staticTreePurePositionContent);

    testFunction.getPosition(time, testVector);
    assertEquivalentVector(testVector.negate(), vector);
  }

  /**
   * This test exercises whether the provider properly connects a leaf node to the top of its
   * section of the frame tree.
   */
  @Test
  public void testTimeForwardStaticTreePurePositionProviderSingleLeafForwardLink()
      throws SourceException {

    PositionVectorFunction function = staticTreePurePositionProvider
        .createPositionVectorFunction(E_AA, E_R, R, Coverage.ALL_TIME);

    assertEquals(E_AA, function.getTargetID());
    assertEquals(E_R, function.getObserverID());
    assertEquals(R, function.getFrameID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;
    VectorIJK returned = function.getPosition(time, vector);
    assertSame(returned, vector);

    computeChain(testVector, time, R, staticTreePurePositionProvider.getFrameProvider(),
        staticTreePurePositionContent, E_AA, E_AB, E_AC, E_R);
    assertEquivalentVector(testVector, vector);

  }

  /**
   * This test exercises whether the provider properly connects the root node of a section of the
   * ephemeris tree to one of its links.
   * 
   * @throws SourceException
   */
  @Test
  public void testTimeForwardStaticTreePurePositionProviderSingleLeafBackwardLink()
      throws SourceException {

    PositionVectorFunction function = staticTreePurePositionProvider
        .createPositionVectorFunction(E_R, E_AA, R, Coverage.ALL_TIME);

    assertEquals(E_R, function.getTargetID());
    assertEquals(E_AA, function.getObserverID());
    assertEquals(R, function.getFrameID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;

    VectorIJK returned = function.getPosition(time, vector);
    assertSame(returned, vector);

    computeChain(testVector, time, R, staticTreePurePositionProvider.getFrameProvider(),
        staticTreePurePositionContent, E_AA, E_AB, E_AC, E_R);
    assertEquivalentVector(testVector.negate(), vector);
  }

  /**
   * This test exercises whether the provider properly connects two leaf nodes sharing a common
   * root.
   * 
   * @throws SourceException
   */
  @Test
  public void testTimeForwardStaticTreePurePositionProviderFullLink() throws SourceException {

    PositionVectorFunction function = staticTreePurePositionProvider
        .createPositionVectorFunction(E_AA, E_BA, R, Coverage.ALL_TIME);

    assertEquals(E_AA, function.getTargetID());
    assertEquals(E_BA, function.getObserverID());
    assertEquals(R, function.getFrameID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;

    VectorIJK returned = function.getPosition(time, vector);
    assertSame(returned, vector);

    computeChain(testVector, time, R, staticTreePurePositionProvider.getFrameProvider(),
        staticTreePurePositionContent, E_AA, E_AB, E_AC, E_R);
    computeChain(altVector, time, R, staticTreePurePositionProvider.getFrameProvider(),
        staticTreePurePositionContent, E_BA, E_BB, E_R);
    VectorIJK.subtract(testVector, altVector, testVector);

    assertEquivalentVector(testVector, vector);
  }

  /**
   * This test exercises whether crossing a broken link in the ephemeris tree results in either of
   * the two acceptable exceptions being generated.
   */
  @Test
  public void testTimeForwardStaticTreePurePositionProviderBrokenLink() {

    try {
      PositionVectorFunction function = staticTreePurePositionProvider
          .createPositionVectorFunction(E_AA, E_XB, R, Coverage.ALL_TIME);

      assertEquals(E_AA, function.getTargetID());
      assertEquals(E_XB, function.getObserverID());

      time += 5;

      try {
        function.getPosition(time, vector);
      } catch (EphemerisEvaluationException e) {
        return;
      }

    } catch (SourceException e) {
      return;
    }
    fail("Expected source exception not thrown.");
  }

  private void populateStaticTreeMixedProvider() throws SourceException {

    Set<EphemerisTestCodes> codeSet = new HashSet<EphemerisTestCodes>();
    List<PositionVectorFunction> sources = new LinkedList<>();
    List<ConstantPositionVectorFunction> functions =
        new LinkedList<ConstantPositionVectorFunction>();
    Map<EphemerisCodePair, List<ConstantPositionVectorFunction>> map =
        new HashMap<EphemerisCodePair, List<ConstantPositionVectorFunction>>();
    List<FrameTransformFunction> frameSources;

    ConstantStateVectorFunction function = new ConstantStateVectorFunction(E_AA, E_AB, R,
        Coverage.ALL_TIME, new StateVector(new VectorIJK(10, 20, 30), new VectorIJK(1, 2, 5)));

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    function = new ConstantStateVectorFunction(E_AB, E_AC, R, Coverage.ALL_TIME,
        new StateVector(new VectorIJK(-1, Math.PI, 12.7), new VectorIJK(-1, 17, 32)));

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    function = new ConstantStateVectorFunction(E_AC, E_R, R, Coverage.ALL_TIME,
        new StateVector(new VectorIJK(-1.5, 47.2, -345.1), new VectorIJK(2.23, 1.01, 2.3)));

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    function = new ConstantStateVectorFunction(E_BA, E_BB, R, Coverage.ALL_TIME,
        new StateVector(new VectorIJK(27.2, 12314.1, -4501.123), new VectorIJK(6.28, 31.7, 16.24)));

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    function = new ConstantStateVectorFunction(E_BB, E_R, R, Coverage.ALL_TIME,
        new StateVector(new VectorIJK(27.2, 12314.1, -4501.123), new VectorIJK(6.28, 31.7, 16.24)));

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    sources.addAll(functions);
    functions.clear();

    function = new ConstantStateVectorFunction(E_XB, E_XA, R, Coverage.ALL_TIME, new StateVector());

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    ConstantPositionVectorFunction f =
        new ConstantPositionVectorFunction(E_AA, E_AC, R, Coverage.ALL_TIME, new VectorIJK());

    functions.add(f);
    addContentToMap(f, map);
    addContentToSet(f, codeSet);

    sources.addAll(functions);
    functions.clear();

    frameSources = createStaticFrameSources();
    staticTreeMixedProvider = createProvider(sources, frameSources);
    staticTreeMixedFrameSources = frameSources;
    staticTreeMixedContent = map;
    staticTreeMixedSourceList = sources;
    staticTreeMixedCodes = codeSet;
  }

  /**
   * Verify that selecting a state vector function from the provider when a body position function
   * source overrides the contents works properly
   * 
   * @throws SourceException
   */
  @Test
  public void testTimeForwardStaticTreeMixedStateSelection() throws SourceException {

    StateVectorFunction function =
        staticTreeMixedProvider.createStateVectorFunction(E_AA, E_BA, R, Coverage.ALL_TIME);

    assertEquals(E_AA, function.getTargetID());
    assertEquals(E_BA, function.getObserverID());
    assertEquals(R, function.getFrameID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;
    VectorIJK retVec = function.getPosition(time, vector);
    assertSame(retVec, vector);

    computeChain(state, time, R, staticTreeMixedProvider.getFrameProvider(), staticTreeMixedContent,
        E_AA, E_AB, E_AC, E_R);
    computeChain(testState, time, R, staticTreeMixedProvider.getFrameProvider(),
        staticTreeMixedContent, E_BA, E_BB, E_R);
    StateVector.subtract(state, testState, state);

    assertEquivalentVector(state.getPosition(), vector);

    time += 5;
    StateVector retState = function.getState(time, state);
    assertSame(retState, state);

    computeChain(testState, time, R, staticTreeMixedProvider.getFrameProvider(),
        staticTreeMixedContent, E_AA, E_AB, E_AC, E_R);
    computeChain(altState, time, R, staticTreeMixedProvider.getFrameProvider(),
        staticTreeMixedContent, E_BA, E_BB, E_R);
    StateVector.subtract(testState, altState, testState);

    assertEquivalentStateVector(testState, state);

  }

  /**
   * Verify that the overriding position vector function is selected when querying for position
   * vector functions only.
   * 
   * @throws SourceException
   */
  @Test
  public void testTimeForwardStaticTreeMixedPositionSelection() throws SourceException {

    PositionVectorFunction function =
        staticTreeMixedProvider.createPositionVectorFunction(E_AA, E_BA, R, Coverage.ALL_TIME);

    assertEquals(E_AA, function.getTargetID());
    assertEquals(E_BA, function.getObserverID());
    assertEquals(R, function.getFrameID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;
    VectorIJK retVec = function.getPosition(time, vector);
    assertSame(retVec, vector);

    computeChain(testVector, time, R, staticTreeMixedProvider.getFrameProvider(),
        staticTreeMixedContent, E_AA, E_AC, E_R);
    computeChain(altVector, time, R, staticTreeMixedProvider.getFrameProvider(),
        staticTreeMixedContent, E_BA, E_BB, E_R);
    VectorIJK.subtract(testVector, altVector, testVector);

    assertEquivalentVector(testVector, vector);

  }

  private void populateStaticTreeRotatedProvider() throws SourceException {

    Set<EphemerisTestCodes> codeSet = new HashSet<EphemerisTestCodes>();
    List<PositionVectorFunction> sources = new LinkedList<>();
    List<ConstantPositionVectorFunction> functions =
        new LinkedList<ConstantPositionVectorFunction>();
    Map<EphemerisCodePair, List<ConstantPositionVectorFunction>> map =
        new HashMap<EphemerisCodePair, List<ConstantPositionVectorFunction>>();
    List<FrameTransformFunction> frameSources;

    ConstantStateVectorFunction function = new ConstantStateVectorFunction(E_AA, E_AB, R,
        Coverage.ALL_TIME, new StateVector(new VectorIJK(10, 20, 30), new VectorIJK(1, 2, 5)));

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    function = new ConstantStateVectorFunction(E_AB, E_AC, AA, Coverage.ALL_TIME,
        new StateVector(new VectorIJK(-1, Math.PI, 12.7), new VectorIJK(-1, 17, 32)));

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    function = new ConstantStateVectorFunction(E_AC, E_R, BA, Coverage.ALL_TIME,
        new StateVector(new VectorIJK(-1.5, 47.2, -345.1), new VectorIJK(2.23, 1.01, 2.3)));

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    function = new ConstantStateVectorFunction(E_BA, E_BB, R, Coverage.ALL_TIME,
        new StateVector(new VectorIJK(27.2, 12314.1, -4501.123), new VectorIJK(6.28, 31.7, 16.24)));

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    function = new ConstantStateVectorFunction(E_BB, E_R, AC, Coverage.ALL_TIME,
        new StateVector(new VectorIJK(27.2, 12314.1, -4501.123), new VectorIJK(6.28, 31.7, 16.24)));

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    sources.addAll(functions);
    functions.clear();

    function = new ConstantStateVectorFunction(E_XB, E_XA, R, Coverage.ALL_TIME, new StateVector());

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    sources.addAll(functions);
    functions.clear();

    frameSources = createStaticFrameSources();
    staticTreeRotatedProvider = createProvider(sources, frameSources);
    staticTreeRotatedFrameSources = frameSources;
    staticTreeRotatedContent = map;
    staticTreeRotatedSourceList = sources;
    staticTreeRotatedCodes = codeSet;
  }

  @Test
  public void testTimeForwardStaticTreeRotatedStateLookup() throws SourceException {

    StateVectorFunction function =
        staticTreeRotatedProvider.createStateVectorFunction(E_BB, E_AA, R, Coverage.ALL_TIME);

    assertEquals(E_BB, function.getTargetID());
    assertEquals(E_AA, function.getObserverID());
    assertEquals(R, function.getFrameID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;

    VectorIJK retVec = function.getPosition(time, vector);
    assertSame(retVec, vector);

    computeChain(state, time, R, staticTreeRotatedProvider.getFrameProvider(),
        staticTreeRotatedContent, E_BB, E_R);
    computeChain(testState, time, R, staticTreeRotatedProvider.getFrameProvider(),
        staticTreeRotatedContent, E_AA, E_AB, E_AC, E_R);
    StateVector.subtract(state, testState, state);

    assertComponentRelativeEquality(state.getPosition(), vector, TIGHT_TOLERANCE);

    StateVector retState = function.getState(time, state);
    assertSame(retState, state);

    computeChain(testState, time, R, staticTreeRotatedProvider.getFrameProvider(),
        staticTreeRotatedContent, E_BB, E_R);
    computeChain(altState, time, R, staticTreeRotatedProvider.getFrameProvider(),
        staticTreeRotatedContent, E_AA, E_AB, E_AC, E_R);
    StateVector.subtract(testState, altState, testState);

    assertComponentRelativeEquality(testState, state, LOOSE_TOLERANCE);

  }

  @Test
  public void testTimeForwardStaticTreeRotatedPositionLookup() throws SourceException {

    PositionVectorFunction function =
        staticTreeRotatedProvider.createPositionVectorFunction(E_BB, E_AA, R, Coverage.ALL_TIME);

    assertEquals(E_BB, function.getTargetID());
    assertEquals(E_AA, function.getObserverID());
    assertEquals(R, function.getFrameID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;

    VectorIJK retVec = function.getPosition(time, vector);
    assertSame(retVec, vector);

    computeChain(testVector, time, R, staticTreeRotatedProvider.getFrameProvider(),
        staticTreeRotatedContent, E_BB, E_R);
    computeChain(altVector, time, R, staticTreeRotatedProvider.getFrameProvider(),
        staticTreeRotatedContent, E_AA, E_AB, E_AC, E_R);
    VectorIJK.subtract(testVector, altVector, testVector);

    assertComponentRelativeEquality(testVector, vector, TIGHT_TOLERANCE);
  }

  @Test
  public void testNullBufferPositionVectorFunction() throws SourceException {

    PositionVectorFunction function = staticTreePureStateVectorProvider
        .createPositionVectorFunction(E_BB, E_AA, R, Coverage.ALL_TIME);

    time += 5;
    VectorIJK retVec = function.getPosition(time, vector);
    assertSame(retVec, vector);

    VectorIJK result = function.getPosition(time, null);
    assertNotSame(retVec, result);

    assertEquivalentVector(retVec, result);

  }

  @Test
  public void testNullBufferStateVectorFunction() throws SourceException {

    StateVectorFunction function = staticTreePureStateVectorProvider.createStateVectorFunction(E_BB,
        E_AA, R, Coverage.ALL_TIME);

    time += 5;
    VectorIJK retVec = function.getPosition(time, vector);
    assertSame(retVec, vector);

    VectorIJK result = function.getPosition(time, null);
    assertNotSame(retVec, result);

    assertEquivalentVector(retVec, result);

    StateVector retState = function.getState(time, state);
    assertSame(retState, state);

    StateVector resultState = function.getState(time, null);
    assertNotSame(retState, result);

    assertEquivalentStateVector(retState, resultState);

  }


}


@SuppressWarnings("unused")
class CoverageInterval implements Coverage {

  private double start;
  private double stop;

  public CoverageInterval() {

  }

  public CoverageInterval(double time) {
    this.start = time;
    this.stop = time;
  }

  public void setTimes(double start, double stop) {
    this.start = start;
    this.stop = stop;
  }

  @Override
  public boolean contains(double time) {
    return (time <= start) && (time >= stop);
  }

  @Override
  public Interval getBoundingInterval(Interval buffer) {
    throw new UnsupportedOperationException();
  }

  @Override
  public Interval getBracketingInterval(double t, Interval buffer) {
    throw new UnsupportedOperationException();
  }

  @Override
  public boolean hasNextInterval(double time) {
    throw new UnsupportedOperationException();
  }

  @Override
  public Interval getNextInterval(double time, Interval buffer) {
    throw new UnsupportedOperationException();
  }

  @Override
  public boolean equals(Object o) {
    throw new UnsupportedOperationException();
  }

  @Override
  public int hashCode() {
    throw new UnsupportedOperationException();
  }
}
