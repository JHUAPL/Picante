package picante.mechanics;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static picante.junit.AssertTools.assertComponentRelativeEquality;
import static picante.junit.AssertTools.assertEquivalentMatrix;
import static picante.junit.AssertTools.assertEquivalentStateTransform;
import static picante.mechanics.FrameTestCodes.AA;
import static picante.mechanics.FrameTestCodes.AB;
import static picante.mechanics.FrameTestCodes.AC;
import static picante.mechanics.FrameTestCodes.BA;
import static picante.mechanics.FrameTestCodes.BB;
import static picante.mechanics.FrameTestCodes.R;
import static picante.mechanics.FrameTestCodes.UNK;
import static picante.mechanics.FrameTestCodes.XA;
import static picante.mechanics.FrameTestCodes.XB;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Set;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.RotationMatrixIJK;

/**
 * This abstract class is designed to accelerate testing of various implementations of the
 * FrameProvider interface. It will be grown &quot;organically&quot; as new implementations of the
 * interface arise, bringing additional complications to the test framework.
 * <p>
 * Any methods marked as abstract will require implementation in the actual test case; and read the
 * notes in the javadoc comments carefully. They will describe any expectations the tests defined on
 * this class will have above and beyond the interface specification.
 * </p>
 * <p>
 * If you wish to insert your own setup or teardown methods, make certain that if you elect to
 * override the methods in this class (rather than defining your own) that you invoke them as
 * appropriate. They configure the test data fed into the specific implementation.
 * </p>
 * <p>
 * When adding test cases that exercise generic functionality of the frame provider methods that
 * create transform functions, in the line just prior to executing a transform lookup, augment the
 * time supplied by 5. This will prevent any buffering scheme from being invoked as every query to
 * the provider will be with a new, unseen time.
 * </p>
 * <p>
 * This class defines three test scenarios and the data necessary to perform the test execution.
 * <ul>
 * <li>StaticTreePureStateTransformProvider</li>
 * <li>StaticTreePureFrameTransformProvider</li>
 * <li>StaticTreeMixedProvider</li>
 * </ul>
 * These three scenarios are used to exercise a variety of the functionality of each implementation
 * of the <code>FrameProvider</code> interface.
 * </p>
 * <p>
 * <b>StaticTreePureStateTransformProvider</b> is a frame provider that contains only
 * <code>StateTransformFunction</code>s, and they are valid for all time. Each transform loaded into
 * the provider defines a frame in the tree and there is no redundancy. This allows the nominal
 * execution of the state chaining and frame chaining code to be properly exercised. Specific tests
 * utilizing this test data set are named: test*StaticTreePureStateProvider*().
 * </p>
 * <p>
 * <b>StaticTreePureFrameTransformProvider</b> is a frame provider that mirrors the
 * StaticTreePureStateTransformProvider in the functions it defines; however, it consists only of
 * <code>FrameTransformFunction</code>s. These transforms are supplied to the load method and will
 * be utilized by tests to exercise the frame transformation components of the interface. Specifc
 * tests utilizing this data set are named: test*StaticTreePureFrameProvider*().
 * </p>
 * <p>
 * <b>StaticTreeMixedProvider</b> is a frame provider that contains a mix of state and frame
 * transform functions. They are also static and valid for all time; however, this particular
 * provider has a frame transform function that has higher priority than a state transform function
 * providing the same frame definition. This permits various tests to be executed on the frame and
 * state transform creation methods to insure that they link through the appropriate functions.
 * </p>
 * 
 *         TODO: Figure out how to handle coverage comparisons, simply checking against
 *         Coverage.ALL_TIME works now, due to how the underlying provider is implemented.
 */
public abstract class FrameProviderTest {

  private static final double TOLERANCE = 1.0E-14;
  private static final double TIGHT_TOLERANCE = 1.0E-15;

  protected FrameProvider staticTreePureStateTransformProvider;
  protected Map<FrameCodePair, List<ConstantFrameTransformFunction>> staticTreePureStateTransformContent;
  protected List<FrameTransformFunction> staticTreePureStateTransformSourceList;
  protected Set<FrameTestCodes> staticTreePureStateTransformCodes;

  protected FrameProvider staticTreePureFrameTransformProvider;
  protected Map<FrameCodePair, List<ConstantFrameTransformFunction>> staticTreePureFrameTransformContent;
  protected List<FrameTransformFunction> staticTreePureFrameTransformSourceList;
  protected Set<FrameTestCodes> staticTreePureFrameTransformCodes;

  protected FrameProvider staticTreeMixedProvider;
  protected Map<FrameCodePair, List<ConstantFrameTransformFunction>> staticTreeMixedContent;
  protected List<FrameTransformFunction> staticTreeMixedSourceList;
  protected Set<FrameTestCodes> staticTreeMixedCodes;

  protected StateTransform transform = new StateTransform();
  protected RotationMatrixIJK rotation = new RotationMatrixIJK();

  protected StateTransform testTransform = new StateTransform();
  protected RotationMatrixIJK testRotation = new RotationMatrixIJK();

  protected StateTransform altTransform = new StateTransform();
  protected RotationMatrixIJK altRotation = new RotationMatrixIJK();

  protected double time = 0.0;

  /**
   * Create a provider, installing the appropriate sources by whatever mechanism is required in the
   * particular implementation of the interface.
   * 
   * @param sources a list of frame sources
   * 
   * @return a newly created implementation of the <code>FrameProvider</code> interface
   */
  public abstract FrameProvider createProvider(List<? extends FrameTransformFunction> sources);


  @Before
  public void setUp() throws Exception {
    clearBuffers();
    populateStaticTreePureStateTransformProvider();
    populateStaticTreePureFrameTransformProvider();
    populateStaticTreeMixedProvider();
  }

  private void clearBuffers() {
    transform.setTo(StateTransform.IDENTITY);
    rotation.setTo(RotationMatrixIJK.IDENTITY);
    testTransform.setTo(StateTransform.IDENTITY);
    testRotation.setTo(RotationMatrixIJK.IDENTITY);
    altTransform.setTo(StateTransform.IDENTITY);
    altRotation.setTo(RotationMatrixIJK.IDENTITY);
  }

  /**
   * Adds the codes from a given transform function to the set of test codes.
   * 
   * @param function
   * @param codeSet
   */
  protected void addContentToSet(ConstantFrameTransformFunction function,
      Set<FrameTestCodes> codeSet) {
    codeSet.add(function.getFromID());
    codeSet.add(function.getToID());
  }

  /**
   * Adds the function to a map connecting various frame code pairings to a list of functions
   * associated with that pairing.
   * 
   * @param function
   * @param map
   */
  protected void addContentToMap(ConstantFrameTransformFunction function,
      Map<FrameCodePair, List<ConstantFrameTransformFunction>> map) {

    FrameCodePair pair = new FrameCodePair(function.getFromID(), function.getToID());

    if (map.containsKey(pair)) {
      map.get(pair).add(function);
      return;
    }

    LinkedList<ConstantFrameTransformFunction> newList =
        new LinkedList<ConstantFrameTransformFunction>();
    newList.add(function);

    map.put(pair, newList);
  }

  /**
   * Retrieves the desired frame transform function from the supplied map. It evaluates the list
   * contents to determine the highest priority mapping that is applicable at the requested time.
   * 
   * @param fromID
   * @param toID
   * @param time
   * @param map
   * @return
   */
  private ConstantFrameTransformFunction getContentFromMap(FrameTestCodes fromID,
      FrameTestCodes toID, double time,
      Map<FrameCodePair, List<ConstantFrameTransformFunction>> map) {
    FrameCodePair pair = new FrameCodePair(fromID, toID);

    if (map.containsKey(pair)) {
      List<ConstantFrameTransformFunction> functions = map.get(pair);

      ListIterator<ConstantFrameTransformFunction> iterator =
          functions.listIterator(functions.size());

      while (iterator.hasPrevious()) {
        ConstantFrameTransformFunction function = iterator.previous();
        if (function.getCoverage().contains(time)) {
          return function;
        }
      }

    }
    return null;

  }

  /**
   * Retrieves the desired state transform function from the supplied map. It evaluates the list
   * contents to determine the highest priority mapping that is applicable at the requested time.
   * 
   * @param fromID
   * @param toID
   * @param time
   * @param map
   * @return
   */
  private ConstantStateTransformFunction getStateContentFromMap(FrameTestCodes fromID,
      FrameTestCodes toID, double time,
      Map<FrameCodePair, List<ConstantFrameTransformFunction>> map) {
    FrameCodePair pair = new FrameCodePair(fromID, toID);

    if (map.containsKey(pair)) {
      List<ConstantFrameTransformFunction> functions = map.get(pair);

      ListIterator<ConstantFrameTransformFunction> iterator =
          functions.listIterator(functions.size());

      while (iterator.hasPrevious()) {
        ConstantFrameTransformFunction function = iterator.previous();
        if ((function.getCoverage().contains(time))
            && (function instanceof ConstantStateTransformFunction)) {
          return (ConstantStateTransformFunction) function;
        }
      }

    }
    return null;

  }

  /**
   * Given a buffer to capture the results, a time of interest, the map containing the test
   * transform functions, and a list of codes to connect compute the frame transform.
   * 
   * @param buffer
   * @param time
   * @param map
   * @param codes
   * @return
   */
  protected RotationMatrixIJK computeChain(RotationMatrixIJK buffer, double time,
      Map<FrameCodePair, List<ConstantFrameTransformFunction>> map, FrameTestCodes... codes) {

    FrameTestCodes from = codes[0];
    FrameTestCodes to;

    buffer.setTo(RotationMatrixIJK.IDENTITY);

    RotationMatrixIJK tmp = new RotationMatrixIJK();

    for (int i = 1; i < codes.length; i++) {
      to = codes[i];

      FrameTransformFunction function = getContentFromMap(from, to, time, map);

      function.getTransform(time, tmp);
      RotationMatrixIJK.mxm(tmp, buffer, buffer);

      from = to;
    }

    return buffer;

  }

  /**
   * Given a buffer to capture the results, a time of interest, the map containing the test
   * transform functions, and a list of codes to connect compute the state transform.
   * 
   * @param buffer
   * @param time
   * @param map
   * @param codes
   * @return
   */
  protected StateTransform computeChain(StateTransform buffer, double time,
      Map<FrameCodePair, List<ConstantFrameTransformFunction>> map, FrameTestCodes... codes) {

    FrameTestCodes from = codes[0];
    FrameTestCodes to;

    buffer.setTo(StateTransform.IDENTITY);

    StateTransform tmp = new StateTransform();

    for (int i = 1; i < codes.length; i++) {
      to = codes[i];

      StateTransformFunction function = getStateContentFromMap(from, to, time, map);

      function.getStateTransform(time, tmp);
      StateTransform.mxm(tmp, buffer, buffer);

      from = to;
    }

    return buffer;

  }

  /**
   * As test data sets are added to the class, include them in this test of the get sources in load
   * order method.
   */
  @Test
  public void testGetSourcesInLoadOrder() {

    List<FrameTransformFunction> returnedList;
    returnedList = staticTreePureStateTransformProvider.getFrameSourcesInLoadOrder();
    assertEquals(staticTreePureStateTransformSourceList, returnedList);

    returnedList = staticTreePureFrameTransformProvider.getFrameSourcesInLoadOrder();
    assertEquals(staticTreePureFrameTransformSourceList, returnedList);

    returnedList = staticTreeMixedProvider.getFrameSourcesInLoadOrder();
    assertEquals(staticTreeMixedSourceList, returnedList);
  }


  /**
   * As test data sets are added to the class, include them in this test of the get known frames
   * method.
   */
  @Test
  public void testGetKnownFrames() {

    HashSet<FrameID> set = new HashSet<FrameID>();
    Set<FrameID> returnedSet;

    returnedSet = staticTreePureStateTransformProvider.getKnownFrames(set);
    assertSame(returnedSet, set);
    assertEquals(staticTreePureStateTransformCodes, returnedSet);
    set.clear();

    returnedSet = staticTreePureFrameTransformProvider.getKnownFrames(set);
    assertSame(returnedSet, set);
    assertEquals(staticTreePureFrameTransformCodes, returnedSet);
    set.clear();

    returnedSet = staticTreeMixedProvider.getKnownFrames(set);
    assertSame(returnedSet, set);
    assertEquals(staticTreeMixedCodes, returnedSet);
    set.clear();
  }

  /**
   * Verifies that the get known frames method unions the contents of the frame provider with the
   * contents of whatever set is supplied it.
   */
  @Test
  public void testGetKnownFramesUnion() {

    HashSet<FrameID> set = new HashSet<FrameID>();
    Set<FrameID> returnedSet;
    HashSet<FrameID> testSet = new HashSet<FrameID>();

    set.add(FrameTestCodes.UNK);
    testSet.addAll(staticTreePureStateTransformCodes);
    testSet.add(FrameTestCodes.UNK);
    returnedSet = staticTreePureStateTransformProvider.getKnownFrames(set);
    assertSame(returnedSet, set);
    assertEquals(testSet, returnedSet);
    set.clear();
    testSet.clear();

    set.add(FrameTestCodes.UNK);
    testSet.addAll(staticTreePureFrameTransformCodes);
    testSet.add(FrameTestCodes.UNK);
    returnedSet = staticTreePureFrameTransformProvider.getKnownFrames(set);
    assertSame(returnedSet, set);
    assertEquals(testSet, returnedSet);
    set.clear();
    testSet.clear();

    set.add(FrameTestCodes.UNK);
    testSet.addAll(staticTreeMixedCodes);
    testSet.add(FrameTestCodes.UNK);
    returnedSet = staticTreeMixedProvider.getKnownFrames(set);
    assertSame(returnedSet, set);
    assertEquals(testSet, returnedSet);
    set.clear();
    testSet.clear();

  }

  /**
   * As test data sets are added to the class, include them in the test of the is aware of method.
   */
  @Test
  public void testIsAwareOf() {
    for (FrameTestCodes code : staticTreePureStateTransformCodes) {
      assertTrue(staticTreePureStateTransformProvider.isAwareOf(code));
    }
    assertFalse(staticTreePureStateTransformProvider.isAwareOf(FrameTestCodes.UNK));

    for (FrameTestCodes code : staticTreePureFrameTransformCodes) {
      assertTrue(staticTreePureFrameTransformProvider.isAwareOf(code));
    }
    assertFalse(staticTreePureFrameTransformProvider.isAwareOf(FrameTestCodes.UNK));

    for (FrameTestCodes code : staticTreeMixedCodes) {
      assertTrue(staticTreeMixedProvider.isAwareOf(code));
    }
    assertFalse(staticTreeMixedProvider.isAwareOf(UNK));

  }

  private void populateStaticTreePureStateTransformProvider() {

    Set<FrameTestCodes> codeSet = new HashSet<FrameTestCodes>();
    List<FrameTransformFunction> sources = new LinkedList<>();
    List<ConstantStateTransformFunction> functions =
        new LinkedList<ConstantStateTransformFunction>();
    Map<FrameCodePair, List<ConstantFrameTransformFunction>> map =
        new HashMap<FrameCodePair, List<ConstantFrameTransformFunction>>();

    ConstantStateTransformFunction function =
        new ConstantStateTransformFunction(AA, AB, Coverage.ALL_TIME,
            new StateTransform(
                new RotationMatrixIJK(0.23262501680698122, -0.11345880161244597, 0.9659258262890683,
                    0.6851945196756121, 0.7239556287790326, -0.0799794834045749,
                    -0.6902130625843436, 0.6804523112576097, 0.2461515393860416),
                new MatrixIJK(0.0060232452411852384, -0.005938066617125284, -0.0021480774105694634,
                    0.012046490482370477, -0.011876133234250567, -0.004296154821138927,
                    0.013988936617854983, 0.011645294533787818, 0.007033387679941524)));

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    function = new ConstantStateTransformFunction(AB, AC, Coverage.ALL_TIME,
        new StateTransform(
            new RotationMatrixIJK(0.46865981337225204, -0.5847388119079097, 0.6621468879171843,
                0.5103093929582195, 0.7910490458898228, 0.33738069069734045, -0.7210702481648369,
                0.17978300488457546, 0.6691306063588582),
            new MatrixIJK(0.019362795617574476, 0.0753076627264154, 0.052799083840262336,
                -0.01572818413834173, 0.04475247691123446, -0.0811402810436, 0.0014538445916930972,
                0.04802405585505995, -0.011336478881335063)));

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    function = new ConstantStateTransformFunction(AC, R, Coverage.ALL_TIME,
        new StateTransform(
            new RotationMatrixIJK(0.08348014257622213, -0.0629067995414709, 0.9945218953682733,
                0.6291677054416244, 0.7772610183154323, -0.0036479907591269655, -0.7727736177075243,
                0.6260255937090003, 0.10446478735209536),
            new MatrixIJK(-0.037955936010262696, 0.008286651706940469, 0.0037101784332086787,
                0.0014570033479893777, -0.0010979307738907262, 0.017357681557350912,
                -0.0029140066959787553, 0.0021958615477814525, -0.034715363114701824)));

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    sources.addAll(functions);
    functions.clear();

    function = new ConstantStateTransformFunction(BA, BB, Coverage.ALL_TIME,
        new StateTransform(
            new RotationMatrixIJK(0.9781476007338057, -0.20636194860240742, 0.02533806124621436,
                0.05029839035996436, 0.11662217487111268, -0.991901880356887, 0.20173582504328824,
                0.9715009081299661, 0.12445337438878395),
            new MatrixIJK(0.009246054318345706, 0.04781450720044231, 0.03248429394833022,
                0.03617070740362956, 0.045465125418033596, 0.007179712337132024,
                -0.0538493061705677, 0.004698763564817432, 0.050609163222396385)));

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    function = new ConstantStateTransformFunction(BB, R, Coverage.ALL_TIME,
        new StateTransform(
            new RotationMatrixIJK(-0.7009436628498646, -0.6793378264128004, 0.2172052004795177,
                0.6582394338705277, -0.7334323383185172, -0.16969930113715712, 0.27458847246092255,
                0.024023378475231092, 0.9612616959383189),
            new MatrixIJK(-0.7466815019357148, 0.9010880847202829, 0.40865298683151163,
                -0.808439505815695, -0.8257748557786994, 0.43313802873181917, 0.03192084357758847,
                0.27028827309353176, -0.015873235378095915)));

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    sources.addAll(functions);
    functions.clear();

    function = new ConstantStateTransformFunction(XB, XA, Coverage.ALL_TIME, new StateTransform());

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    sources.addAll(functions);
    functions.clear();

    staticTreePureStateTransformProvider = createProvider(sources);
    staticTreePureStateTransformContent = map;
    staticTreePureStateTransformSourceList = sources;
    staticTreePureStateTransformCodes = codeSet;
  }

  /**
   * Test exercising that the provider responds properly to requests for a frame transformation
   * involving a code unknown to the provider.
   */
  @Test
  public void testTimeForwardStaticTreePureStateProviderUnknownFrame() {

    /*
     * There are two possible outcomes, the first is that the provider throws an exception when
     * attempting to create the function, the other is a runtime evaluation exception gets generated
     * when asking for a transform. Handle both cases.
     */
    try {
      FrameTransformFunction function = staticTreePureStateTransformProvider
          .createFrameTransformFunction(AA, UNK, Coverage.ALL_TIME);

      assertEquals(AA, function.getFromID());
      assertEquals(UNK, function.getToID());

      time += 5;

      try {
        function.getTransform(time, rotation);
      } catch (FrameEvaluationException e) {
        return;
      }

      fail("Expected evaluation exception not thrown.");

    } catch (SourceException e) {
      return;
    }

    fail("Expected source exception not thrown.");

  }

  /**
   * Test exercising that the provider responds properly to requests for a state transformation
   * involving a code unknown to the provider.
   */
  @Test
  public void testTimeForwardStaticTreePureStateProviderUnknownState() {

    /*
     * There are two possible outcomes, the first is that the provider throws an exception when
     * attempting to create the function, the other is a runtime evaluation exception gets generated
     * when asking for a transform. Handle both cases.
     */
    try {
      StateTransformFunction function = staticTreePureStateTransformProvider
          .createStateTransformFunction(AA, UNK, Coverage.ALL_TIME);

      assertEquals(AA, function.getFromID());
      assertEquals(UNK, function.getToID());

      time += 5;

      try {
        function.getStateTransform(time, transform);
      } catch (FrameEvaluationException e) {
        return;
      }

      fail("Expected evaluation exception not thrown.");

    } catch (SourceException e) {
      return;
    }

    fail("Expected source exception not thrown.");

  }

  /**
   * Test exercising that the provider responds properly to requests for a state transformation
   * involving a code unknown to the provider that is then utilized to compute a frame transform.
   */
  @Test
  public void testTimeForwardStaticTreePureStateProviderUnknownFrameFromState() {

    /*
     * There are two possible outcomes, the first is that the provider throws an exception when
     * attempting to create the function, the other is a runtime evaluation exception gets generated
     * when asking for a transform. Handle both cases.
     */
    try {
      StateTransformFunction function = staticTreePureStateTransformProvider
          .createStateTransformFunction(AA, UNK, Coverage.ALL_TIME);

      assertEquals(AA, function.getFromID());
      assertEquals(UNK, function.getToID());

      time += 5;

      try {
        function.getTransform(time, rotation);
      } catch (FrameEvaluationException e) {
        return;
      }

      fail("Expected evaluation exception not thrown.");

    } catch (SourceException e) {
      return;
    }

    fail("Expected source exception not thrown.");

  }

  /**
   * This test exercises whether the provider properly handles the loopback exceptional case, where
   * two equivalent codes are used as the from and to ID arguments.
   * 
   * @throws SourceException
   */
  @Test
  public void testTimeForwardStaticTreePureStateProviderLoopbackLinkFrame() throws SourceException {

    FrameTransformFunction function = staticTreePureStateTransformProvider
        .createFrameTransformFunction(AA, AA, Coverage.ALL_TIME);

    assertEquals(AA, function.getFromID());
    assertEquals(AA, function.getToID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;
    RotationMatrixIJK returned = function.getTransform(time, rotation);
    assertSame(returned, rotation);
    assertEquivalentMatrix(RotationMatrixIJK.IDENTITY, rotation);

  }

  /**
   * This test exercises whether the provider properly creates a frame transform function that links
   * through a single frame transform function loaded into the provider.
   * 
   * @throws SourceException
   */
  @Test
  public void testTimeForwardStaticTreePureStateProviderSingleForwardLinkFrame()
      throws SourceException {

    FrameTransformFunction function = staticTreePureStateTransformProvider
        .createFrameTransformFunction(AA, AB, Coverage.ALL_TIME);

    assertEquals(AA, function.getFromID());
    assertEquals(AB, function.getToID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;
    RotationMatrixIJK returned = function.getTransform(time, rotation);
    assertSame(returned, rotation);

    /*
     * Test this is simple as it is merely a single node. Retrieve it from the map.
     */
    ConstantFrameTransformFunction testFunction =
        getContentFromMap(AA, AB, time, staticTreePureStateTransformContent);

    testFunction.getTransform(time, testRotation);
    assertEquivalentMatrix(testRotation, rotation);

  }

  /**
   * This test exercises whether the provider handles the case where a frame transform is selected
   * individually from the list but applied in the reverse direction from the native definition
   * loaded into the provider.
   * 
   * @throws SourceException
   */
  @Test
  public void testTimeForwardStaticTreePureStateProviderSingleBackwardLinkFrame()
      throws SourceException {

    FrameTransformFunction function = staticTreePureStateTransformProvider
        .createFrameTransformFunction(AB, AA, Coverage.ALL_TIME);

    assertEquals(AB, function.getFromID());
    assertEquals(AA, function.getToID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;
    RotationMatrixIJK returned = function.getTransform(time, rotation);
    assertSame(returned, rotation);

    /*
     * Test this is simple as it is merely a single node. Retrieve it from the map.
     */
    ConstantFrameTransformFunction testFunction =
        getContentFromMap(AA, AB, time, staticTreePureStateTransformContent);

    testFunction.getTransform(time, testRotation);
    assertEquivalentMatrix(testRotation.transpose(), rotation);

  }

  /**
   * This test exercises whether the provider properly connects a leaf node to the top of its
   * section of the frame tree.
   * 
   * @throws SourceException
   */
  @Test
  public void testTimeForwardStaticTreePureStateProviderSingleLeafForwardLinkFrame()
      throws SourceException {

    FrameTransformFunction function =
        staticTreePureStateTransformProvider.createFrameTransformFunction(AA, R, Coverage.ALL_TIME);

    assertEquals(AA, function.getFromID());
    assertEquals(R, function.getToID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;
    RotationMatrixIJK returned = function.getTransform(time, rotation);
    assertSame(returned, rotation);

    computeChain(transform, time, staticTreePureStateTransformContent, AA, AB, AC, R);
    assertEquivalentMatrix(transform.getRotation(), rotation);

  }

  /**
   * This test exercises whether the provider properly connects the root node of a section of the
   * frame tree to one of its links.
   * 
   * @throws SourceException
   */
  @Test
  public void testTimeForwardStaticTreePureStateProviderSingleLeafBackwardLinkFrame()
      throws SourceException {

    FrameTransformFunction function =
        staticTreePureStateTransformProvider.createFrameTransformFunction(R, AA, Coverage.ALL_TIME);

    assertEquals(R, function.getFromID());
    assertEquals(AA, function.getToID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;
    RotationMatrixIJK returned = function.getTransform(time, rotation);
    assertSame(returned, rotation);

    computeChain(transform, time, staticTreePureStateTransformContent, AA, AB, AC, R);
    assertComponentRelativeEquality(transform.getRotation().createTranspose(), rotation,
        TIGHT_TOLERANCE);

  }

  /**
   * This test exercises whether the provider properly connects two leaf nodes sharing a common
   * root.
   * 
   * @throws SourceException
   */
  @Test
  public void testTimeForwardStaticTreePureStateProviderFullLinkFrame() throws SourceException {

    FrameTransformFunction function = staticTreePureStateTransformProvider
        .createFrameTransformFunction(AA, BA, Coverage.ALL_TIME);

    assertEquals(AA, function.getFromID());
    assertEquals(BA, function.getToID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;
    RotationMatrixIJK returned = function.getTransform(time, rotation);
    assertSame(returned, rotation);

    computeChain(transform, time, staticTreePureStateTransformContent, AA, AB, AC, R);
    computeChain(testTransform, time, staticTreePureStateTransformContent, BA, BB, R);
    StateTransform.mixm(testTransform, transform, transform);

    assertEquivalentMatrix(transform.getRotation(), rotation);

  }

  /**
   * This test exercises whether the provider properly fails when it is unable to determine a
   * connection between the nodes at the supplied time.
   */
  @Test
  public void testTimeForwardStaticTreePureStateProviderBrokenLinkRotationFrame() {

    /*
     * There are two possible outcomes, the first is that the provider throws an exception when
     * attempting to create the function, the other is a runtime evaluation exception gets generated
     * when asking for a transform. Handle both cases.
     */
    try {
      FrameTransformFunction function = staticTreePureStateTransformProvider
          .createFrameTransformFunction(AA, XB, Coverage.ALL_TIME);

      assertEquals(AA, function.getFromID());
      assertEquals(XB, function.getToID());

      time += 5;

      try {
        function.getTransform(time, rotation);
      } catch (FrameEvaluationException e) {
        return;
      }

      fail("Expected evaluation exception not thrown.");

    } catch (SourceException e) {
      return;
    }

    fail("Expected source exception not thrown.");
  }

  /**
   * Test exercising that the provider responds properly to requests for a state transformation
   * involving a code unknown to the provider.
   */
  @Test
  public void testTimeForwardStaticTreePureStateProviderLoopbackLinkState() throws SourceException {

    StateTransformFunction function = staticTreePureStateTransformProvider
        .createStateTransformFunction(AA, AA, Coverage.ALL_TIME);

    assertEquals(AA, function.getFromID());
    assertEquals(AA, function.getToID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;
    RotationMatrixIJK retRot = function.getTransform(time, rotation);
    assertSame(retRot, rotation);
    assertEquivalentMatrix(RotationMatrixIJK.IDENTITY, rotation);

    time += 5;
    StateTransform retState = function.getStateTransform(time, transform);
    assertSame(retState, transform);
    assertEquivalentStateTransform(StateTransform.IDENTITY, transform);

  }

  /**
   * This test exercises whether the provider properly creates a state transform function that links
   * through a single frame transform function loaded into the provider.
   * 
   * @throws SourceException
   */
  @Test
  public void testTimeForwardStaticTreePureStateProviderSingleForwardLinkState()
      throws SourceException {

    StateTransformFunction function = staticTreePureStateTransformProvider
        .createStateTransformFunction(AA, AB, Coverage.ALL_TIME);

    assertEquals(AA, function.getFromID());
    assertEquals(AB, function.getToID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;
    RotationMatrixIJK retRot = function.getTransform(time, rotation);
    assertSame(retRot, rotation);
    /*
     * Test this is simple as it is merely a single node. Retrieve it from the map.
     */
    ConstantFrameTransformFunction testFunction =
        getContentFromMap(AA, AB, time, staticTreePureStateTransformContent);

    testFunction.getTransform(time, testRotation);
    assertEquivalentMatrix(testRotation, rotation);

    time += 5;
    StateTransform retState = function.getStateTransform(time, transform);
    assertSame(retState, transform);

    ConstantStateTransformFunction testStateFunction =
        getStateContentFromMap(AA, AB, time, staticTreePureStateTransformContent);
    testStateFunction.getStateTransform(time, testTransform);

    assertEquivalentStateTransform(testTransform, transform);
  }

  /**
   * This test exercises whether the provider handles the case where a state transform is selected
   * individually from the list but applied in the reverse direction from the native definition
   * loaded into the provider.
   * 
   * @throws SourceException
   */
  @Test
  public void testTimeForwardStaticTreePureStateProviderSingleBackwardLinkState()
      throws SourceException {

    StateTransformFunction function = staticTreePureStateTransformProvider
        .createStateTransformFunction(AB, AA, Coverage.ALL_TIME);

    assertEquals(AB, function.getFromID());
    assertEquals(AA, function.getToID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;
    RotationMatrixIJK retRot = function.getTransform(time, rotation);
    assertSame(retRot, rotation);

    /*
     * Test this is simple as it is merely a single node. Retrieve it from the map.
     */
    ConstantFrameTransformFunction testFunction =
        getContentFromMap(AA, AB, time, staticTreePureStateTransformContent);

    testFunction.getTransform(time, testRotation);
    assertEquivalentMatrix(testRotation.transpose(), rotation);

    time += 5;
    StateTransform retState = function.getStateTransform(time, transform);
    assertSame(retState, transform);

    ConstantStateTransformFunction testStateFunction =
        getStateContentFromMap(AA, AB, time, staticTreePureStateTransformContent);
    testStateFunction.getStateTransform(time, testTransform);

    assertEquivalentStateTransform(testTransform.invert(), transform);

  }

  /**
   * This test exercises whether the provider properly connects a leaf node to the top of its
   * section of the state tree.
   * 
   * @throws SourceException
   */
  @Test
  public void testTimeForwardStaticTreePureStateProviderSingleLeafForwardLinkState()
      throws SourceException {

    StateTransformFunction function =
        staticTreePureStateTransformProvider.createStateTransformFunction(AA, R, Coverage.ALL_TIME);

    assertEquals(AA, function.getFromID());
    assertEquals(R, function.getToID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;
    RotationMatrixIJK retRot = function.getTransform(time, rotation);
    assertSame(retRot, rotation);

    computeChain(transform, time, staticTreePureStateTransformContent, AA, AB, AC, R);
    assertEquivalentMatrix(transform.getRotation(), rotation);

    time += 5;
    StateTransform retState = function.getStateTransform(time, transform);
    assertSame(retState, transform);

    computeChain(testTransform, time, staticTreePureStateTransformContent, AA, AB, AC, R);
    assertEquivalentStateTransform(testTransform, transform);

  }

  /**
   * This test exercises whether the provider properly connects a the root node of a section of the
   * state tree to one of its leaves.
   * 
   * @throws SourceException
   */
  @Test
  public void testTimeForwardStaticTreePureStateProviderSingleLeafBackwardLinkState()
      throws SourceException {

    StateTransformFunction function =
        staticTreePureStateTransformProvider.createStateTransformFunction(R, AA, Coverage.ALL_TIME);

    assertEquals(R, function.getFromID());
    assertEquals(AA, function.getToID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;
    RotationMatrixIJK retRot = function.getTransform(time, rotation);
    assertSame(retRot, rotation);

    computeChain(transform, time, staticTreePureStateTransformContent, AA, AB, AC, R);
    assertComponentRelativeEquality(transform.getRotation().createTranspose(), rotation,
        TIGHT_TOLERANCE);

    time += 5;
    StateTransform retState = function.getStateTransform(time, transform);
    assertSame(retState, transform);

    computeChain(testTransform, time, staticTreePureStateTransformContent, AA, AB, AC, R);
    assertComponentRelativeEquality(testTransform.invert(), transform, TIGHT_TOLERANCE);

  }

  /**
   * This test exercises whether the provider properly connects two leaf nodes sharing a common
   * root.
   * 
   * @throws SourceException
   */
  @Test
  public void testTimeForwardStaticTreePureStateProviderFullLinkState() throws SourceException {

    StateTransformFunction function = staticTreePureStateTransformProvider
        .createStateTransformFunction(AA, BA, Coverage.ALL_TIME);

    assertEquals(AA, function.getFromID());
    assertEquals(BA, function.getToID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;
    RotationMatrixIJK retRot = function.getTransform(time, rotation);
    assertSame(retRot, rotation);

    computeChain(transform, time, staticTreePureStateTransformContent, AA, AB, AC, R);
    computeChain(testTransform, time, staticTreePureStateTransformContent, BA, BB, R);
    StateTransform.mixm(testTransform, transform, transform);

    assertEquivalentMatrix(transform.getRotation(), rotation);

    time += 5;
    StateTransform retState = function.getStateTransform(time, transform);
    assertSame(retState, transform);

    computeChain(testTransform, time, staticTreePureStateTransformContent, AA, AB, AC, R);
    computeChain(altTransform, time, staticTreePureStateTransformContent, BA, BB, R);
    StateTransform.mixm(altTransform, testTransform, testTransform);

    assertComponentRelativeEquality(testTransform, transform, TOLERANCE);

  }

  /**
   * This test exercises whether the provider properly fails when it is unable to determine a
   * connection between the nodes at the supplied time when evaluating a frame transform from a
   * requested state transform.
   */
  @Test
  public void testTimeForwardStaticTreePureStateProviderBrokenLinkRotationState() {

    /*
     * There are two possible outcomes, the first is that the provider throws an exception when
     * attempting to create the function, the other is a runtime evaluation exception gets generated
     * when asking for a transform. Handle both cases.
     */
    try {
      StateTransformFunction function = staticTreePureStateTransformProvider
          .createStateTransformFunction(AA, XB, Coverage.ALL_TIME);

      assertEquals(AA, function.getFromID());
      assertEquals(XB, function.getToID());

      time += 5;

      try {
        function.getTransform(time, rotation);
      } catch (FrameEvaluationException e) {
        return;
      }

      fail("Expected evaluation exception not thrown.");

    } catch (SourceException e) {
      return;
    }

    fail("Expected source exception not thrown.");
  }

  /**
   * This test exercises whether the provider properly fails when it is unable to determine a
   * connection between the nodes at the supplied time, when evaluating a state transform.
   */
  @Test
  public void testTimeForwardStaticTreePureStateProviderBrokenLinkTransformState() {

    /*
     * There are two possible outcomes, the first is that the provider throws an exception when
     * attempting to create the function, the other is a runtime evaluation exception gets generated
     * when asking for a transform. Handle both cases.
     */
    try {
      StateTransformFunction function = staticTreePureStateTransformProvider
          .createStateTransformFunction(AA, XB, Coverage.ALL_TIME);

      assertEquals(AA, function.getFromID());
      assertEquals(XB, function.getToID());

      time += 5;

      try {
        function.getStateTransform(time, transform);
      } catch (FrameEvaluationException e) {
        return;
      }

      fail("Expected evaluation exception not thrown.");

    } catch (SourceException e) {
      return;
    }

    fail("Expected source exception not thrown.");

  }

  private void populateStaticTreePureFrameTransformProvider() {

    Set<FrameTestCodes> codeSet = new HashSet<FrameTestCodes>();
    List<FrameTransformFunction> sources = new LinkedList<>();
    List<ConstantFrameTransformFunction> functions =
        new LinkedList<ConstantFrameTransformFunction>();
    Map<FrameCodePair, List<ConstantFrameTransformFunction>> map =
        new HashMap<FrameCodePair, List<ConstantFrameTransformFunction>>();

    ConstantFrameTransformFunction function =
        new ConstantFrameTransformFunction(AA, AB, Coverage.ALL_TIME,
            new RotationMatrixIJK(0.23262501680698122, -0.11345880161244597, 0.9659258262890683,
                0.6851945196756121, 0.7239556287790326, -0.0799794834045749, -0.6902130625843436,
                0.6804523112576097, 0.2461515393860416));

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    function = new ConstantFrameTransformFunction(AB, AC, Coverage.ALL_TIME,
        new RotationMatrixIJK(0.46865981337225204, -0.5847388119079097, 0.6621468879171843,
            0.5103093929582195, 0.7910490458898228, 0.33738069069734045, -0.7210702481648369,
            0.17978300488457546, 0.6691306063588582));

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    function = new ConstantFrameTransformFunction(AC, R, Coverage.ALL_TIME,
        new RotationMatrixIJK(0.08348014257622213, -0.0629067995414709, 0.9945218953682733,
            0.6291677054416244, 0.7772610183154323, -0.0036479907591269655, -0.7727736177075243,
            0.6260255937090003, 0.10446478735209536));

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    sources.addAll(functions);
    functions.clear();

    function = new ConstantFrameTransformFunction(BA, BB, Coverage.ALL_TIME,
        new RotationMatrixIJK(0.9781476007338057, -0.20636194860240742, 0.02533806124621436,
            0.05029839035996436, 0.11662217487111268, -0.991901880356887, 0.20173582504328824,
            0.9715009081299661, 0.12445337438878395));

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    function = new ConstantFrameTransformFunction(BB, R, Coverage.ALL_TIME,
        new RotationMatrixIJK(-0.7009436628498646, -0.6793378264128004, 0.2172052004795177,
            0.6582394338705277, -0.7334323383185172, -0.16969930113715712, 0.27458847246092255,
            0.024023378475231092, 0.9612616959383189));

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    sources.addAll(functions);
    functions.clear();

    function =
        new ConstantFrameTransformFunction(XB, XA, Coverage.ALL_TIME, new RotationMatrixIJK());

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    sources.addAll(functions);
    functions.clear();

    staticTreePureFrameTransformProvider = createProvider(sources);
    staticTreePureFrameTransformContent = map;
    staticTreePureFrameTransformSourceList = sources;
    staticTreePureFrameTransformCodes = codeSet;
  }

  /**
   * This test exercises whether the provider properly handles the loopback exceptional case, where
   * two equivalent codes are used as the from and to ID arguments.
   * 
   * @throws SourceException
   */
  @Test
  public void testTimeForwardStaticTreePureFrameProviderLoopbackLinkFrame() throws SourceException {

    FrameTransformFunction function = staticTreePureFrameTransformProvider
        .createFrameTransformFunction(AA, AA, Coverage.ALL_TIME);

    assertEquals(AA, function.getFromID());
    assertEquals(AA, function.getToID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;
    RotationMatrixIJK retRot = function.getTransform(time, rotation);
    assertSame(retRot, rotation);
    assertEquivalentMatrix(RotationMatrixIJK.IDENTITY, rotation);

  }

  /**
   * This test exercises whether the provider properly creates a frame transform function that links
   * through a single frame transform function loaded into the provider.
   * 
   * @throws SourceException
   */
  @Test
  public void testTimeForwardStaticTreePureFrameProviderSingleForwardLinkFrame()
      throws SourceException {

    FrameTransformFunction function = staticTreePureFrameTransformProvider
        .createFrameTransformFunction(AA, AB, Coverage.ALL_TIME);

    assertEquals(AA, function.getFromID());
    assertEquals(AB, function.getToID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;
    RotationMatrixIJK retRot = function.getTransform(time, rotation);
    assertSame(retRot, rotation);

    /*
     * Test this is simple as it is merely a single node. Retrieve it from the map.
     */
    ConstantFrameTransformFunction testFunction =
        getContentFromMap(AA, AB, time, staticTreePureFrameTransformContent);

    testFunction.getTransform(time, testRotation);
    assertEquivalentMatrix(testRotation, rotation);

  }

  /**
   * This test exercises whether the provider handles the case where a frame transform is selected
   * individually from the list but applied in the reverse direction from the native definition
   * loaded into the provider.
   * 
   * @throws SourceException
   */
  @Test
  public void testTimeForwardStaticTreePureFrameProviderSingleBackwardLinkFrame()
      throws SourceException {

    FrameTransformFunction function = staticTreePureFrameTransformProvider
        .createFrameTransformFunction(AB, AA, Coverage.ALL_TIME);

    assertEquals(AB, function.getFromID());
    assertEquals(AA, function.getToID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;
    RotationMatrixIJK retRot = function.getTransform(time, rotation);
    assertSame(retRot, rotation);

    /*
     * Test this is simple as it is merely a single node. Retrieve it from the map.
     */
    ConstantFrameTransformFunction testFunction =
        getContentFromMap(AA, AB, time, staticTreePureFrameTransformContent);

    testFunction.getTransform(time, testRotation);
    assertEquivalentMatrix(testRotation.transpose(), rotation);

  }

  /**
   * This test exercises whether the provider properly connects a leaf node to the top of its
   * section of the frame tree.
   * 
   * @throws SourceException
   */
  @Test
  public void testTimeForwardStaticTreePureFrameProviderSingleLeafForwardLinkFrame()
      throws SourceException {

    FrameTransformFunction function =
        staticTreePureFrameTransformProvider.createFrameTransformFunction(AA, R, Coverage.ALL_TIME);

    assertEquals(AA, function.getFromID());
    assertEquals(R, function.getToID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;
    RotationMatrixIJK retRot = function.getTransform(time, rotation);
    assertSame(retRot, rotation);

    computeChain(testRotation, time, staticTreePureFrameTransformContent, AA, AB, AC, R);
    assertEquivalentMatrix(testRotation, rotation);

  }

  /**
   * This test exercises whether the provider properly connects a the root node of a section of the
   * frame tree to one of its links.
   * 
   * @throws SourceException
   */
  @Test
  public void testTimeForwardStaticTreePureFrameProviderSingleLeafBackwardLinkFrame()
      throws SourceException {

    FrameTransformFunction function =
        staticTreePureFrameTransformProvider.createFrameTransformFunction(R, AA, Coverage.ALL_TIME);

    assertEquals(R, function.getFromID());
    assertEquals(AA, function.getToID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;
    RotationMatrixIJK retRot = function.getTransform(time, rotation);
    assertSame(retRot, rotation);

    computeChain(testRotation, time, staticTreePureFrameTransformContent, AA, AB, AC, R);
    assertComponentRelativeEquality(testRotation.createTranspose(), rotation, TIGHT_TOLERANCE);

  }

  /**
   * This test exercises whether the provider properly connects two leaf nodes sharing a common
   * root.
   * 
   * @throws SourceException
   */
  @Test
  public void testTimeForwardStaticTreePureFrameProviderFullLinkFrame() throws SourceException {

    FrameTransformFunction function = staticTreePureFrameTransformProvider
        .createFrameTransformFunction(AA, BA, Coverage.ALL_TIME);

    assertEquals(AA, function.getFromID());
    assertEquals(BA, function.getToID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;
    RotationMatrixIJK retRot = function.getTransform(time, rotation);
    assertSame(retRot, rotation);

    computeChain(testRotation, time, staticTreePureFrameTransformContent, AA, AB, AC, R);
    computeChain(altRotation, time, staticTreePureFrameTransformContent, BA, BB, R);
    RotationMatrixIJK.mtxm(altRotation, testRotation, testRotation);

    assertEquivalentMatrix(testRotation, rotation);

  }

  /**
   * This test exercises whether the provider properly fails when it is unable to determine a
   * connection between the nodes at the supplied time.
   */
  @Test
  public void testTimeForwardStaticTreePureFrameProviderBrokenLinkRotationFrame() {

    /*
     * There are two possible outcomes, the first is that the provider throws an exception when
     * attempting to create the function, the other is a runtime evaluation exception gets generated
     * when asking for a transform. Handle both cases.
     */
    try {
      FrameTransformFunction function = staticTreePureFrameTransformProvider
          .createFrameTransformFunction(AA, XB, Coverage.ALL_TIME);

      assertEquals(AA, function.getFromID());
      assertEquals(XB, function.getToID());

      time += 5;

      try {
        function.getTransform(time, rotation);
      } catch (FrameEvaluationException e) {
        return;
      }

      fail("Expected evaluation exception not thrown.");

    } catch (SourceException e) {
      return;
    }

    fail("Expected source exception not thrown.");
  }

  private void populateStaticTreeMixedProvider() {

    Set<FrameTestCodes> codeSet = new HashSet<FrameTestCodes>();
    List<FrameTransformFunction> sources = new LinkedList<>();
    List<ConstantFrameTransformFunction> functions =
        new LinkedList<ConstantFrameTransformFunction>();
    Map<FrameCodePair, List<ConstantFrameTransformFunction>> map =
        new HashMap<FrameCodePair, List<ConstantFrameTransformFunction>>();

    ConstantFrameTransformFunction function =
        new ConstantStateTransformFunction(AA, AB, Coverage.ALL_TIME,
            new StateTransform(
                new RotationMatrixIJK(0.23262501680698122, -0.11345880161244597, 0.9659258262890683,
                    0.6851945196756121, 0.7239556287790326, -0.0799794834045749,
                    -0.6902130625843436, 0.6804523112576097, 0.2461515393860416),
                new MatrixIJK(0.0060232452411852384, -0.005938066617125284, -0.0021480774105694634,
                    0.012046490482370477, -0.011876133234250567, -0.004296154821138927,
                    0.013988936617854983, 0.011645294533787818, 0.007033387679941524)));

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    function = new ConstantStateTransformFunction(AB, AC, Coverage.ALL_TIME,
        new StateTransform(
            new RotationMatrixIJK(0.46865981337225204, -0.5847388119079097, 0.6621468879171843,
                0.5103093929582195, 0.7910490458898228, 0.33738069069734045, -0.7210702481648369,
                0.17978300488457546, 0.6691306063588582),
            new MatrixIJK(0.019362795617574476, 0.0753076627264154, 0.052799083840262336,
                -0.01572818413834173, 0.04475247691123446, -0.0811402810436, 0.0014538445916930972,
                0.04802405585505995, -0.011336478881335063)));

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    function = new ConstantStateTransformFunction(AC, R, Coverage.ALL_TIME,
        new StateTransform(
            new RotationMatrixIJK(0.08348014257622213, -0.0629067995414709, 0.9945218953682733,
                0.6291677054416244, 0.7772610183154323, -0.0036479907591269655, -0.7727736177075243,
                0.6260255937090003, 0.10446478735209536),
            new MatrixIJK(-0.037955936010262696, 0.008286651706940469, 0.0037101784332086787,
                0.0014570033479893777, -0.0010979307738907262, 0.017357681557350912,
                -0.0029140066959787553, 0.0021958615477814525, -0.034715363114701824)));

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    function = new ConstantStateTransformFunction(BA, BB, Coverage.ALL_TIME,
        new StateTransform(
            new RotationMatrixIJK(0.9781476007338057, -0.20636194860240742, 0.02533806124621436,
                0.05029839035996436, 0.11662217487111268, -0.991901880356887, 0.20173582504328824,
                0.9715009081299661, 0.12445337438878395),
            new MatrixIJK(0.009246054318345706, 0.04781450720044231, 0.03248429394833022,
                0.03617070740362956, 0.045465125418033596, 0.007179712337132024,
                -0.0538493061705677, 0.004698763564817432, 0.050609163222396385)));

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    function = new ConstantStateTransformFunction(BB, R, Coverage.ALL_TIME,
        new StateTransform(
            new RotationMatrixIJK(-0.7009436628498646, -0.6793378264128004, 0.2172052004795177,
                0.6582394338705277, -0.7334323383185172, -0.16969930113715712, 0.27458847246092255,
                0.024023378475231092, 0.9612616959383189),
            new MatrixIJK(-0.7466815019357148, 0.9010880847202829, 0.40865298683151163,
                -0.808439505815695, -0.8257748557786994, 0.43313802873181917, 0.03192084357758847,
                0.27028827309353176, -0.015873235378095915)));

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    function =
        new ConstantFrameTransformFunction(AA, AC, Coverage.ALL_TIME, new RotationMatrixIJK());

    functions.add(function);
    addContentToMap(function, map);
    addContentToSet(function, codeSet);

    sources.addAll(functions);
    functions.clear();

    staticTreeMixedProvider = createProvider(sources);
    staticTreeMixedContent = map;
    staticTreeMixedSourceList = sources;
    staticTreeMixedCodes = codeSet;
  }

  /**
   * This test exercises whether the provider with a frame transform overriding a state transform
   * properly ignores the frame transform when building the state transform function linking through
   * the different options.
   * 
   * @throws SourceException
   */
  @Test
  public void testTimeForwardStaticTreeMixedStateSelection() throws SourceException {

    StateTransformFunction function =
        staticTreeMixedProvider.createStateTransformFunction(AA, BA, Coverage.ALL_TIME);

    assertEquals(AA, function.getFromID());
    assertEquals(BA, function.getToID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;
    RotationMatrixIJK retRot = function.getTransform(time, rotation);
    assertSame(retRot, rotation);

    computeChain(transform, time, staticTreeMixedContent, AA, AB, AC, R);
    computeChain(testTransform, time, staticTreeMixedContent, BA, BB, R);
    StateTransform.mixm(testTransform, transform, transform);

    assertEquivalentMatrix(transform.getRotation(), rotation);

    time += 5;
    StateTransform retState = function.getStateTransform(time, transform);
    assertSame(retState, transform);

    computeChain(testTransform, time, staticTreeMixedContent, AA, AB, AC, R);
    computeChain(altTransform, time, staticTreeMixedContent, BA, BB, R);
    StateTransform.mixm(altTransform, testTransform, testTransform);

    assertComponentRelativeEquality(testTransform, transform, TOLERANCE);

  }

  /**
   * This test exercises the when linking through a frame transform that overrides a state transform
   * in the provider, the frame transform is picked up instead of the state transform at a lower
   * priority.
   * 
   * @throws SourceException
   */
  @Test
  public void testTimeForwardStaticTreeMixedFrameSelection() throws SourceException {

    FrameTransformFunction function =
        staticTreeMixedProvider.createFrameTransformFunction(AA, BA, Coverage.ALL_TIME);

    assertEquals(AA, function.getFromID());
    assertEquals(BA, function.getToID());
    assertEquals(Coverage.ALL_TIME, function.getCoverage());

    time += 5;
    RotationMatrixIJK retRot = function.getTransform(time, rotation);
    assertSame(retRot, rotation);

    computeChain(testRotation, time, staticTreeMixedContent, AA, AC, R);
    computeChain(altRotation, time, staticTreeMixedContent, BA, BB, R);
    RotationMatrixIJK.mtxm(altRotation, testRotation, testRotation);

    assertComponentRelativeEquality(testRotation, rotation, TOLERANCE);

  }

  @Test
  public void testNullBufferFrameTransformFunction() throws SourceException {

    FrameTransformFunction function = staticTreePureStateTransformProvider
        .createFrameTransformFunction(AA, BA, Coverage.ALL_TIME);

    time += 5;
    RotationMatrixIJK retRot = function.getTransform(time, rotation);
    assertSame(retRot, rotation);

    RotationMatrixIJK result = function.getTransform(time, null);
    assertNotSame(retRot, result);

    assertEquivalentMatrix(retRot, result);
  }

  @Test
  public void testNullBufferStateTransformFunction() throws SourceException {

    StateTransformFunction function = staticTreePureStateTransformProvider
        .createStateTransformFunction(AA, BA, Coverage.ALL_TIME);

    time += 5;
    RotationMatrixIJK retRot = function.getTransform(time, rotation);
    assertSame(retRot, rotation);

    RotationMatrixIJK result = function.getTransform(time, null);
    assertNotSame(retRot, result);

    assertEquivalentMatrix(retRot, result);

    StateTransform retXfr = function.getStateTransform(time, transform);
    assertSame(retXfr, transform);

    StateTransform resultXfr = function.getStateTransform(time, null);
    assertNotSame(retXfr, resultXfr);

    assertEquivalentStateTransform(retXfr, resultXfr);
  }

}
