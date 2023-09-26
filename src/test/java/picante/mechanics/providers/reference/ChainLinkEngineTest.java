package picante.mechanics.providers.reference;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import java.util.LinkedList;
import java.util.List;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class ChainLinkEngineTest {

  private ChainLinkEngine<EngineTestCodes, Function, Provider> engine;
  private List<Function> functions;
  private Function aaToRNonNegativeTime;
  private Function aaToAdNonNegativeTime;
  private Function aaToAcNegativeTime;

  private void populateDefaultFunctionsList() {
    functions = new LinkedList<Function>();

    for (EngineTestCodes code : EngineTestCodes.values()) {

      Function f = code.getFunction();

      if (f.getNodeCode() != null) {
        functions.add(f);
      }
    }
  }

  private void createEngineFromFunctionsList() {
    engine = new ChainLinkEngine<EngineTestCodes, Function, Provider>(functions,
        EngineTestCodes.createProvider(), EngineTestCodes.SEPARATOR, EngineTestCodes.BROKEN_LINK);
  }

  @Before
  public void setUp() throws Exception {

    populateDefaultFunctionsList();

    createEngineFromFunctionsList();

    aaToRNonNegativeTime = new Function() {

      @Override
      public EngineTestCodes getLeafCode() {
        return EngineTestCodes.AA;
      }

      @Override
      public EngineTestCodes getNodeCode() {
        return EngineTestCodes.R;
      }

      @Override
      public boolean validAt(double time) {
        return (time >= 0.0);
      }

    };

    aaToAdNonNegativeTime = new Function() {

      @Override
      public EngineTestCodes getLeafCode() {
        return EngineTestCodes.AA;
      }

      @Override
      public EngineTestCodes getNodeCode() {
        return EngineTestCodes.AD;
      }

      @Override
      public boolean validAt(double time) {
        return (time >= 0.0);
      }

    };

    aaToAcNegativeTime = new Function() {

      @Override
      public EngineTestCodes getLeafCode() {
        return EngineTestCodes.AA;
      }

      @Override
      public EngineTestCodes getNodeCode() {
        return EngineTestCodes.AC;
      }

      @Override
      public boolean validAt(double time) {
        return (time < 0.0);
      }

    };

    functions.clear();
  }

  @After
  public void tearDown() throws Exception {}

  @Test
  public void testLinkTwoUnknownCodes() {
    boolean result =
        engine.populateLinkage(EngineTestCodes.ANK, EngineTestCodes.UNK, 0.0, functions);

    assertFalse(result);
    assertEquals(1, functions.size());
    assertEquals(EngineTestCodes.BROKEN_LINK, functions.get(0));
  }

  @Test
  public void testLinkIsolatedWithUnknownCode() {
    boolean result = engine.populateLinkage(EngineTestCodes.R, EngineTestCodes.UNK, 0.0, functions);

    assertFalse(result);
    assertEquals(1, functions.size());
    assertEquals(EngineTestCodes.BROKEN_LINK, functions.get(0));
  }

  @Test
  public void testLinkUnknownWithIsolatedCode() {
    boolean result = engine.populateLinkage(EngineTestCodes.UNK, EngineTestCodes.R, 0.0, functions);

    assertFalse(result);
    assertEquals(1, functions.size());
    assertEquals(EngineTestCodes.BROKEN_LINK, functions.get(0));
  }

  @Test
  public void testSingleForwardLink() {
    boolean result = engine.populateLinkage(EngineTestCodes.AB, EngineTestCodes.AC, 0.0, functions);

    assertTrue(result);
    assertEquals(1, functions.size());
    assertEquals(EngineTestCodes.AB.getFunction(), functions.get(0));
  }

  @Test
  public void testSingleBackwardLink() {
    boolean result = engine.populateLinkage(EngineTestCodes.AB, EngineTestCodes.AA, 0.0, functions);

    assertTrue(result);
    assertEquals(2, functions.size());
    assertEquals(EngineTestCodes.SEPARATOR, functions.get(0));
    assertEquals(EngineTestCodes.AA.getFunction(), functions.get(1));
  }

  @Test
  public void testMultipleForwardLink() {
    boolean result = engine.populateLinkage(EngineTestCodes.AA, EngineTestCodes.R, 0.0, functions);

    assertTrue(result);
    assertEquals(4, functions.size());
    assertEquals(EngineTestCodes.AA.getFunction(), functions.get(0));
    assertEquals(EngineTestCodes.AB.getFunction(), functions.get(1));
    assertEquals(EngineTestCodes.AC.getFunction(), functions.get(2));
    assertEquals(EngineTestCodes.AD.getFunction(), functions.get(3));
  }

  @Test
  public void testMultipleBackwardLink() {
    boolean result = engine.populateLinkage(EngineTestCodes.R, EngineTestCodes.AA, 0.0, functions);

    assertTrue(result);
    assertEquals(5, functions.size());
    assertEquals(EngineTestCodes.SEPARATOR, functions.get(0));
    assertEquals(EngineTestCodes.AD.getFunction(), functions.get(1));
    assertEquals(EngineTestCodes.AC.getFunction(), functions.get(2));
    assertEquals(EngineTestCodes.AB.getFunction(), functions.get(3));
    assertEquals(EngineTestCodes.AA.getFunction(), functions.get(4));
  }

  @Test
  public void testMultipleForwardLinkWithLeftOver() {
    boolean result = engine.populateLinkage(EngineTestCodes.AB, EngineTestCodes.AD, 0.0, functions);

    assertTrue(result);
    assertEquals(2, functions.size());
    assertEquals(EngineTestCodes.AB.getFunction(), functions.get(0));
    assertEquals(EngineTestCodes.AC.getFunction(), functions.get(1));
  }

  @Test
  public void testMultipleBackwardLinkWithLeftOver() {
    boolean result = engine.populateLinkage(EngineTestCodes.AD, EngineTestCodes.AB, 0.0, functions);

    assertTrue(result);
    assertEquals(3, functions.size());
    assertEquals(EngineTestCodes.SEPARATOR, functions.get(0));
    assertEquals(EngineTestCodes.AC.getFunction(), functions.get(1));
    assertEquals(EngineTestCodes.AB.getFunction(), functions.get(2));
  }

  @Test
  public void testSingleLeafSingleNode() {
    boolean result = engine.populateLinkage(EngineTestCodes.AD, EngineTestCodes.BC, 0.0, functions);

    assertTrue(result);
    assertEquals(3, functions.size());
    assertEquals(EngineTestCodes.AD.getFunction(), functions.get(0));
    assertEquals(EngineTestCodes.SEPARATOR, functions.get(1));
    assertEquals(EngineTestCodes.BC.getFunction(), functions.get(2));
  }

  @Test
  public void testSingleLeafSingleNodeWithLeftOver() {
    boolean result = engine.populateLinkage(EngineTestCodes.AA, EngineTestCodes.DA, 0.0, functions);

    assertTrue(result);
    assertEquals(3, functions.size());
    assertEquals(EngineTestCodes.AA.getFunction(), functions.get(0));
    assertEquals(EngineTestCodes.SEPARATOR, functions.get(1));
    assertEquals(EngineTestCodes.DA.getFunction(), functions.get(2));
  }

  @Test
  public void testSingleLeafSingleNodeUpTreeWithLeftOver() {
    boolean result = engine.populateLinkage(EngineTestCodes.CB, EngineTestCodes.AB, 0.0, functions);

    assertTrue(result);
    assertEquals(3, functions.size());
    assertEquals(EngineTestCodes.CB.getFunction(), functions.get(0));
    assertEquals(EngineTestCodes.SEPARATOR, functions.get(1));
    assertEquals(EngineTestCodes.AB.getFunction(), functions.get(2));

  }

  @Test
  public void testMultipleLeafMultipleNodeRootConnection() {
    boolean result = engine.populateLinkage(EngineTestCodes.AA, EngineTestCodes.BA, 0.0, functions);

    assertTrue(result);
    assertEquals(8, functions.size());
    assertEquals(EngineTestCodes.AA.getFunction(), functions.get(0));
    assertEquals(EngineTestCodes.AB.getFunction(), functions.get(1));
    assertEquals(EngineTestCodes.AC.getFunction(), functions.get(2));
    assertEquals(EngineTestCodes.AD.getFunction(), functions.get(3));
    assertEquals(EngineTestCodes.SEPARATOR, functions.get(4));
    assertEquals(EngineTestCodes.BC.getFunction(), functions.get(5));
    assertEquals(EngineTestCodes.BB.getFunction(), functions.get(6));
    assertEquals(EngineTestCodes.BA.getFunction(), functions.get(7));
  }

  @Test
  public void testMultipleLeafMultipleNodeWithLeftOver() {
    boolean result = engine.populateLinkage(EngineTestCodes.AA, EngineTestCodes.CA, 0.0, functions);

    assertTrue(result);
    assertEquals(5, functions.size());
    assertEquals(EngineTestCodes.AA.getFunction(), functions.get(0));
    assertEquals(EngineTestCodes.AB.getFunction(), functions.get(1));
    assertEquals(EngineTestCodes.SEPARATOR, functions.get(2));
    assertEquals(EngineTestCodes.CB.getFunction(), functions.get(3));
    assertEquals(EngineTestCodes.CA.getFunction(), functions.get(4));
  }

  @Test
  public void testMultipleLinkDisconnect() {
    boolean result = engine.populateLinkage(EngineTestCodes.XA, EngineTestCodes.BA, 0.0, functions);

    assertFalse(result);
    assertEquals(7, functions.size());
    assertEquals(EngineTestCodes.XA.getFunction(), functions.get(0));
    assertEquals(EngineTestCodes.XB.getFunction(), functions.get(1));
    assertEquals(EngineTestCodes.XC.getFunction(), functions.get(2));
    assertEquals(EngineTestCodes.BROKEN_LINK, functions.get(3));
    assertEquals(EngineTestCodes.BC.getFunction(), functions.get(4));
    assertEquals(EngineTestCodes.BB.getFunction(), functions.get(5));
    assertEquals(EngineTestCodes.BA.getFunction(), functions.get(6));
  }

  @Test
  public void testSingleLinkMultipleNodeDisconnect() {
    boolean result = engine.populateLinkage(EngineTestCodes.XC, EngineTestCodes.BA, 0.0, functions);

    assertFalse(result);
    assertEquals(5, functions.size());
    assertEquals(EngineTestCodes.XC.getFunction(), functions.get(0));
    assertEquals(EngineTestCodes.BROKEN_LINK, functions.get(1));
    assertEquals(EngineTestCodes.BC.getFunction(), functions.get(2));
    assertEquals(EngineTestCodes.BB.getFunction(), functions.get(3));
    assertEquals(EngineTestCodes.BA.getFunction(), functions.get(4));
  }

  @Test
  public void testMultipleLeafSingleLinkDisconnect() {
    boolean result = engine.populateLinkage(EngineTestCodes.BA, EngineTestCodes.XC, 0.0, functions);

    assertFalse(result);
    assertEquals(5, functions.size());
    assertEquals(EngineTestCodes.BA.getFunction(), functions.get(0));
    assertEquals(EngineTestCodes.BB.getFunction(), functions.get(1));
    assertEquals(EngineTestCodes.BC.getFunction(), functions.get(2));
    assertEquals(EngineTestCodes.BROKEN_LINK, functions.get(3));
    assertEquals(EngineTestCodes.XC.getFunction(), functions.get(4));
  }

  @Test
  public void testSegmentPrioritySelectionMultipleOverlays() {

    /*
     * Recreate engine, to contain our additional functions.
     */
    populateDefaultFunctionsList();
    functions.add(aaToAdNonNegativeTime);
    functions.add(aaToRNonNegativeTime);

    createEngineFromFunctionsList();

    boolean result = engine.populateLinkage(EngineTestCodes.AA, EngineTestCodes.BA, 1.0, functions);

    assertTrue(result);
    assertEquals(5, functions.size());
    assertEquals(aaToRNonNegativeTime, functions.get(0));
    assertEquals(EngineTestCodes.SEPARATOR, functions.get(1));
    assertEquals(EngineTestCodes.BC.getFunction(), functions.get(2));
    assertEquals(EngineTestCodes.BB.getFunction(), functions.get(3));
    assertEquals(EngineTestCodes.BA.getFunction(), functions.get(4));
  }

  @Test
  public void testSegmentPriorityReverseSelectionMultipleOverlays() {

    /*
     * Recreate engine, to contain our additional functions.
     */
    populateDefaultFunctionsList();
    functions.add(aaToRNonNegativeTime);
    functions.add(aaToAdNonNegativeTime);

    createEngineFromFunctionsList();

    boolean result = engine.populateLinkage(EngineTestCodes.AA, EngineTestCodes.BA, 1.0, functions);

    assertTrue(result);
    assertEquals(6, functions.size());
    assertEquals(aaToAdNonNegativeTime, functions.get(0));
    assertEquals(EngineTestCodes.AD.getFunction(), functions.get(1));
    assertEquals(EngineTestCodes.SEPARATOR, functions.get(2));
    assertEquals(EngineTestCodes.BC.getFunction(), functions.get(3));
    assertEquals(EngineTestCodes.BB.getFunction(), functions.get(4));
    assertEquals(EngineTestCodes.BA.getFunction(), functions.get(5));
  }

  @Test
  public void testReverseSegmentPriorityReverseSelectionMultipleOverlays() {

    /*
     * Recreate engine, to contain our additional functions.
     */
    populateDefaultFunctionsList();
    functions.add(aaToRNonNegativeTime);
    functions.add(aaToAdNonNegativeTime);

    createEngineFromFunctionsList();

    boolean result = engine.populateLinkage(EngineTestCodes.BA, EngineTestCodes.AA, 1.0, functions);

    assertTrue(result);
    assertEquals(6, functions.size());
    assertEquals(EngineTestCodes.BA.getFunction(), functions.get(0));
    assertEquals(EngineTestCodes.BB.getFunction(), functions.get(1));
    assertEquals(EngineTestCodes.BC.getFunction(), functions.get(2));
    assertEquals(EngineTestCodes.SEPARATOR, functions.get(3));
    assertEquals(EngineTestCodes.AD.getFunction(), functions.get(4));
    assertEquals(aaToAdNonNegativeTime, functions.get(5));
  }

  @Test
  public void testReverseSegmentPrioritySelectionMultipleOverlays() {

    /*
     * Recreate engine, to contain our additional functions.
     */
    populateDefaultFunctionsList();
    functions.add(aaToAdNonNegativeTime);
    functions.add(aaToRNonNegativeTime);

    createEngineFromFunctionsList();

    boolean result = engine.populateLinkage(EngineTestCodes.BA, EngineTestCodes.AA, 1.0, functions);

    assertTrue(result);
    assertEquals(5, functions.size());
    assertEquals(EngineTestCodes.BA.getFunction(), functions.get(0));
    assertEquals(EngineTestCodes.BB.getFunction(), functions.get(1));
    assertEquals(EngineTestCodes.BC.getFunction(), functions.get(2));
    assertEquals(EngineTestCodes.SEPARATOR, functions.get(3));
    assertEquals(aaToRNonNegativeTime, functions.get(4));
  }

  @Test
  public void testTimeDependentSegmentOverloading() {

    /*
     * Recreate engine, to contain our additional functions.
     */
    populateDefaultFunctionsList();
    functions.add(aaToAcNegativeTime);
    functions.add(aaToAdNonNegativeTime);
    functions.add(aaToRNonNegativeTime);

    createEngineFromFunctionsList();

    boolean result =
        engine.populateLinkage(EngineTestCodes.AA, EngineTestCodes.BA, -1.0, functions);

    assertTrue(result);
    assertEquals(7, functions.size());
    assertEquals(aaToAcNegativeTime, functions.get(0));
    assertEquals(EngineTestCodes.AC.getFunction(), functions.get(1));
    assertEquals(EngineTestCodes.AD.getFunction(), functions.get(2));
    assertEquals(EngineTestCodes.SEPARATOR, functions.get(3));
    assertEquals(EngineTestCodes.BC.getFunction(), functions.get(4));
    assertEquals(EngineTestCodes.BB.getFunction(), functions.get(5));
    assertEquals(EngineTestCodes.BA.getFunction(), functions.get(6));
  }

  @Test
  public void testReverseTimeDependentSegmentOverloading() {

    /*
     * Recreate engine, to contain our additional functions.
     */
    populateDefaultFunctionsList();
    functions.add(aaToAcNegativeTime);
    functions.add(aaToAdNonNegativeTime);
    functions.add(aaToRNonNegativeTime);

    createEngineFromFunctionsList();

    boolean result =
        engine.populateLinkage(EngineTestCodes.BA, EngineTestCodes.AA, -1.0, functions);

    assertTrue(result);
    assertEquals(7, functions.size());
    assertEquals(EngineTestCodes.BA.getFunction(), functions.get(0));
    assertEquals(EngineTestCodes.BB.getFunction(), functions.get(1));
    assertEquals(EngineTestCodes.BC.getFunction(), functions.get(2));
    assertEquals(EngineTestCodes.SEPARATOR, functions.get(3));
    assertEquals(EngineTestCodes.AD.getFunction(), functions.get(4));
    assertEquals(EngineTestCodes.AC.getFunction(), functions.get(5));
    assertEquals(aaToAcNegativeTime, functions.get(6));
  }

}
