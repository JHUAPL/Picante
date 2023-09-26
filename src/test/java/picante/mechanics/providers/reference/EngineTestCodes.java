package picante.mechanics.providers.reference;

import picante.mechanics.providers.reference.ChainLinkEngine;
import picante.mechanics.providers.reference.CodeProvider;

/**
 * Simple enumeration used for exercising the logic in the {@link ChainLinkEngine}. The
 * <code>Function</code>s that are returned by the getFunction() method on this enum establish the
 * following tree structure, valid for all supplied times.
 * 
 * <pre>
 * <code>
 * 
 *                     R          XD        UNK     ANK
 *                    / \           \
 *                  AD   BC          XC
 *                 /       \           \
 *               AC         BB          XB
 *              /  \          \           \
 *            AB    CB         BA          XA
 *           /  \     \
 *         AA    DA    CA
 * 
 * </code>
 * </pre>
 * 
 * Note: Nodes at the top of the tree have functions that report their node code as null. This
 * indicates they should be filtered, and not loaded into the test scenario.
 */
@SuppressWarnings("unused")
enum EngineTestCodes {

  ANK(null), UNK(null), R(null), AD(R), AC(AD), AB(AC), AA(AB), BC(R), BB(BC), BA(BB), CB(AC), CA(
      CB), DA(AB), XD(null), XC(XD), XB(XC), XA(XB);

  public static Provider createProvider() {
    return new Provider();
  }

  public final static Function SEPARATOR = new Function() {

    @Override
    public EngineTestCodes getLeafCode() {
      return null;
    }

    @Override
    public EngineTestCodes getNodeCode() {
      return null;
    }

    @Override
    public boolean validAt(double time) {
      return false;
    }

  };

  public final static Function BROKEN_LINK = new Function() {

    @Override
    public EngineTestCodes getLeafCode() {
      return null;
    }

    @Override
    public EngineTestCodes getNodeCode() {
      return null;
    }

    @Override
    public boolean validAt(double time) {
      return false;
    }

  };

  private final EngineTestCodes node;
  private final Function function;

  EngineTestCodes(EngineTestCodes node) {
    this.node = node;
    this.function = new Function() {

      @Override
      public EngineTestCodes getLeafCode() {
        return EngineTestCodes.this;
      }

      @Override
      public EngineTestCodes getNodeCode() {
        return EngineTestCodes.this.node;
      }

      @Override
      public boolean validAt(double time) {
        return true;
      }

      @Override
      public String toString() {
        return EngineTestCodes.this.toString() + "->" + EngineTestCodes.this.node.toString();
      }
    };

  }

  public Function getFunction() {
    return function;
  }

}


class Provider implements CodeProvider<EngineTestCodes, Function> {

  @Override
  public EngineTestCodes getLeafCode(Function function) {
    return function.getLeafCode();
  }

  @Override
  public EngineTestCodes getNodeCode(Function function) {
    return function.getNodeCode();
  }

  @Override
  public boolean validAt(Function function, double time) {
    return function.validAt(time);
  }

}


interface Function {

  public EngineTestCodes getLeafCode();

  public EngineTestCodes getNodeCode();

  public boolean validAt(double time);
}
