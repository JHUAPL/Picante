package picante.spice.kernelpool.content;

import java.util.HashMap;
import java.util.Map;
import picante.mechanics.providers.aberrated.AberrationCorrection;
import picante.spice.kernel.tk.fk.FKInstantiationException;
import picante.spice.kernel.tk.fk.FrameInfo;
import picante.spice.kernel.tk.fk.dynamic.DefinableStateVectorFunction;
import picante.spice.kernel.tk.fk.dynamic.DynamicAberratedConstantVectorFunction;
import picante.spice.kernel.tk.fk.dynamic.DynamicConstantVectorFunction;
import picante.spice.kernel.tk.fk.dynamic.DynamicPositionVectorFunction;
import picante.spice.kernel.tk.fk.dynamic.DynamicVelocityVectorFunction;

public enum DynamicVectorProvider {
  OBSERVER_TARGET_POSITION("OBSERVER_TARGET_POSITION") {
    @Override
    public DefinableStateVectorFunction createVectorFunction(VectorID vectorID, FrameInfo info,
        int relativeCode, SpiceInfoHolder infoHolder) throws KernelPoolValidationException {
      int observerCode = ParsingUtilities
          .parseEphemeris("FRAME_%s_" + vectorID.prefix + "_OBSERVER", info, infoHolder);
      int targetCode = ParsingUtilities.parseEphemeris("FRAME_%s_" + vectorID.prefix + "_TARGET",
          info, infoHolder);
      SpiceAberrationCorrection abCorr =
          ParsingUtilities.parseAbCorr("FRAME_%s_" + vectorID.prefix + "_ABCORR", info, infoHolder);
      return new DynamicPositionVectorFunction(observerCode, targetCode, relativeCode, abCorr);
    }
  },
  OBSERVER_TARGET_VELOCITY("OBSERVER_TARGET_VELOCITY") {

    @Override
    public DefinableStateVectorFunction createVectorFunction(VectorID vectorID, FrameInfo info,
        @SuppressWarnings("unused") int relativeCode, SpiceInfoHolder infoHolder)
        throws KernelPoolValidationException {
      int observerCode = ParsingUtilities
          .parseEphemeris("FRAME_%s_" + vectorID.prefix + "_OBSERVER", info, infoHolder);
      int targetCode = ParsingUtilities.parseEphemeris("FRAME_%s_" + vectorID.prefix + "_TARGET",
          info, infoHolder);
      int frameCode =
          ParsingUtilities.parseFrame("FRAME_%s_" + vectorID.prefix + "_FRAME", info, infoHolder);
      SpiceAberrationCorrection abCorr =
          ParsingUtilities.parseAbCorr("FRAME_%s_" + vectorID.prefix + "_ABCORR", info, infoHolder);
      DynamicVelocityVectorFunction vectorFunction =
          new DynamicVelocityVectorFunction(observerCode, targetCode, frameCode, abCorr);
      return vectorFunction;
    }
  },
  CONSTANT("CONSTANT") {

    @Override
    public DefinableStateVectorFunction createVectorFunction(VectorID vectorID, FrameInfo info,
        int relativeCode, SpiceInfoHolder infoHolder) throws KernelPoolValidationException {
      int frameCode =
          ParsingUtilities.parseFrame("FRAME_%s_" + vectorID.prefix + "_FRAME", info, infoHolder);
      SpiceAberrationCorrection abCorr =
          ParsingUtilities.parseAbCorr("FRAME_%s_" + vectorID.prefix + "_ABCORR", info, infoHolder);
      if (abCorr.getAbCorr().equals(AberrationCorrection.LT_S)
          || abCorr.getAbCorr().equals(AberrationCorrection.XLT_S)) {
        throw new KernelPoolValidationException(
            "Aberration Correction " + abCorr + " not allowed for Constant Vector definitions.");
      }

      DynamicConstantVectorFunction constantVec = DynamicConstantVectorProvider
          .createFunction(vectorID, info, relativeCode, frameCode, infoHolder);
      if (!abCorr.equals(SpiceAberrationCorrection.NONE)) {
        boolean inertialFrame = (infoHolder.getFrames().get(frameCode).getClassID() == 1);
        if (!inertialFrame) {
          int observerCode = ParsingUtilities
              .parseFrame("FRAME_%s_" + vectorID.prefix + "_OBSERVER", info, infoHolder);
          return new DynamicAberratedConstantVectorFunction(constantVec, abCorr, observerCode);
        }
      }
      return constantVec;
    }
  },

  TARGET_NEAR_POINT("TARGET_NEAR_POINT") {

    @SuppressWarnings("unused")
    @Override
    public DefinableStateVectorFunction createVectorFunction(VectorID vectorID, FrameInfo info,
        int relativeCode, SpiceInfoHolder infoHolder)
        throws KernelPoolValidationException, FKInstantiationException {
      throw new FKInstantiationException("Frame " + info.getName() + " , code " + info.getCode()
          + ", has a two vector dynamic frame with a " + getKeyword()
          + " vector, which has not been implemented yet.");
    }
  };

  private final String keyword;

  private DynamicVectorProvider(String keyword) {
    this.keyword = keyword;
  }

  public String getKeyword() {
    return keyword;
  }

  public static DynamicVectorProvider fromString(String keyword) {
    DynamicVectorProvider vectorProvider = mapStringToVectorDefinition.get(keyword);
    if (vectorProvider == null) {
      throw new RuntimeException("Invalid VectorProvider: " + keyword);
    }
    return vectorProvider;
  }

  static Map<String, DynamicVectorProvider> mapStringToVectorDefinition = new HashMap<>();
  static {
    for (DynamicVectorProvider vecDef : DynamicVectorProvider.values()) {
      mapStringToVectorDefinition.put(vecDef.getKeyword(), vecDef);
    }
  }

  public abstract DefinableStateVectorFunction createVectorFunction(VectorID vectorID,
      FrameInfo info, int relativeCode, SpiceInfoHolder infoHolder)
      throws KernelPoolValidationException, FKInstantiationException;

  public static DefinableStateVectorFunction createFunction(VectorID vectorID, FrameInfo info,
      int relativeCode, SpiceInfoHolder infoHolder)
      throws KernelPoolValidationException, FKInstantiationException {
    String vecDef = ParsingUtilities.parseString("FRAME_%s_" + vectorID.toString() + "_VECTOR_DEF",
        info, infoHolder);
    DynamicVectorProvider provider = DynamicVectorProvider.fromString(vecDef);
    DefinableStateVectorFunction function =
        provider.createVectorFunction(vectorID, info, relativeCode, infoHolder);
    return function;
  }

  public enum VectorID {
    PRI("PRI"), SEC("SEC");

    private final String prefix;

    public String getPrefix() {
      return prefix;
    }

    private VectorID(String prefix) {
      this.prefix = prefix;
    }
  }


}
