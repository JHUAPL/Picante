package picante.spice.kernelpool.content;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.google.common.collect.Lists;
import picante.mechanics.rotations.EulerAngles;
import picante.spice.kernel.tk.fk.FKInstantiationException;
import picante.spice.kernel.tk.fk.FrameInfo;
import picante.spice.kernel.tk.fk.dynamic.Axis;
import picante.spice.kernel.tk.fk.dynamic.DefinableStateVectorFunction;
import picante.spice.kernel.tk.fk.dynamic.DynamicEulerFrameFunction;
import picante.spice.kernel.tk.fk.dynamic.DynamicFrameFunction;
import picante.spice.kernel.tk.fk.dynamic.DynamicFrozenFrameFunction;
import picante.spice.kernel.tk.fk.dynamic.DynamicInertialFrameFunction;
import picante.spice.kernel.tk.fk.dynamic.DynamicMeanEclipticFrameFunction;
import picante.spice.kernel.tk.fk.dynamic.DynamicMeanEquatorFrameFunction;
import picante.spice.kernel.tk.fk.dynamic.DynamicTrueEquatorFrameFunction;
import picante.spice.kernel.tk.fk.dynamic.DynamicTwoVectorFrameFunction;
import picante.spice.kernel.tk.fk.dynamic.TwoVectorMatrix;
import picante.spice.kernelpool.content.DynamicVectorProvider.VectorID;

public enum DynamicFrameProvider {
  TWO_VECTOR("TWO-VECTOR") {
    @Override
    public DynamicFrameFunction createFrameFunction(FrameInfo info, int relativeCode,
        SpiceInfoHolder infoHolder) throws KernelPoolValidationException, FKInstantiationException {
      int frameCode = info.getCode();
      VectorID priVecID = VectorID.PRI;
      String priAxisStr = ParsingUtilities.parseString("FRAME_%s_PRI_AXIS", info, infoHolder);
      Axis priAxis = Axis.fromString(priAxisStr);
      DefinableStateVectorFunction primaryFunction =
          DynamicVectorProvider.createFunction(priVecID, info, relativeCode, infoHolder);
      VectorID secVecID = VectorID.SEC;
      DefinableStateVectorFunction secondaryFunction =
          DynamicVectorProvider.createFunction(secVecID, info, relativeCode, infoHolder);
      String secAxisStr = ParsingUtilities.parseString("FRAME_%s_SEC_AXIS", info, infoHolder);
      Axis secAxis = Axis.fromString(secAxisStr);
      TwoVectorMatrix twoVecMatrix = new TwoVectorMatrix(priAxis, secAxis);

      KernelPoolValidatingRetriever retriever = infoHolder.getRetriever();
      double angleSepTolerance = Math.PI / 1000;
      String angleSepKeyword =
          FKFactory.getFrameKeyword("FRAME_%s_ANGLE_SEP_TOL", frameCode, info.getName(), retriever);
      if (retriever.containsKeyword(angleSepKeyword)) {
        angleSepTolerance = retriever.getDouble(angleSepKeyword);
      }
      DynamicTwoVectorFrameFunction twoVectorFrameFunction =
          new DynamicTwoVectorFrameFunction(frameCode, relativeCode, primaryFunction,
              secondaryFunction, twoVecMatrix, angleSepTolerance);
      return applyRotationStateOrFreeze(info, twoVectorFrameFunction, infoHolder, true);
    }
  },
  MEAN_EQUATOR_AND_EQUINOX_OF_DATE("MEAN_EQUATOR_AND_EQUINOX_OF_DATE") {
    @Override
    public DynamicFrameFunction createFrameFunction(FrameInfo info, int relativeCode,
        SpiceInfoHolder infoHolder) throws FKInstantiationException, KernelPoolValidationException {
      int frameCode = info.getCode();

      String precession = ParsingUtilities.parseString("FRAME_%s_PREC_MODEL", info, infoHolder);
      if (!precession.equals("EARTH_IAU_1976")) {
        throw new FKInstantiationException("Frame " + info.getName() + " , code " + info.getCode()
            + "is a Mean Equator and Equinox of Date frame, which only supports the Earth IAU 1976 Precession model: "
            + precession);
      }
      DynamicFrameFunction dynamicFrameFunction =
          new DynamicMeanEquatorFrameFunction(frameCode, relativeCode);
      return applyRotationStateOrFreeze(info, dynamicFrameFunction, infoHolder, false);
    }
  },
  TRUE_EQUATOR_AND_EQUINOX_OF_DATE("TRUE_EQUATOR_AND_EQUINOX_OF_DATE") {
    @Override
    public DynamicFrameFunction createFrameFunction(FrameInfo info, int relativeCode,
        SpiceInfoHolder infoHolder) throws FKInstantiationException, KernelPoolValidationException {
      int frameCode = info.getCode();


      String precession = ParsingUtilities.parseString("FRAME_%s_PREC_MODEL", info, infoHolder);
      if (!precession.equals("EARTH_IAU_1976")) {
        throw new FKInstantiationException("Frame " + info.getName() + " , code " + info.getCode()
            + "is a True Equator and Equinox of Date frame, which only supports the Earth IAU 1976 Precession model: "
            + precession);
      }

      String nutation = ParsingUtilities.parseString("FRAME_%s_NUT_MODEL", info, infoHolder);
      if (!nutation.equals("EARTH_IAU_1980")) {
        throw new FKInstantiationException("Frame " + info.getName() + " , code " + info.getCode()
            + "is a True Equator and Equinox of Date frame, which only supports the Earth IAU 1980 Nutation model: "
            + nutation);
      }

      DynamicFrameFunction dynamicFrameFunction =
          new DynamicTrueEquatorFrameFunction(frameCode, relativeCode);
      return applyRotationStateOrFreeze(info, dynamicFrameFunction, infoHolder, false);
    }
  },

  MEAN_ECLIPTIC_AND_EQUINOX_OF_DATE("MEAN_ECLIPTIC_AND_EQUINOX_OF_DATE") {
    @Override
    public DynamicFrameFunction createFrameFunction(FrameInfo info, int relativeCode,
        SpiceInfoHolder infoHolder) throws FKInstantiationException, KernelPoolValidationException {
      int frameCode = info.getCode();

      String precession = ParsingUtilities.parseString("FRAME_%s_PREC_MODEL", info, infoHolder);
      if (!precession.equals("EARTH_IAU_1976")) {
        throw new FKInstantiationException("Frame " + info.getName() + " , code " + info.getCode()
            + "is a Mean Ecliptic and Equinox of Date frame, which only supports the Earth IAU 1976 Precession model: "
            + precession);
      }
      String obliquity = ParsingUtilities.parseString("FRAME_%s_OBLIQ_MODEL", info, infoHolder);
      if (!obliquity.equals("EARTH_IAU_1980")) {
        throw new FKInstantiationException("Frame " + info.getName() + " , code " + info.getCode()
            + "is a Mean Ecliptic and Equinox of Date frame, which only supports the Earth IAU 1980 Obliquity model: "
            + obliquity);
      }
      DynamicFrameFunction dynamicFrameFunction =
          new DynamicMeanEclipticFrameFunction(frameCode, relativeCode);
      return applyRotationStateOrFreeze(info, dynamicFrameFunction, infoHolder, false);
    }
  },
  EULER_FRAMES("EULER") {
    @Override
    public DynamicFrameFunction createFrameFunction(FrameInfo info, int relativeCode,
        SpiceInfoHolder infoHolder) throws FKInstantiationException, KernelPoolValidationException {
      int frameCode = info.getCode();
      double epoch = ParsingUtilities.parseDouble("FRAME_%s_EPOCH", info, infoHolder);

      KernelPoolValidatingRetriever retriever = infoHolder.getRetriever();
      String unitsKeyword =
          FKFactory.getFrameKeyword("FRAME_%s_UNITS", frameCode, info.getName(), retriever);
      DynamicUnits units = DynamicUnits.fromString(retriever.getString(unitsKeyword));

      List<Integer> axes = ParsingUtilities.parseIntegers("FRAME_%s_AXES", info, infoHolder, 3);
      List<EulerAngles.Axis> axis = Lists.newArrayList();
      for (int i = 0; i < axes.size(); i++) {
        if (axes.get(i) == 1) {
          axis.add(EulerAngles.Axis.I);
        } else if (axes.get(i) == 2) {
          axis.add(EulerAngles.Axis.J);
        } else {
          axis.add(EulerAngles.Axis.K);
        }
      }

      List<Double> angle1Coeffs =
          ParsingUtilities.parseDoubles("FRAME_%s_ANGLE_1_COEFFS", info, infoHolder);
      List<Double> angle2Coeffs =
          ParsingUtilities.parseDoubles("FRAME_%s_ANGLE_2_COEFFS", info, infoHolder);
      List<Double> angle3Coeffs =
          ParsingUtilities.parseDoubles("FRAME_%s_ANGLE_3_COEFFS", info, infoHolder);
      DynamicFrameFunction dynamicFrameFunction = new DynamicEulerFrameFunction(frameCode,
          relativeCode, epoch, axis, angle1Coeffs, angle2Coeffs, angle3Coeffs, units);
      return applyRotationStateOrFreeze(info, dynamicFrameFunction, infoHolder, true);
    }
  };

  private final String name;

  private DynamicFrameProvider(String name) {
    this.name = name;
  }

  static private Map<String, DynamicFrameProvider> mapStringToProvider = new HashMap<>();
  static {
    for (DynamicFrameProvider provider : DynamicFrameProvider.values()) {
      mapStringToProvider.put(provider.getName(), provider);
    }
  }

  public static DynamicFrameProvider fromString(String name) {
    DynamicFrameProvider dynFrameProvider = mapStringToProvider.get(name);
    if (dynFrameProvider == null) {
      throw new RuntimeException("Invalid DynFrameProvider: " + name);
    }
    return dynFrameProvider;
  }

  public abstract DynamicFrameFunction createFrameFunction(FrameInfo info, int relativeCode,
      SpiceInfoHolder infoHolder) throws KernelPoolValidationException, FKInstantiationException;

  public String getName() {
    return name;
  }


  public static DynamicFrameFunction createFunction(FrameInfo info, SpiceInfoHolder infoHolder)
      throws KernelPoolValidationException, FKInstantiationException {
    String family = ParsingUtilities.parseString("FRAME_%s_FAMILY", info, infoHolder);
    DynamicFrameProvider provider = fromString(family);
    int relativeCode = ParsingUtilities.parseFrame("FRAME_%s_RELATIVE", info, infoHolder);

    return provider.createFrameFunction(info, relativeCode, infoHolder);
  }

  private static DynamicFrameFunction applyRotationStateOrFreeze(FrameInfo info,
      DynamicFrameFunction function, SpiceInfoHolder infoHolder, boolean optional)
      throws FKInstantiationException, KernelPoolValidationException {
    KernelPoolValidatingRetriever retriever = infoHolder.getRetriever();
    String name = info.getName();
    int id = info.getCode();
    String freezeStateKeyword =
        FKFactory.getFrameKeyword("FRAME_%s_FREEZE_EPOCH", id, name, retriever);
    if (retriever.containsKeyword(freezeStateKeyword)) {
      double freeze = retriever.getDouble(freezeStateKeyword);
      return new DynamicFrozenFrameFunction(function, freeze);
    }
    String rotationStateKeyword =
        FKFactory.getFrameKeyword("FRAME_%s_ROTATION_STATE", id, name, retriever);
    if (retriever.containsKeyword(rotationStateKeyword)) {
      String rotationState = retriever.getString(rotationStateKeyword);
      if (rotationState.equals("ROTATING")) {
        return function;
      } else if (rotationState.equals("INERTIAL")) {
        return new DynamicInertialFrameFunction(function);
      } else {
        throw new FKInstantiationException(
            "Rotation state for frame " + id + ", " + name + " is not valid: " + rotationState);
      }
    } else {
      if (optional) {
        return function;
      }
      throw new FKInstantiationException(
          "Lack of rotation state or freeze state for frame " + id + ", " + name + ".");
    }

  }
}
