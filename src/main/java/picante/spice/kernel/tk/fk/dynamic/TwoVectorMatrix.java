package picante.spice.kernel.tk.fk.dynamic;

import com.google.common.base.Preconditions;
import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.StateTransform;
import picante.mechanics.StateVector;
import picante.mechanics.UnwritableStateVector;

public class TwoVectorMatrix {
  private final Axis pri;
  private final Axis sec;

  public TwoVectorMatrix(Axis pri, Axis sec) {
    Preconditions.checkArgument(!pri.equals(sec),
        "Axes cannot be the same axis: " + pri + " " + sec);

    if (sec.equals(pri.getOpposite())) {
      throw new IllegalArgumentException(
          "Axes cannot be the same axis, regardless of polarity: " + pri + " " + sec);
    }
    this.pri = pri;
    this.sec = sec;
  }

  public RotationMatrixIJK makeMatrix(UnwritableVectorIJK priVec, UnwritableVectorIJK secVec) {
    if (pri.equals(Axis.I) || pri.equals(Axis.MINUS_I)) {
      if (pri.isNegative()) {
        priVec = priVec.createNegated();
      }
      if (sec.isNegative()) {
        secVec = secVec.createNegated();
      }
      if (sec.equals(Axis.J) || sec.equals(Axis.MINUS_J)) {
        return crossIJ(priVec, secVec);
      } else if (sec.equals(Axis.K) || sec.equals(Axis.MINUS_K)) {
        return crossIK(priVec, secVec);
      }
    } else if (pri.equals(Axis.J) || pri.equals(Axis.MINUS_J)) {
      if (pri.isNegative()) {
        priVec = priVec.createNegated();
      }
      if (sec.isNegative()) {
        secVec = secVec.createNegated();
      }
      if (sec.equals(Axis.I) || sec.equals(Axis.MINUS_I)) {
        return crossJI(priVec, secVec);
      } else if (sec.equals(Axis.K) || sec.equals(Axis.MINUS_K)) {
        return crossJK(priVec, secVec);
      }
    } else if (pri.equals(Axis.K) || pri.equals(Axis.MINUS_K)) {
      if (pri.isNegative()) {
        priVec = priVec.createNegated();
      }
      if (sec.isNegative()) {
        secVec = secVec.createNegated();
      }
      if (sec.equals(Axis.J) || sec.equals(Axis.MINUS_J)) {
        return crossKJ(priVec, secVec);
      } else if (sec.equals(Axis.I) || sec.equals(Axis.MINUS_I)) {
        return crossKI(priVec, secVec);
      }
    }
    throw new RuntimeException("Invalid Axes: " + pri + " " + sec);
  }

  public StateTransform makeStateTransform(UnwritableStateVector priVec,
      UnwritableStateVector secVec) {
    if (pri.equals(Axis.I) || pri.equals(Axis.MINUS_I)) {
      if (pri.isNegative()) {
        priVec = priVec.createNegated();
      }
      if (sec.isNegative()) {
        secVec = secVec.createNegated();
      }
      if (sec.equals(Axis.J) || sec.equals(Axis.MINUS_J)) {
        return crossIJ(priVec, secVec);
      } else if (sec.equals(Axis.K) || sec.equals(Axis.MINUS_K)) {
        return crossIK(priVec, secVec);
      }
    } else if (pri.equals(Axis.J) || pri.equals(Axis.MINUS_J)) {
      if (pri.isNegative()) {
        priVec = priVec.createNegated();
      }
      if (sec.isNegative()) {
        secVec = secVec.createNegated();
      }
      if (sec.equals(Axis.I) || sec.equals(Axis.MINUS_I)) {
        return crossJI(priVec, secVec);
      } else if (sec.equals(Axis.K) || sec.equals(Axis.MINUS_K)) {
        return crossJK(priVec, secVec);
      }
    } else if (pri.equals(Axis.K) || pri.equals(Axis.MINUS_K)) {
      if (pri.isNegative()) {
        priVec = priVec.createNegated();
      }
      if (sec.isNegative()) {
        secVec = secVec.createNegated();
      }
      if (sec.equals(Axis.J) || sec.equals(Axis.MINUS_J)) {
        return crossKJ(priVec, secVec);
      } else if (sec.equals(Axis.I) || sec.equals(Axis.MINUS_I)) {
        return crossKI(priVec, secVec);
      }
    }
    throw new RuntimeException("Invalid Axes: " + pri + " " + sec);
  }


  private static RotationMatrixIJK crossIJ(UnwritableVectorIJK i, UnwritableVectorIJK j) {
    UnwritableVectorIJK rotI = i.createUnitized();
    VectorIJK rotK = VectorIJK.uCross(rotI, j);
    VectorIJK rotJ = VectorIJK.uCross(rotK, rotI);
    return new RotationMatrixIJK(rotI, rotJ, rotK);
  }

  private static RotationMatrixIJK crossIK(UnwritableVectorIJK i, UnwritableVectorIJK k) {
    UnwritableVectorIJK rotI = i.createUnitized();
    VectorIJK rotJ = VectorIJK.uCross(rotI, k.createNegated());
    VectorIJK rotK = VectorIJK.uCross(rotI, rotJ);
    return new RotationMatrixIJK(rotI, rotJ, rotK);
  }

  private static RotationMatrixIJK crossJI(UnwritableVectorIJK j, UnwritableVectorIJK i) {
    UnwritableVectorIJK rotJ = j.createUnitized();
    VectorIJK rotK = VectorIJK.uCross(rotJ, i.createNegated());
    VectorIJK rotI = VectorIJK.uCross(rotJ, rotK);
    return new RotationMatrixIJK(rotI, rotJ, rotK);
  }

  private static RotationMatrixIJK crossJK(UnwritableVectorIJK j, UnwritableVectorIJK k) {
    UnwritableVectorIJK rotJ = j.createUnitized();
    VectorIJK rotI = VectorIJK.uCross(rotJ, k);
    VectorIJK rotK = VectorIJK.uCross(rotI, rotJ);
    return new RotationMatrixIJK(rotI, rotJ, rotK);
  }

  private static RotationMatrixIJK crossKI(UnwritableVectorIJK k, UnwritableVectorIJK i) {
    UnwritableVectorIJK rotK = k.createUnitized();
    VectorIJK rotJ = VectorIJK.uCross(rotK, i);
    VectorIJK rotI = VectorIJK.uCross(rotJ, rotK);
    return new RotationMatrixIJK(rotI, rotJ, rotK);
  }

  private static RotationMatrixIJK crossKJ(UnwritableVectorIJK k, UnwritableVectorIJK j) {
    UnwritableVectorIJK rotK = k.createUnitized();
    VectorIJK rotI = VectorIJK.uCross(rotK, j.createNegated());
    VectorIJK rotJ = VectorIJK.uCross(rotK, rotI);
    return new RotationMatrixIJK(rotI, rotJ, rotK);
  }

  private static StateTransform crossIJ(UnwritableStateVector i, UnwritableStateVector j) {
    UnwritableStateVector rotI = i.createUnitized();
    StateVector rotK = StateVector.uCross(rotI, j);
    StateVector rotJ = StateVector.uCross(rotK, rotI);
    RotationMatrixIJK rotMat =
        new RotationMatrixIJK(rotI.getPosition(), rotJ.getPosition(), rotK.getPosition());
    MatrixIJK dRotMat = new MatrixIJK(rotI.getVelocity(), rotJ.getVelocity(), rotK.getVelocity());
    return new StateTransform(rotMat, dRotMat);
  }

  private static StateTransform crossIK(UnwritableStateVector i, UnwritableStateVector k) {
    UnwritableStateVector rotI = i.createUnitized();
    StateVector rotJ = StateVector.uCross(rotI, k.createNegated());
    StateVector rotK = StateVector.uCross(rotI, rotJ);
    RotationMatrixIJK rotMat =
        new RotationMatrixIJK(rotI.getPosition(), rotJ.getPosition(), rotK.getPosition());
    MatrixIJK dRotMat = new MatrixIJK(rotI.getVelocity(), rotJ.getVelocity(), rotK.getVelocity());
    return new StateTransform(rotMat, dRotMat);
  }

  private static StateTransform crossJI(UnwritableStateVector j, UnwritableStateVector i) {
    UnwritableStateVector rotJ = j.createUnitized();
    StateVector rotK = StateVector.uCross(rotJ, i.createNegated());
    StateVector rotI = StateVector.uCross(rotJ, rotK);
    RotationMatrixIJK rotMat =
        new RotationMatrixIJK(rotI.getPosition(), rotJ.getPosition(), rotK.getPosition());
    MatrixIJK dRotMat = new MatrixIJK(rotI.getVelocity(), rotJ.getVelocity(), rotK.getVelocity());
    return new StateTransform(rotMat, dRotMat);
  }

  private static StateTransform crossJK(UnwritableStateVector j, UnwritableStateVector k) {
    UnwritableStateVector rotJ = j.createUnitized();
    StateVector rotI = StateVector.uCross(rotJ, k);
    StateVector rotK = StateVector.uCross(rotI, rotJ);
    RotationMatrixIJK rotMat =
        new RotationMatrixIJK(rotI.getPosition(), rotJ.getPosition(), rotK.getPosition());
    MatrixIJK dRotMat = new MatrixIJK(rotI.getVelocity(), rotJ.getVelocity(), rotK.getVelocity());
    return new StateTransform(rotMat, dRotMat);
  }

  private static StateTransform crossKI(UnwritableStateVector k, UnwritableStateVector i) {
    UnwritableStateVector rotK = k.createUnitized();
    StateVector rotJ = StateVector.uCross(rotK, i);
    StateVector rotI = StateVector.uCross(rotJ, rotK);
    RotationMatrixIJK rotMat =
        new RotationMatrixIJK(rotI.getPosition(), rotJ.getPosition(), rotK.getPosition());
    MatrixIJK dRotMat = new MatrixIJK(rotI.getVelocity(), rotJ.getVelocity(), rotK.getVelocity());
    return new StateTransform(rotMat, dRotMat);
  }

  private static StateTransform crossKJ(UnwritableStateVector k, UnwritableStateVector j) {
    UnwritableStateVector rotK = k.createUnitized();
    StateVector rotI = StateVector.uCross(rotK, j.createNegated());
    StateVector rotJ = StateVector.uCross(rotK, rotI);
    RotationMatrixIJK rotMat =
        new RotationMatrixIJK(rotI.getPosition(), rotJ.getPosition(), rotK.getPosition());
    MatrixIJK dRotMat = new MatrixIJK(rotI.getVelocity(), rotJ.getVelocity(), rotK.getVelocity());
    return new StateTransform(rotMat, dRotMat);
  }
}

