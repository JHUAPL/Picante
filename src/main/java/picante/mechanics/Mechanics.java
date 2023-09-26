package picante.mechanics;

import static com.google.common.base.Preconditions.checkArgument;
import static picante.math.PicanteMath.PI;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.List;
import com.google.common.base.Predicate;
import com.google.common.base.Predicates;
import com.google.common.collect.Iterables;
import picante.exceptions.BugException;
import picante.math.intervals.IntervalSet;
import picante.mechanics.utilities.ChainLinkEngine;
import picante.mechanics.utilities.ChainLinkEngine.CodeProvider;

/**
 * utility methods for manipulating mechanics objects or related quantities
 * 
 * @author vandejd1
 */
public class Mechanics {
  /**
   * @param longitude east longitude in radians; other than this, there are no restrictions, i.e.,
   *        it can be -PI to PI, or 0 to 2PI, or even -5PI or 27PI.
   * @return local time in hours, with midnight (hour 0) at longitude PI and noon (hour 12) is
   *         longitude 0; the hours increase in the same way that east longitude increases (CCW
   *         about +Z)
   */
  public static HourAngle getLocalTime(double longitude) {
    // localtime is measured in hours, like a clock face, and is defined as:
    // "how far past the -X axis in the CCW direction about +Z";

    // the longitude indicates how far around from +X it is, so first
    // subtract off 180 degrees.
    longitude -= PI;

    // now make sure the resulting longitude is from 0 to 2*Pi:
    double TWOPI = PI * 2.0;
    while (longitude < 0) {
      longitude += TWOPI;
    }
    while (longitude > TWOPI) {
      longitude -= TWOPI;
    }

    // now convert to hours:
    return new HourAngle(longitude / TWOPI * 24.0);
  }

  /**
   * Creates an {@link IntervalSet} containing a snapshot of the basic coverage of a given
   * {@link FrameID} within a list of {@link FrameSource}s.
   * <p>
   * Recall that the frame chaining system defines a tree of leaf to node connections, where each
   * link in the tree is an individual {@link FrameTransformFunction}. This method simply looks for
   * all sources that have frame as their leaf {@link FrameTransformFunction#getFromID()}, and
   * snapshots the aggregated coverage.
   * </p>
   * <p>
   * If you are connecting one frame to another through multiple links in a tree, this only tells
   * one part of the coverage story. It's entirely possible that some link further up the tree will
   * have gaps independent of those reported here.
   * </p>
   * 
   * @param sources the sources to consider
   * @param frame the frame ID of interest to locate basic coverage
   * @param stateTransformsOnly a boolean indicating whether to select only
   *        {@link StateTransformFunctions} or not
   * 
   * @return an {@link IntervalSet} containing the basic coverage of frame from amongst the supplied
   *         sources.
   * 
   * @throws FrameSourceIOException if a problem arises reading the source data
   */
  public static IntervalSet getBasicCoverageSnapshot(List<FrameTransformFunction> sources,
      FrameID frame, boolean stateTransformsOnly) throws FrameSourceIOException {
    IntervalSet.Builder resultBuilder = IntervalSet.builder();

    /*
     * The fromID describes the frame being defined, so search for these.
     */
    Predicate<FrameTransformFunction> idFilter = (func) -> {
      return frame.equals(func.getFromID());
    };
    Predicate<FrameTransformFunction> filter = idFilter;

    /*
     * Change filter if the caller only wants to consider state transforms.
     */
    if (stateTransformsOnly) {
      Predicate<FrameTransformFunction> stateTransformFilter = (func) -> {
        return func instanceof StateTransformFunction;
      };
      filter = Predicates.and(idFilter, stateTransformFilter);
    }

    for (FrameTransformFunction f : Iterables.filter(sources, filter)) {
      resultBuilder.union(Coverages.snapshotToIntervalSet(f.getCoverage()));
    }

    return resultBuilder.build();
  }


  /**
   * Creates an {@link IntervalSet} containing a snapshot of the basic coverage of a given
   * {@link EphemerisID} within a list of {@link EphemerisSource}s.
   * <p>
   * Recall that the ephemeris chaining system defines a tree of leaf to node connections, where
   * each link in the tree is an individual {@link PositionVectorFunction}. This method simply looks
   * for all sources that have object as their leaf {@link PositionVectorFunction#getTargetID()},
   * and snapshots the aggregated coverage.
   * </p>
   * <p>
   * If you are connecting one object to another through multiple links in a tree, this only tells
   * one part of the coverage story. It's entirely possible that the underlying required frames will
   * have a coverage gap, or some link further up the tree will have gaps independent of those
   * reported by this method.
   * </p>
   * 
   * @param sources the sources to consider
   * @param object the ephemeris ID of interest for which to locate basic coverage
   * @param stateVectorsOnly a boolean indicating whether to select only {@link StateVectorFunction}
   *        or not
   * @return an {@link IntervalSet} containing the basic coverage of object from amongst the
   *         supplied sources.
   * 
   * @throws EphemerisSourceIOException if a problem arises reading the source data
   */
  public static IntervalSet getBasicCoverageSnapshot(List<PositionVectorFunction> sources,
      EphemerisID object, boolean stateVectorsOnly) throws EphemerisSourceIOException {
    IntervalSet.Builder resultBuilder = IntervalSet.builder();

    /*
     * The targetID describes the object being defined, so search for these.
     */
    Predicate<PositionVectorFunction> idFilter = (func) -> {
      return object.equals(func.getTargetID());
    };
    Predicate<PositionVectorFunction> filter = idFilter;

    /*
     * Change the filter if the caller only wants to consider state vector functions.
     */
    if (stateVectorsOnly) {
      Predicate<PositionVectorFunction> stateVectorFilter = (func) -> {
        return func instanceof StateVectorFunction;
      };
      filter = Predicates.and(idFilter, stateVectorFilter);
    }
    for (PositionVectorFunction f : Iterables.filter(sources, filter)) {
      resultBuilder.union(Coverages.snapshotToIntervalSet(f.getCoverage()));
    }

    return resultBuilder.build();

  }



  /**
   * Method for creating proxy implementations of an interface to be utilized as a marker arguments
   * to the ChainLinkEngine constructor.
   * <p>
   * This method does not create implementations that are the most efficient with regards to
   * interfacing with the Collections API (i.e. equals and hashCode), but they should be fast
   * enough.
   * 
   * @param <F> the interface type to proxy
   * @param interfaceType the class identifying the interface to proxy
   * 
   * @return an implementation of interfaceType where every method on the interface throws
   *         {@link UnsupportedOperationException} because they should never be invoked.
   */
  @SuppressWarnings("unchecked")
  static <F> F createMarkerFunctionProxy(Class<F> interfaceType) {
    checkArgument(interfaceType.isInterface(), "Type must be an interface");

    Method equals, hashCode;
    try {
      equals = Object.class.getMethod("equals", Object.class);
      hashCode = Object.class.getMethod("hashCode");
    } catch (Exception e) {
      throw new BugException("Object has equals and hashCode methods, this should never happen", e);
    }

    InvocationHandler handler = (o, m, a) -> {

      /*
       * Just emulate what equals and hashCode on Object do...
       */
      if (m.equals(equals)) {
        return o == a[0];
      } else if (m.equals(hashCode)) {
        return System.identityHashCode(o);
      }

      throw new UnsupportedOperationException("This instance is simply a marker implementation");
    };
    return (F) Proxy.newProxyInstance(interfaceType.getClassLoader(), new Class[] {interfaceType},
        handler);
  }

  /**
   * Implementation of the {@link ChainLinkEngine.CodeProvider} interface for ephemerides
   */
  static class EphemerisCodeProvider implements CodeProvider<EphemerisID, PositionVectorFunction> {

    @Override
    public EphemerisID getLeafCode(PositionVectorFunction function) {
      return function.getTargetID();
    }

    @Override
    public EphemerisID getNodeCode(PositionVectorFunction function) {
      return function.getObserverID();
    }

    @Override
    public boolean validAt(PositionVectorFunction function, double time) {
      return function.getCoverage().contains(time);
    }

  }

  /**
   * Creates an instance of a {@link ChainLinkEngine} from the list of source position functions.
   * 
   * @param functions a list of functions with the lowest index being the lowest priority
   * 
   * @return newly created instance of the ChainLinkEngine with the supplied source functions
   *         installed for interrogation
   */
  public static ChainLinkEngine<EphemerisID, PositionVectorFunction> createPositionVectorEngine(
      List<? extends PositionVectorFunction> functions) {
    return new ChainLinkEngine<>(functions, new EphemerisCodeProvider(),
        createMarkerFunctionProxy(PositionVectorFunction.class),
        createMarkerFunctionProxy(PositionVectorFunction.class));
  }

  /**
   * Creates an instance of a {@link ChainLinkEngine} from the list of source state functions.
   * 
   * @param functions list of functions with the lowest index being the lowest priority
   * 
   * @return newly created instance of the ChainLinkEngine with the supplied source state vector
   *         functions installed for interrogation
   */
  public static ChainLinkEngine<EphemerisID, StateVectorFunction> createStateVectorEngine(
      List<? extends StateVectorFunction> functions) {
    return new ChainLinkEngine<>(functions, new EphemerisCodeProvider(),
        createMarkerFunctionProxy(StateVectorFunction.class),
        createMarkerFunctionProxy(StateVectorFunction.class));
  }

  /**
   * Implementation of the {@link ChainLinkEngine.CodeProvider} interface for frame transformations
   */
  static class FrameCodeProvider implements CodeProvider<FrameID, FrameTransformFunction> {

    @Override
    public FrameID getLeafCode(FrameTransformFunction function) {
      return function.getFromID();
    }

    @Override
    public FrameID getNodeCode(FrameTransformFunction function) {
      return function.getToID();
    }

    @Override
    public boolean validAt(FrameTransformFunction function, double time) {
      return function.getCoverage().contains(time);
    }

  }

  /**
   * Creates an instance of a {@link ChainLinkEngine} from the list of source frame transform
   * functions.
   * 
   * @param functions list of functions with the lowest index being the lowest priority
   * 
   * @return newly created instance of the ChainLinkEngine with the supplied source frame transform
   *         functions installed for interrogation
   */
  public static ChainLinkEngine<FrameID, FrameTransformFunction> createFrameTransformEngine(
      List<? extends FrameTransformFunction> functions) {
    return new ChainLinkEngine<>(functions, new FrameCodeProvider(),
        createMarkerFunctionProxy(FrameTransformFunction.class),
        createMarkerFunctionProxy(FrameTransformFunction.class));
  }

  /**
   * Creates an instance of a {@link ChainLinkEngine} from the list of source state transform
   * functions.
   * 
   * @param functions list of functions with the lowest index being the lowest priority
   * 
   * @return newly created instance of the ChainLinkEngine with the supplied source state transform
   *         functions installed for interrogation
   */
  public static ChainLinkEngine<FrameID, StateTransformFunction> createStateTransformEngine(
      List<? extends StateTransformFunction> functions) {
    return new ChainLinkEngine<>(functions, new FrameCodeProvider(),
        createMarkerFunctionProxy(StateTransformFunction.class),
        createMarkerFunctionProxy(StateTransformFunction.class));
  }



}
