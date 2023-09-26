package picante.spice;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableListMultimap;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ListMultimap;
import picante.mechanics.EphemerisAndFrameProvider;
import picante.mechanics.EphemerisID;
import picante.mechanics.EphemerisSourceIOException;
import picante.mechanics.EphemerisSourceLinkException;
import picante.mechanics.FrameID;
import picante.mechanics.FrameSourceIOException;
import picante.mechanics.FrameSourceLinkException;
import picante.mechanics.FrameTransformFunction;
import picante.mechanics.PositionVectorFunction;
import picante.mechanics.providers.aberrated.AberratedEphemerisProvider;
import picante.mechanics.providers.lockable.LockableEphemerisProvider;
import picante.spice.adapters.AdapterInstantiationException;
import picante.spice.adapters.EphemerisSourceFactory;
import picante.spice.adapters.FrameSourceFactory;
import picante.spice.kernel.ck.CK;
import picante.spice.kernel.pck.PCK;
import picante.spice.kernel.spk.SPK;
import picante.spice.kernel.tk.fk.FrameKernel;
import picante.spice.kernel.tk.fk.dynamic.DynamicFrameDefinitionException;
import picante.spice.kernel.tk.fk.dynamic.DynamicStateTransformFunction;
import picante.spice.kernel.tk.lsk.LSK;
import picante.spice.kernel.tk.pck.TextPCK;
import picante.spice.kernel.tk.sclk.SCLKKernel;
import picante.spice.kernelpool.BasicKernelPool;
import picante.spice.kernelpool.UnwritableKernelPool;

/**
 * 
 * <p>
 * As text kernels are potentially destructive when loaded, as they may rewire all the ephemeris and
 * frame code bindings; they are not permitted to be converted here. However, as binary kernels are
 * not capable of adjusting any existing bindings (they may add new source IDs...) they are
 * permitted to be converted.
 * </p>
 */
public class SpiceEnvironment {
  private final boolean ignoreFaultyFrames;
  /*
   * Adapters
   */
  private final EphemerisSourceFactory ephemerisFactory;
  private final FrameSourceFactory frameFactory;

  /*
   * Data sources.
   */
  private final BasicKernelPool pool;
  private final ImmutableList<CK> cks;
  private final ImmutableList<SPK> spks;
  private final ImmutableList<PCK> pcks;
  private final TextPCK textPCK;
  private final FrameKernel fk;
  private final ImmutableMap<Integer, SCLKKernel> sclks;
  private final ImmutableMap<FrameID, EphemerisID> frameCenterMap;
  private final LSK lsk;
  private final ImmutableListMultimap<EphemerisID, Double> bodyRadii;
  private final ImmutableList<PositionVectorFunction> ephemerisSources;
  private final ImmutableList<FrameTransformFunction> frameSources;

  SpiceEnvironment(EphemerisSourceFactory ephemerisFactory, FrameSourceFactory frameFactory,
      TextPCK textPCK, FrameKernel fk, BasicKernelPool pool, Collection<CK> cks,
      Collection<SPK> spks, Collection<PCK> pcks, Map<Integer, SCLKKernel> sclks, LSK lsk,
      Map<FrameID, EphemerisID> frameCenterMap, ListMultimap<EphemerisID, Double> bodyRadii,
      boolean ignoreFaultyFrames) {
    this.ignoreFaultyFrames = ignoreFaultyFrames;
    this.ephemerisFactory = ephemerisFactory;
    this.frameFactory = frameFactory;
    this.textPCK = textPCK;
    this.fk = fk;
    this.cks = ImmutableList.copyOf(cks);
    this.spks = ImmutableList.copyOf(spks);
    this.pcks = ImmutableList.copyOf(pcks);
    this.sclks = ImmutableMap.copyOf(sclks);
    this.pool = pool;
    this.frameCenterMap = ImmutableMap.copyOf(frameCenterMap);
    this.lsk = lsk;
    this.bodyRadii = ImmutableListMultimap.copyOf(bodyRadii);
    this.ephemerisSources = createEphemerisSources();
    this.frameSources = createFrameSources();
  }

  public ImmutableList<PositionVectorFunction> getEphemerisSources() {
    return ephemerisSources;
  }

  public ImmutableList<FrameTransformFunction> getFrameSources() {
    return frameSources;
  }

  ImmutableList<PositionVectorFunction> createEphemerisSources() {

    ImmutableList.Builder<PositionVectorFunction> resultBuilder = ImmutableList.builder();

    for (SPK spk : spks) {
      resultBuilder.addAll(ephemerisFactory.createEphemerisSource(spk));
    }

    return resultBuilder.build();
  }

  ImmutableList<FrameTransformFunction> createFrameSources() {

    ImmutableList.Builder<FrameTransformFunction> resultBuilder = ImmutableList.builder();

    resultBuilder.addAll(frameFactory.createBuiltInInertialFrameSource());

    resultBuilder.addAll(frameFactory.createFrameSource(textPCK));

    resultBuilder.addAll(frameFactory.createFrameSource(fk));
    try {
      for (CK ck : cks) {
        resultBuilder.addAll(frameFactory.createFrameSource(ck));
      }

      for (PCK pck : pcks) {
        resultBuilder.addAll(frameFactory.createFrameSource(pck));
      }
      List<DynamicStateTransformFunction> dynamicFrameFunctions =
          frameFactory.createDynamicFrameSource(fk);
      Map<Integer, FrameID> frameIDMap = frameFactory.getFrameIDMap();
      Map<Integer, EphemerisID> ephemerisIDMap = ephemerisFactory.getEphemerisIdMap();
      resultBuilder.addAll(dynamicFrameFunctions);
      GeneralAberratedEphemerisProvider provider = new GeneralAberratedEphemerisProvider(
          new LockableEphemerisProvider(ephemerisSources, resultBuilder.build()), frameCenterMap);
      for (FrameTransformFunction stf : dynamicFrameFunctions) {
        if (stf instanceof DynamicStateTransformFunction) {
          DynamicStateTransformFunction dynStf = (DynamicStateTransformFunction) stf;
          try {
            dynStf.define(provider, frameIDMap, ephemerisIDMap);
          } catch (DynamicFrameDefinitionException | EphemerisSourceIOException
              | EphemerisSourceLinkException | FrameSourceIOException
              | FrameSourceLinkException e) {
            if (!ignoreFaultyFrames) {
              throw new DynamicFrameDefinitionException("Faulty dynamic frame from "
                  + dynStf.getFromID() + " to " + dynStf.getToID()
                  + " could not be constructed. This can be ignored by calling SpiceEnvironmentBuilder.setIgnoreFaultyFrames(true).",
                  e);
            }
          }
        }
      }
    } catch (AdapterInstantiationException e) {
      throw new RuntimeException(e);
    }
    return resultBuilder.build();
  }

  public UnwritableKernelPool getPool() {
    return pool;
  }

  /**
   * Stop gap method that provides access to the SPICE SCLK implementation. Note this map maps the
   * SCLK integer ID codes used in SPICE to their corresponding SCLKs. Instead of using the string
   * lookup capability, because none of the APIs in SPICE for the SCLK system utilize the string
   * name of the spacecraft.
   * 
   * @return a map containing all of the identified and created SCLK content
   */
  public ImmutableMap<Integer, SCLKKernel> getSclkKernels() {
    return sclks;
  }

  public ImmutableList<PositionVectorFunction> createEphemerisSource(SPK kernel)
      throws AdapterInstantiationException {
    return ephemerisFactory.createEphemerisSource(kernel);
  }

  public ImmutableList<FrameTransformFunction> createFrameSource(PCK kernel)
      throws AdapterInstantiationException {
    return frameFactory.createFrameSource(kernel);
  }

  public ImmutableList<FrameTransformFunction> createFrameSource(CK kernel)
      throws AdapterInstantiationException {
    return frameFactory.createFrameSource(kernel);
  }

  public ImmutableMap<FrameID, EphemerisID> getFrameCenterMap() {
    return frameCenterMap;
  }

  /**
   * This can be used to configure the TimeSystems builder in the core.
   * 
   * <code>
   *    LSK lsk = env.getLSK();
   *    TimeSystems.Builder builder = lsk.configure(TimeSystems.builder());
   *    TimeSystems systems = builder.build();
   * </code>
   * 
   * @return
   */
  public LSK getLSK() {
    return lsk;
  }

  /**
   * Retrieves the mapping from ephemeris body IDs to the three body radii. The first is associated
   * with the X-axis of the body fixed frame, second Y-axis, and third Z-axis (rotation axis).
   * 
   * @return
   */
  public ImmutableListMultimap<EphemerisID, Double> getBodyRadii() {
    return bodyRadii;
  }

  /**
   * Utility method for creating a single iteration {@link AberratedEphemerisProvider}.
   * 
   * @return aberratedProvider
   * @throws AdapterInstantiationException
   */
  public AberratedEphemerisProvider createSingleAberratedProvider()
      throws AdapterInstantiationException {
    EphemerisAndFrameProvider provider =
        new LockableEphemerisProvider(getEphemerisSources(), getFrameSources());
    AberratedEphemerisProvider aberratedProvider =
        AberratedEphemerisProvider.createSingleIteration(provider, frameCenterMap);
    return aberratedProvider;
  }

  /**
   * Utility method for creating a triple iteration {@link AberratedEphemerisProvider}.
   * 
   * @return aberratedProvider
   * @throws AdapterInstantiationException
   */
  public AberratedEphemerisProvider createTripleAberratedProvider()
      throws AdapterInstantiationException {
    EphemerisAndFrameProvider provider =
        new LockableEphemerisProvider(getEphemerisSources(), getFrameSources());
    AberratedEphemerisProvider aberratedProvider =
        AberratedEphemerisProvider.createTripleIteration(provider, frameCenterMap);
    return aberratedProvider;
  }
}
