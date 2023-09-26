package picante.spice;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

import com.google.common.collect.ImmutableListMultimap;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ListMultimap;
import com.google.common.io.ByteSource;
import picante.designpatterns.BuildFailedException;
import picante.designpatterns.Builder;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;
import picante.spice.FrameIDBindingManager.BindingResult;
import picante.spice.adapters.EphemerisSourceFactory;
import picante.spice.adapters.FrameSourceFactory;
import picante.spice.adapters.SclkRegistry;
import picante.spice.adapters.SpiceEphemerisID;
import picante.spice.kernel.ForgivingKernelLoader;
import picante.spice.kernel.KernelInstantiationException;
import picante.spice.kernel.KernelLoader;
import picante.spice.kernel.tk.TextKernel;
import picante.spice.kernel.tk.fk.FrameKernel;
import picante.spice.kernel.tk.fk.FrameType;
import picante.spice.kernel.tk.lsk.LSK;
import picante.spice.kernel.tk.lsk.LSKInstantiationException;
import picante.spice.kernel.tk.lsk.UniformTimeProvider;
import picante.spice.kernel.tk.pck.TextPCK;
import picante.spice.kernel.tk.sclk.EncodedSCLKConverter;
import picante.spice.kernel.tk.sclk.SCLKKernel;
import picante.spice.kernelpool.BasicKernelPool;
import picante.spice.kernelpool.content.FKFactory;
import picante.spice.kernelpool.content.KernelPoolContentFactory;
import picante.spice.kernelpool.content.LSKFactory;
import picante.spice.kernelpool.content.PCKFactory;
import picante.spice.kernelpool.content.SCLKFactory;
import picante.spice.provided.EphemerisNames;
import picante.spice.provided.FrameNames;

/**
 * This class provides a means of assembling a {@link SpiceEnvironment}. Basically to use this
 * class, simply instantiate it, supply the appropriate ephemeris and frame ID code bindings, load
 * the kernels into the builder in the order you would have loaded them into a SPICE application,
 * then invoke {@link SpiceEnvironmentBuilder#build()} to create the environment.
 * <p>
 * For example, you can do the following:
 * </p>
 * 
 * <pre>
 * SpiceEnvironment e = new SpiceEnvironmentBuilder().bindEphemerisID(-82, cassiniID)
 *     .bindEphemerisID(1, ssbID).load(&quot;naif0009.tls&quot;, new File(&quot;naif0009.tls&quot;))
 *     .load(&quot;cassini.bsp&quot;, new File(&quot;cassini.bsp&quot;)).build();
 * </pre>
 * 
 * <p>
 * A word about ID code bindings: if you omit bindings for a particular frame or ephemeris object an
 * appropriate SPICE ID code will be created for you. However, you will have to know the SPICE
 * integer code for the object of interest to you in order to properly retrieve it from the higher
 * level crucible interfaces. If you are simply connecting to another object of interest that is
 * chained through one of these internally named intermediate, unbound IDs, then you never need to
 * know the (internally assigned) ids of the intermediates.
 * </p>
 * <p>
 * You can load and unload kernels from this builder, much like NAIF's FURNSH method. Each SPICE
 * environment created with the build() method will be independent of all others with regards to
 * configuration. It is worth mentioning however, though independent, they are using some of the
 * same underlying objects. And none of this code was built with thread safety in mind. Each of the
 * load methods take a String to associate with the data as an identifier. These identifiers need
 * not be unique, but unloading an identifier that has been used multiple times in the same builder
 * will only result in the last loaded item being removed from the builder. In this way it works
 * much like the UNLOAD method of SPICE.
 * </p>
 */
public class SpiceEnvironmentBuilder implements Builder<SpiceEnvironment, BuildFailedException> {

  private final FrameNames frameNames = new FrameNames();

  private final EphemerisIDBindingManager ephemerisBindings =
      new EphemerisIDBindingManager(new NameIDBindingList<EphemerisID>(), new EphemerisNames());
  private final FrameIDBindingManager frameBindings =
      new FrameIDBindingManager(new NameIDBindingList<FrameID>(), frameNames);


  /**
   * Boolean to ignore any faulty dynamic frames during construction time, therefore causing errors
   * to happen at runtime
   */
  private boolean ignoreFaultyFrames = false;
  
  /**
   * Kernel manager that tracks loaded files and connects them with the identifiers supplied at load
   * time.
   */
  private final KernelManager manager;

  /**
   * Kernel loader that takes the various input formats, determines the data type connected to the
   * type, and instantiates the necessary SPICE objects.
   */
  private final KernelLoader loader;

  /**
   * Forgiving kernel loader that takes the various input formats, determines the data type
   * connected to the type, and instantiates the necessary SPICE objects. It does this in a
   * forgiving manner, i.e. if a current data type is not supported it attempts to ignore it.
   */
  private final ForgivingKernelLoader forgivingLoader;

  /**
   * User provided TDT converter used with some encoded SCLK converters. If null, the default SPICE
   * leapseconds kernel content will be used to satisfy this interface. @see
   * {@link SpiceEnvironmentBuilder#getTdtProvider(BasicKernelPool)} for details.
   */
  private UniformTimeProvider userTdtProvider;

  /**
   * Creates the environment builder.
   */
  public SpiceEnvironmentBuilder() {
    this(new KernelManager(), new KernelLoader(), new ForgivingKernelLoader());
  }

  /**
   * Package private environment builder constructor used to inject dependencies for test purposes.
   * 
   * @param userEphemerisIDMap
   * @param userFrameIdMap
   * @param userCkIdMap
   * @param userSclkMap
   * @param userCkSclkIdMap
   * @param userCkSpecificMap
   * @param manager
   * @param loader
   */
  SpiceEnvironmentBuilder(KernelManager manager, KernelLoader loader,
      ForgivingKernelLoader forgivingLoader) {
    this.manager = manager;
    this.loader = loader;
    this.forgivingLoader = forgivingLoader;
  }

  /**
   * Bind a SPICE ephemeris object name with the supplied EphemerisID
   * 
   * @param name a string indicating the SPICE name of an ephemeris object
   * @param id an EphemerisID to associate with the object
   * 
   * @return a reference to the builder to support method chaining
   */
  public SpiceEnvironmentBuilder bindEphemerisID(String name, EphemerisID id) {
    ephemerisBindings.add(name, id);
    return this;
  }

  /**
   * Bind a set of SPICE ephemeris object names with their paired EphemerisID
   * <p>
   * Note: this method inserts bindings into a prioritized list of binding requests in the iteration
   * order of the key set supplied by the map. This is only really a cautionary message if you
   * provide different string names that map to the same SPICE ephemeris object. The last entry in
   * the key set will take precedence.
   * </p>
   * 
   * @param bindings a map of SPICE ephemeris object names to the desired EphemerisID
   * 
   * @return a reference to the builder to support method chaining
   */
  public SpiceEnvironmentBuilder bindAllEphemerisID(Map<String, ? extends EphemerisID> bindings) {
    ephemerisBindings.addAll(bindings);
    return this;
  }

  /**
   * Remove the binding for the supplied SPICE ephemeris object name. Note: this operation is a
   * no-op if the code has not already been bound to an EphemerisID.
   * 
   * @param name the SPICE ephemeris object name to remove a binding for
   * 
   * @return a reference to the builder to support method chaining
   */
  public SpiceEnvironmentBuilder unbindEphemerisID(String name) {
    ephemerisBindings.remove(name);
    return this;
  }

  /**
   * Remove a collection of SPICE ephemeris object name bindings. Note: any names specified to be
   * removed that have not already been bound will simply be ignored.
   * 
   * @param codes an iterable of SPICE ephemeris object names to unbind
   * 
   * @return a reference to the builder to support method chaining
   */
  public SpiceEnvironmentBuilder unbindAllEphemerisID(Iterable<String> names) {
    ephemerisBindings.removeAll(names);
    return this;
  }

  /**
   * Set the ignoreFaultyFrames boolean
   */
  public SpiceEnvironmentBuilder setIgnoreFaultyFrames(boolean ignoreFaultyFrames) {
    this.ignoreFaultyFrames = ignoreFaultyFrames;
    return this;
  }


  /**
   * Bind a SPICE frame name with the supplied FrameID
   * 
   * @param name a string indicating the SPICE name of a frame
   * @param id an FrameID to associate with the frame
   * 
   * @return a reference to the builder to support method chaining
   */
  public SpiceEnvironmentBuilder bindFrameID(String name, FrameID id) {
    frameBindings.add(name, id);
    return this;
  }

  /**
   * Bind a set of SPICE frame names with their paired FrameID
   * <p>
   * Note: this method inserts bindings into a prioritized list of binding requests in the iteration
   * order of the key set supplied by the map. This is only really a cautionary message if you
   * provide different string names that map to the same SPICE frame. The last entry in the key set
   * will take precedence.
   * </p>
   * 
   * @param bindings a map of SPICE frame names to the desired FrameID
   * 
   * @return a reference to the builder to support method chaining
   */
  public SpiceEnvironmentBuilder bindAllFrameID(Map<String, ? extends FrameID> bindings) {
    frameBindings.addAll(bindings);
    return this;
  }

  /**
   * Remove the binding for the supplied SPICE frame name. Note: this operation is a no-op if the
   * code has not already been bound to an FrameID.
   * 
   * @param name the SPICE frame name to remove a binding for
   * 
   * @return a reference to the builder to support method chaining
   */
  public SpiceEnvironmentBuilder unbindFrameID(String name) {
    frameBindings.remove(name);
    return this;
  }

  /**
   * Remove a collection of SPICE frame name bindings. Note: any names specified to be removed that
   * have not already been bound will simply be ignored.
   * 
   * @param codes an iterable of SPICE frame names to unbind
   * 
   * @return a reference to the builder to support method chaining
   */
  public SpiceEnvironmentBuilder unbindAllFrameID(Iterable<String> names) {
    frameBindings.removeAll(names);
    return this;
  }

  /**
   * Unload the last data source associated with an identifier
   * 
   * @param identifier the identifier associated with the data to unload
   * 
   * @return a reference to the builder to support method chaining
   */
  public SpiceEnvironmentBuilder unload(String identifier) {
    manager.remove(identifier);
    return this;
  }

  /**
   * Load the data content captured by an input stream, associating it with the supplied identifier.
   * <p>
   * Note: Using this method, rather than the file based loader may result in the entire contents of
   * stream being read directly into memory and retained for the duration of the object and objects
   * derived from its usage. This is necessary, due to the random access nature of some SPICE based
   * data formats.
   * </p>
   * 
   * @param identifier the identifier to associate with the stream
   * @param supplier the stream capturing data content
   * 
   * @return a reference to the builder to support method chaining
   * 
   * @throws IOException if reading from the stream fails unexpectedly
   * @throws KernelInstantiationException if the data content of the stream is inappropriate for the
   *         supported SPICE kernels
   */
  public SpiceEnvironmentBuilder load(String identifier, ByteSource source)
      throws IOException, KernelInstantiationException {
    manager.add(identifier, loader.load(identifier, source));
    return this;
  }

  /**
   * Load the data content captured by an input stream, associating it with the supplied identifier.
   * <p>
   * This method is identical to {@link SpiceEnvironmentBuilder#load(String, InputStream)}, except
   * it loads kernels in a forgiving manner; that is, if a particular segment's type is not
   * supported by the version of the library, it is ignored. This is extremely dangerous, as it may
   * result in substantial deviations from SPICE's behavior when loading the same kernel set.
   * Exercise caution when using this method, and make certain you are not ignoring segments that
   * are significant.
   * </p>
   * <p>
   * Note: Using this method, rather than the file based loader may result in the entire contents of
   * stream being read directly into memory and retained for the duration of the object and objects
   * derived from its usage. This is necessary, due to the random access nature of some SPICE based
   * data formats.
   * </p>
   * 
   * @param identifier the identifier to associate with the stream
   * @param supplier the stream capturing data content
   * 
   * @return a reference to the builder to support method chaining
   * 
   * @throws IOException if reading from the stream fails unexpectedly
   * @throws KernelInstantiationException if the data content of the stream is inappropriate for the
   *         supported SPICE kernels
   */
  public SpiceEnvironmentBuilder forgivingLoad(String identifier, ByteSource source)
      throws IOException, KernelInstantiationException {
    manager.add(identifier, forgivingLoader.load(identifier, source));
    return this;
  }

  /**
   * Load the data content captured in a file, associating it with the supplied identifier.
   * 
   * @param identifier the identifier to associate with the file, typically the name of the file.
   * @param file the file capturing the data content to load
   * 
   * @return a reference to the builder to support method chaining
   * 
   * @throws KernelInstantiationException if the data contents within the file is inappropriate for
   *         the supported SPICE kernels
   * @throws FileNotFoundException if the file is not available on the system
   * @throws IOException if reading from the file fails for any reason
   */
  public SpiceEnvironmentBuilder load(String identifier, File file)
      throws KernelInstantiationException, FileNotFoundException, IOException {
    manager.add(identifier, loader.load(file));
    return this;
  }

  /**
   * Load the data content captured in a file, associating it with the supplied identifier.
   * <p>
   * This method is identical to {@link SpiceEnvironmentBuilder#load(String, InputStream)}, except
   * it loads kernels in a forgiving manner; that is, if a particular segment's type is not
   * supported by the version of the library, it is ignored. This is extremely dangerous, as it may
   * result in substantial deviations from SPICE's behavior when loading the same kernel set.
   * Exercise caution when using this method, and make certain you are not ignoring segments that
   * are significant.
   * </p>
   * 
   * @param identifier the identifier to associate with the file, typically the name of the file.
   * @param file the file capturing the data content to load
   * 
   * @return a reference to the builder to support method chaining
   * 
   * @throws KernelInstantiationException if the data contents within the file is inappropriate for
   *         the supported SPICE kernels
   * @throws FileNotFoundException if the file is not available on the system
   * @throws IOException if reading from the file fails for any reason
   */
  public SpiceEnvironmentBuilder forgivingLoad(String identifier, File file)
      throws KernelInstantiationException, FileNotFoundException, IOException {
    manager.add(identifier, forgivingLoader.load(file));
    return this;
  }

  /**
   * Builds a new <code>SpiceEnvironment</code> from the current adapter configuration.
   * 
   * @return a newly created environment from which data sources can be adapted from SPICE.
   * 
   * @throws BuildFailedException if any of the necessary kernel instantiations fail.
   * 
   */
  @Override
  public SpiceEnvironment build() throws BuildFailedException {

    /*
     * Ok, this is basically a total and absolute mess, because it requires pulling all the little
     * individual classes together to create this large ensemble of adapters.
     * 
     * A couple of assumptions that are to be made here: first, a leapseconds kernel has been
     * loaded. Granted, one could get quite far with regards to the ephemeris system without one,
     * but it is easy enough to instantiate the ephemeris source factory yourself. This object is
     * the 10k lbs gorilla.
     * 
     * Start the entire process by first assembling the kernel pool that is to contain all of the
     * text kernel content.
     */
    BasicKernelPool pool = new BasicKernelPool();
    for (TextKernel kernel : manager.getTextKernels()) {
      pool.load(kernel.getPool());
    }

    try {

      UniformTimeProvider tdtProvider = getTdtProvider(pool);

      /*
       * Next extract the encoded SCLK content from the kernel pool.
       */
      Map<Integer, SCLKKernel> sclkMap = new SCLKFactory(tdtProvider).createConverters(pool);

      /*
       * Create the ephemeris ID code bindings.
       */
      picante.spice.EphemerisIDBindingManager.BindingResult result = ephemerisBindings
          .createEphemerisIDMap(new KernelPoolContentFactory().createEphemerisIDMap(pool));

      /*
       * Now build the frame kernel to derive content from it.
       */
      FKFactory factory = new FKFactory();
      FrameKernel fk = factory.create(pool, result.getEphemerisCodeMap(), frameNames.getMap());
      Map<Integer, Integer> ckSclkIdMap = fk.getSCLKMap();

      /*
       * Add in the user specific content to the various maps. This is not supported at the moment,
       * but I left the comment here to remind us of the intentional enhancement.
       */
      // sclkMap.putAll(userSclkMap);

      /*
       * Then create the sclk registry that will be used to determine the appropriate C-kernel to
       * SCLK implementation binding. We do not support user supplied encoded SCLK converters at the
       * moment, so simply pass in an empty hash map.
       */

      SclkRegistry sclkRegistry =
          new SclkRegistry(sclkMap, ckSclkIdMap, new HashMap<Integer, EncodedSCLKConverter>());

      BindingResult resultantFrameBindings =
          frameBindings.createFrameIDMap(fk.getDefinedFrames(), fk.getNameBindings());

      /*
       * Create the frame center map from the binding results for the frame and ephemeris codes.
       * Only include an entry in the map if it has a corresponding ephemeris ID.
       */
      ImmutableMap<FrameID, EphemerisID> frameCenterMap =
          createFrameCenterMap(resultantFrameBindings.getFrameCenterMap(), result.getMap());

      /*
       * And lastly the appropriate factories for the environment.
       */
      EphemerisSourceFactory ephemerisFactory =
          new EphemerisSourceFactory(result.getMap(), resultantFrameBindings.getFrameCodeMap());
      FrameSourceFactory frameFactory =
          new FrameSourceFactory(resultantFrameBindings.getFrameCodeMap(),
              resultantFrameBindings.getClassCodeMapForType(FrameType.CK),
              resultantFrameBindings.getClassCodeMapForType(FrameType.PCK),
              resultantFrameBindings.getClassCodeMapForType(FrameType.TK), sclkRegistry);

      PCKFactory textPCKFactory = new PCKFactory();
      TextPCK textPCK = textPCKFactory.createTextPCK(pool);

      /*
       * Populate the body radii
       */
      ImmutableListMultimap.Builder<EphemerisID, Double> bodyRadiiBuilder =
          ImmutableListMultimap.builder();
      ListMultimap<Integer, Double> radii = textPCK.getRadii();
      for (Integer i : radii.keySet()) {
        if (result.getMap().containsKey(i)) {
          bodyRadiiBuilder.putAll(result.getMap().get(i), radii.get(i));
        } else {
          bodyRadiiBuilder.putAll(new SpiceEphemerisID(i), radii.get(i));
        }
      }

      LSK lsk = new LSKFactory().createLSK(pool);
      /*
       * Note: the ephemerisFactory and frameFactory classes will ultimately maintain two separate
       * pools of frame bindings. This should be OK though, as they are instantiating individual
       * frame ID codes that should be equal.
       */
      return new SpiceEnvironment(ephemerisFactory, frameFactory, textPCK, fk, pool,
          manager.getCks(), manager.getSpks(), manager.getPCKs(), sclkMap, lsk, frameCenterMap,
          bodyRadiiBuilder.build(), ignoreFaultyFrames);

    } catch (KernelInstantiationException e) {
      throw new BuildFailedException("Problem instantiating SPICE kernel", e);
    }
  }

  ImmutableMap<FrameID, EphemerisID> createFrameCenterMap(Map<FrameID, Integer> frameCenterMap,
      Map<Integer, EphemerisID> ephemerisIDMap) {

    ImmutableMap.Builder<FrameID, EphemerisID> builder = ImmutableMap.builder();

    for (FrameID frame : frameCenterMap.keySet()) {

      /*
       * Only include the center if it has a corresponding ephemerisID binding.
       */
      Integer centerEphemerisCode = frameCenterMap.get(frame);
      if (ephemerisIDMap.containsKey(frameCenterMap.get(frame))) {
        builder.put(frame, ephemerisIDMap.get(centerEphemerisCode));
      }
    }

    return builder.build();

  }

  /**
   * Returns the TDT provider configured for usage with this environment builder. If the user
   * supplied one, then the internal field will be initialized. If they neglected to supply one,
   * then it will remain uninitialized (null) and the appropriate provider should be derived from
   * the leapseconds content in the loaded kernel pool.
   * 
   * @return the TDT provider to utilize in the derived <code>SpiceEnvironment</code>.
   * 
   * @throws LSKInstantiationException if the user did not supply a <code>UniformTimeProvider</code>
   *         for TDT conversions and there was a problem instantiating one from the leapseconds
   *         kernel content in the kernel pool.
   */
  UniformTimeProvider getTdtProvider(BasicKernelPool pool) throws LSKInstantiationException {

    if (userTdtProvider != null) {
      return userTdtProvider;
    }

    return new LSKFactory().createLSK(pool).getTDTProvider();

  }

}
