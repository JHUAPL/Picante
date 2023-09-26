package picante.demo;

import static org.junit.Assert.*;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.CelestialBodies;
import picante.mechanics.CelestialFrames;
import picante.mechanics.Coverage;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;
import picante.mechanics.StateVector;
import picante.mechanics.StateVectorFunction;
import picante.mechanics.providers.aberrated.AberratedEphemerisProvider;
import picante.mechanics.providers.aberrated.AberrationCorrection;
import picante.spice.MetakernelReader;
import picante.spice.SpiceEnvironment;
import picante.spice.SpiceEnvironmentBuilder;
import picante.spice.kernel.KernelInstantiationException;
import picante.time.TimeConversion;

/**
 * Example for setting up a multithreaded spice calculation.
 *
 * @author Hari.Nair@jhuapl.edu
 */
public class MultiThreaded implements Callable<Map<Double, StateVector>> {

  private final String metakernel;
  private final EphemerisID target;
  private final EphemerisID observer;
  private final FrameID frameID;
  private final int numPts;
  private final double startTime;
  private final double delta;
  private final ThreadLocal<SpiceEnvironment> spiceEnv;

  public MultiThreaded(
      String metakernel,
      EphemerisID target,
      EphemerisID observer,
      FrameID frameID,
      int numPts,
      double startTime,
      double delta) {
    this.metakernel = metakernel;
    this.target = target;
    this.observer = observer;
    this.frameID = frameID;
    this.numPts = numPts;
    this.startTime = startTime;
    this.delta = delta;
    this.spiceEnv = new ThreadLocal<>();
  }

  public static void main(String[] args) {

    EphemerisID target = CelestialBodies.MARS;
    EphemerisID observer = CelestialBodies.EARTH;
    FrameID frameID = CelestialFrames.J2000;
    String epoch = "2003 Jul 4 19:00:00";

    String metakernel = "src/test/resources/kernels/mk/multiThreaded.tm";
    SpiceEnvironment env = getSpiceEnvironment(metakernel);

    TimeConversion tc = new TimeConversion(env.getLSK());
    double startTime = tc.utcStringToTDB(epoch);

    int numPts = 2000000;
    double delta = 1.5;

    // single thread
    MultiThreaded app =
        new MultiThreaded(metakernel, target, observer, frameID, numPts, startTime, delta);

    long startMillis = System.currentTimeMillis();
    Map<Double, StateVector> oneThread = new HashMap<>();
    try {
      oneThread = app.call();
    } catch (Exception e) {
      e.printStackTrace();
      System.exit(1);
    }

    System.out.printf(
        "Single thread time: %d entries in %.3f seconds\n",
        oneThread.size(), 1e-3 * (System.currentTimeMillis() - startMillis));

    startMillis = System.currentTimeMillis();

    int numThreads = 5;
    ExecutorService executor = Executors.newFixedThreadPool(numThreads);
    Set<Future<Map<Double, StateVector>>> futures = new HashSet<>();

    for (int i = 0; i < numThreads; i++) {
      int thisNumPts = numPts / numThreads;
      double thisStartTime = startTime + i * thisNumPts * delta;
      app =
          new MultiThreaded(
              metakernel, target, observer, frameID, thisNumPts, thisStartTime, delta);
      futures.add(executor.submit(app));
    }

    Map<Double, StateVector> allThreads = new HashMap<>();
    for (Future<Map<Double, StateVector>> future : futures) {
      try {
        allThreads.putAll(future.get());
      } catch (Exception e) {
        e.printStackTrace();
      }
    }

    executor.shutdown();

    System.out.printf(
        "%d thread time: %d entries in %.3f seconds\n",
        numThreads, allThreads.size(), 1e-3 * (System.currentTimeMillis() - startMillis));

    System.out.print("Comparing multiple thread calculations with single thread... ");
    double tolerance = 0;
    for (double key : oneThread.keySet()) {
      StateVector difference = StateVector.subtract(oneThread.get(key), allThreads.get(key));
      VectorIJK pos = difference.getPosition();
      VectorIJK vel = difference.getVelocity();

      assertFalse(pos.getLength() > tolerance);
      assertFalse(vel.getLength() > tolerance);

      if (pos.getLength() > tolerance || vel.getLength() > tolerance) {
        System.out.printf(
            "%s: states %s %s\n",
            tc.tdbToUTCString(key, "C"), oneThread.get(key), allThreads.get(key));
      }
    }

    System.out.println("Finished");
  }

  /**
   * @param metakernel metakernel to read
   * @return SpiceEnvironment
   */
  private static SpiceEnvironment getSpiceEnvironment(String metakernel) {
    MetakernelReader reader = new MetakernelReader(metakernel);
    SpiceEnvironment env = null;
    try {
      SpiceEnvironmentBuilder builder = new SpiceEnvironmentBuilder();
      for (File kernel : reader.getKernelsToLoad()) builder.load(kernel.getName(), kernel);
      env = builder.build();
    } catch (KernelInstantiationException | IOException e) {
      e.printStackTrace();
      System.exit(1);
    }
    return env;
  }

  /**
   * @return A SpiceEnvironment unique to this thread
   */
  private SpiceEnvironment getSpiceEnvironment() {
    if (spiceEnv.get() == null) {
      spiceEnv.set(getSpiceEnvironment(metakernel));
    }
    return spiceEnv.get();
  }

  @Override
  public Map<Double, StateVector> call() throws Exception {

    // create a new SpiceEnvironment only used by this thread
    SpiceEnvironment env = getSpiceEnvironment();

    AberratedEphemerisProvider provider = env.createSingleAberratedProvider();
    StateVectorFunction stateVectorFunction =
        provider.createAberratedStateVectorFunction(
            target, observer, frameID, Coverage.ALL_TIME, AberrationCorrection.LT_S);

    Map<Double, StateVector> map = new HashMap<>();
    for (int i = 0; i < numPts; i++) {
      double tdb = startTime + i * delta;
      StateVector state = stateVectorFunction.getState(tdb);
      map.put(tdb, state);
    }

    return map;
  }
}
