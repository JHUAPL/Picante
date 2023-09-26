package picante.math.functions;

import java.util.concurrent.atomic.AtomicInteger;

import com.google.common.base.Preconditions;
import picante.exceptions.RuntimeInterruptedException;

/**
 * Class that attempts to abstract the code required to check a thread for interrupt status and
 * generate an appropriate runtime exception indicating that has occurred.
 * <p>
 * This class is probably headed for {@link picante.concurrent}, but not until it's been run
 * through the ringer a bit first.
 * </p>
 */
abstract class RuntimeInterruptionChecker {

  /**
   * Basic check that simply executes every time the check method is called.
   */
  private static class BasicChecker extends RuntimeInterruptionChecker {

    @Override
    void processRuntimeInterruptCheckAndThrow() {
      if (Thread.interrupted()) {
        throw new RuntimeInterruptedException();
      }
    }

  }

  /**
   * Creates a checker that executes every time the check method is called.
   * 
   * @return
   */
  static RuntimeInterruptionChecker create() {
    return new BasicChecker();
  }

  /**
   * Creates a checker that executes every &quot;nth&quot; evaluation.
   */
  private static class ModuloChecker extends BasicChecker {

    private final AtomicInteger counter;
    private final int evaluationCheckModulo;

    ModuloChecker(int evaluationCheckModulo) {
      Preconditions.checkArgument(evaluationCheckModulo > 0,
          "Evaluation check modulo must be strictly positive, was %s", evaluationCheckModulo);
      this.counter = new AtomicInteger(evaluationCheckModulo);
      this.evaluationCheckModulo = evaluationCheckModulo;
    }

    @Override
    void processRuntimeInterruptCheckAndThrow() {
      if (counter.decrementAndGet() == 0) {
        counter.set(evaluationCheckModulo);
        super.processRuntimeInterruptCheckAndThrow();
      }

    }

  }

  /**
   * Creates a checker that executes every &quot;evaluationCheckModulo-th&quot; time the method is
   * called.
   * 
   * @param evaluationCheckModulo
   * @return
   */
  static RuntimeInterruptionChecker createWithCheckModulo(int evaluationCheckModulo) {
    if (evaluationCheckModulo == 0) {
      return new BasicChecker();
    }
    return new ModuloChecker(evaluationCheckModulo);
  }

  /**
   * @throws RuntimeInterruptedException if the check is to execute according to the evaluation
   *         conditions, and the thread is in an interrupted state. The thread interrupt is cleared
   *         by this method.
   */
  abstract void processRuntimeInterruptCheckAndThrow();

}
