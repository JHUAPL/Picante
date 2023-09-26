package picante.spice.kernelpool.content;

import com.google.common.collect.ImmutableMap;
import picante.mechanics.rotations.EulerAngles;

/**
 * Class used to unravel the integer code specifications from the kernel pool into the appropriate
 * instance of EulerAngles. It should be thread-safe, so just retrieve an instance of it from
 * {@link EulerAnglesFactory#INSTANCE}.
 */
class EulerAnglesFactory {

  /**
   * Instance of the factory that should be safe for access from multiple threads.
   */
  static final EulerAnglesFactory INSTANCE = new EulerAnglesFactory();

  /**
   * Map used to hold the individual creator instances.
   */
  private final ImmutableMap<Integer, Creator> factories;

  /**
   * Constructs the factory. It's marked package private so users prefer the static reference.
   */
  private EulerAnglesFactory() {

    ImmutableMap.Builder<Integer, Creator> builder = ImmutableMap.builder();
    builder.put(121, new CreatorIJI());
    builder.put(131, new CreatorIKI());
    builder.put(123, new CreatorIJK());
    builder.put(132, new CreatorIKJ());
    builder.put(212, new CreatorJIJ());
    builder.put(232, new CreatorJKJ());
    builder.put(213, new CreatorJIK());
    builder.put(231, new CreatorJKI());
    builder.put(313, new CreatorKIK());
    builder.put(323, new CreatorKJK());
    builder.put(312, new CreatorKIJ());
    builder.put(321, new CreatorKJI());
    factories = builder.build();
  }

  /**
   * Creates an instance of EulerAngles with the supplied axes.
   * <p>
   * Note: as this is a package private helper class, no validation is performed on the arguments,
   * as it is assumed to be handled elsewhere. You will likely get something unexpected if you feed
   * values that lie outside the {1,2,3} set.
   * </p>
   * 
   * @param leftAxis
   * @param centerAxis
   * @param rightAxis
   * 
   * @return
   * 
   * @throws KernelPoolValidationException
   */
  EulerAngles createAngles(String keyword, int leftAxis, int centerAxis, int rightAxis)
      throws KernelPoolValidationException {

    int query = leftAxis * 100 + centerAxis * 10 + rightAxis;

    /*
     * Start by determining if the first and second or second and third axes specifications are the
     * same. While NAIF allows this, the EulerAngles classes won't support it due to their need to
     * convert both ways between rotations.
     */

    if (!factories.containsKey(query)) {
      throw new KernelPoolValidationException(keyword,
          "Supplied axes arguments must map properly to an Euler angle decomposition: " + leftAxis
              + " " + centerAxis + " " + rightAxis + " unfortunately do not.");
    }

    return factories.get(query).create();
  }

  interface Creator {
    EulerAngles create();
  }

  private static class CreatorIJI implements Creator {
    @Override
    public EulerAngles create() {
      return new EulerAngles.IJI();
    }
  }

  private static class CreatorIJK implements Creator {

    @Override
    public EulerAngles create() {
      return new EulerAngles.IJK();
    }

  }

  private static class CreatorIKI implements Creator {
    @Override
    public EulerAngles create() {
      return new EulerAngles.IKI();
    }
  }

  private static class CreatorIKJ implements Creator {
    @Override
    public EulerAngles create() {
      return new EulerAngles.IKJ();
    }
  }

  private static class CreatorJIJ implements Creator {
    @Override
    public EulerAngles create() {
      return new EulerAngles.JIJ();
    }
  }

  private static class CreatorJIK implements Creator {
    @Override
    public EulerAngles create() {
      return new EulerAngles.JIK();
    }
  }

  private static class CreatorJKI implements Creator {
    @Override
    public EulerAngles create() {
      return new EulerAngles.JKI();
    }
  }

  private static class CreatorJKJ implements Creator {
    @Override
    public EulerAngles create() {
      return new EulerAngles.JKJ();
    }
  }

  private static class CreatorKIK implements Creator {
    @Override
    public EulerAngles create() {
      return new EulerAngles.KIK();
    }
  }

  private static class CreatorKJK implements Creator {
    @Override
    public EulerAngles create() {
      return new EulerAngles.KJK();
    }
  }

  private static class CreatorKIJ implements Creator {
    @Override
    public EulerAngles create() {
      return new EulerAngles.KIJ();
    }
  }

  private static class CreatorKJI implements Creator {
    @Override
    public EulerAngles create() {
      return new EulerAngles.KJI();
    }
  }

}
