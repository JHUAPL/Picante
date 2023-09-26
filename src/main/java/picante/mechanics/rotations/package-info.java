/**
 * Contains interfaces defining rotations and the supported implementations supplied by the library.
 * <p>
 * Key interfaces are:
 * <ul>
 * <li>{@link picante.mechanics.rotations.UnwritableRotation}</li> Interface for retrieving
 * rotation content
 * <li>{@link picante.mechanics.rotations.Rotation}</li> Interface for applying rotation
 * content
 * <li>{@link picante.mechanics.rotations.UnwritableDifferentiatedRotation}</li> Interface for
 * retrieving rotation and derivative content
 * <li>{@link picante.mechanics.rotations.DifferentiatedRotation}</li> Interface for applying
 * rotation and derivative content
 * </ul>
 * </p>
 * <p>
 * Any standard crucible provided implementations of the rotation interface are located in this
 * package. The {@link picante.math.vectorspace.RotationMatrixIJK} is the key connector
 * between the rotation classes and any implementor of the interfaces in this package must provide
 * methods to move to and from this class or its unwritable parent.
 * </p>
 * <p>
 * Further, in much the same way, the {@link picante.mechanics.StateTransform} is the key
 * connector between the rotation and derivative classes and any implementors of the derivative
 * interfaces in this package. It is worth pointing out that the differentiable interfaces extend
 * from the {@link picante.mechanics.rotations.UnwritableRotation} interface, and require any
 * setting of the rotation and derivative to be applied at the same time. This was done to allow
 * implementors to maintain consistency between the rotation and derivative values, and not have to
 * concern themselves with processing a request to adjust the rotation without changing the
 * derivative value. Fortunately, the state transform class has methods that allow the separate
 * manipulation of the rotation and derivative fields. Utilize these if necessary.
 * </p>
 * <p>
 * Another point worth making, these classes often provide accessors that yield references to their
 * internal fields. This is done for developer's convenience, when configuring these classes without
 * additional memory on hand. However, in general, one should avoid using these values to retrieve
 * data to broadcast through layers of code. This opens the door to the usual problems with changing
 * the internals. For moving rotations around in software, it is better to pass the appropriate
 * interface implemented by these classes.
 * </p>
 */
package picante.mechanics.rotations;
