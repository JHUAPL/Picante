/**
 * Package containing the generic crucible one dimensional, record based data model.
 * <p>
 * The primary &quot;record&quot; storage interface is:
 * <dl>
 * <dt>{@link Retrievable}</dt>
 * <dd>Interfaces that use an inversion of control memory management pattern, much like the
 * {@link picante.designpatterns.Writable} design pattern. The caller supplies the memory that
 * the implementation then mutates to capture the result.</dd>
 * </dl>
 * </p>
 * <p>
 * The next concept that is present in these packages is that of the &quot;gauge&quot;. We struggled
 * to come up with unique terminology that captures the essence of what we are trying to express
 * here. The basic idea is that if you wish to interpolate between &quot;records&quot; then you
 * likely need these records registered against some sort of domain or number line. This
 * &quot;domain&quot; axis is the gauge, and it is an ordered association of records with a double
 * precision number. Thus the gauge locates records on the table directly to a position on this
 * domain number line or axis. Frequently the gauge is some measure of time, but the API makes no
 * such restriction. Implementations of these gauged interfaces are expected to provide the records
 * in an index order that is non-decreasing against the gauge.
 * </p>
 * <p>
 * Lastly, because at times if the implementations of these interfaces are backed by databases,
 * files, or other means, there are a set of additional extensions that add methods to perform
 * searches. Allowing implementors to provide search mechanisms that are most efficient for their
 * respective data stores or implementations seemed appropriate. In most cases, binary search or
 * direct index computation is sufficient. Abstract helper classes that provide commonly used
 * searching algorithms are available in an additional sub-package.
 * </p>
 */
package picante.data.list;

