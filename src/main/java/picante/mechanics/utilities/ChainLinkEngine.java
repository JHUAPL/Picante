package picante.mechanics.utilities;

import static com.google.common.base.Preconditions.checkNotNull;
import java.util.List;
import javax.annotation.Nullable;
import com.google.common.collect.ImmutableListMultimap;
import com.google.common.collect.Lists;

/**
 * Class that implements the chain linkage algorithm utilized by implementations of the ephemeris
 * and frame provider interfaces. The class is parameterized to allow it to function with any sort
 * of time varying tree structure with load priority conflict resolution.
 * 
 * In a nut shell, this class takes a list of functions that connect &quot;leaf&quot; nodes to
 * &quot;node&quot; nodes and provides an interface to connect them. Conflicts in definitions, i.e.
 * two functions provide a definition for the same leaf node are resolved through load order. Namely
 * the last applicable function provided at construction time is the first applicable.
 * <p>
 * <b>Note:</b>This implementation of the link engine places a few constraints on the function list
 * provided to the constructor which are not enforced or checked for in the implementation. In the
 * most general case, at any requested time, there may be no functions that close nodes in the tree
 * into a cycle. I.e.:
 * 
 * <pre>
 * <code>
 * A -&gt; B -&gt; C -&gt; D -&gt; Ap
 * &circ;                   |
 * +-------------------+
 * </code>
 * </pre>
 * 
 * This can happen if Ap directly connects to A, or if the IDs for A and Ap are such that
 * A.equals(Ap), or Ap.equals(A). In the event this happens, then the response of the code is
 * unspecified. At worst, it may enter into an infinite loop evaluating links in the chain.
 * </p>
 * <p>
 * TODO: It may be worth revisiting the public API on this class, to see if its usability could be
 * improved. It was only ever designed with the intention of being an internal implementation detail
 * for various provider implementations.
 * </p>
 * 
 * @param <I> Codes used to define nodes in the tree
 * @param <F> Functions used to connect nodes in the tree
 */
public class ChainLinkEngine<I, F> {

  /**
   * Map that connects the leaf node ID codes of the supplied functions to a prioritized list of
   * functions with that leaf node code. Functions at the top of the list have higher priority in
   * this implementation.
   */
  private final ImmutableListMultimap<I, F> elements;

  /**
   * A reference to the code provider supplied at construction time. This is used to extract leaf
   * and node ID codes from the supplied functions as well as determining their validity at a
   * particular time.
   */
  private final CodeProvider<I, ? super F> provider;

  /**
   * A reference to the function to be inserted into the output buffer of
   * {@link ChainLinkEngine#populateLinkage(Object, Object, double, List)} to indicate the
   * separation between the forward linking chain and the backward linking one.
   */
  private final F separator;

  /**
   * A reference to the function to be inserted into the output buffer of
   * {@link ChainLinkEngine#populateLinkage(Object, Object, double, List)} to indicate a disconnect
   * between the forward linking chain and the backward linking one.
   */
  private final F brokenLink;

  /**
   * Constructs an instance of the linking engine.
   * 
   * @param functions a list of functions to load into the engine for linking. The function with the
   *        lowest index has the lowest priority. At no time may a function in this list close a
   *        path through the graph enumerating the tree. See the class description for details.
   * 
   * @param provider an implementation of the {@link CodeProvider} interface that allows extraction
   *        of the various pieces of information necessary to perform the linkage
   * 
   * @param separator a function that will be inserted into the chain indicating the point where the
   *        forward linking chain meets the backward linking one
   * 
   * @param brokenLink a function that will be inserted into the chain indicating the point where
   *        the forward linking chain and the backward linking chain disconnect
   * 
   * @throws NullPointerException if functions or provider are null. While discouraged, the
   *         separator and brokenLink arguments are permitted to be null.
   */
  public ChainLinkEngine(List<? extends F> functions, CodeProvider<I, ? super F> provider,
      @Nullable F separator, @Nullable F brokenLink) {
    this.provider = checkNotNull(provider);
    this.elements = createMultimap(checkNotNull(functions));
    this.separator = separator;
    this.brokenLink = brokenLink;
  }

  /**
   * &quot;Append&quot; a list of functions to the element map.
   * 
   * @param functions a list of functions to load, lowest index has lowest selection priority
   */
  private ImmutableListMultimap<I, F> createMultimap(List<? extends F> functions) {

    ImmutableListMultimap.Builder<I, F> builder = ImmutableListMultimap.builder();

    /*
     * The list of functions is in lowest priority to highest. We want the map to be in the reverse
     * order. Reverse the list before iteration:
     */
    for (F function : Lists.reverse(functions)) {
      builder.put(provider.getLeafCode(function), function);
    }

    return builder.build();
  }


  /**
   * Returns the &quot;marker&quot; function used to identify the separation point between the
   * forward linking chain and the backward linking chain in a successfully populated linkage.
   * 
   * @return the separator function
   */
  public F getSeparator() {
    return separator;
  }

  /**
   * Returns the &quot;marker&quot; function used to identify where the forward linking chain and
   * backward linking chain disconnect
   * 
   * @return the broken link function
   */
  public F getBrokenLink() {
    return brokenLink;
  }

  /**
   * Compute the chain linking the supplied leaf ID code to the supplied node ID code. The resultant
   * chain will contain only the base functions provided to this instance, in the order necessary to
   * move from the leaf ID code to the node ID code.
   * <p>
   * <b>Note:</b> This implementation performs no checks if the user supplies two equal codes (i.e.
   * leaf.equals(node) or node.equals(leaf)) into this method. Checks of this nature should be
   * managed by the caller of this method, and the results of such a call are indeterminate.
   * Further, the implementation requires that any linkage between nodes at a given time do not
   * result in any repeat of a code. Namely:
   * 
   * <pre>
   * <code>
   * A -&gt; B -&gt; C -&gt; Ap
   * </code>
   * </pre>
   * 
   * where A.equals(Ap)
   * </p>
   * 
   * @param leaf the ID code of the leaf node from which to start the chain
   * 
   * @param node the ID code of the node node where the chain ends
   * 
   * @param time the &quot;time&quot; of interest, which is used to determine the desired function
   * 
   * @param buffer a list of functions that upon completion contains the linkage from leaf to node.
   *        The contents of the list are cleared at the start of execution of this method. In
   *        addition to the base functions, two separation functions are inserted into the chain in
   *        exceptional cases. If the link requires a forward chain and a backward chain, the
   *        separator function is inserted into the lists between these two chains. If there is no
   *        link, the broken link function is inserted between the forward and backward chain.
   * 
   * @return true if the linkage is properly determined, false otherwise
   */
  public boolean populateLinkage(I leaf, I node, double time, List<F> buffer) {

    /*
     * TODO: Figure out if checking for infinite loop is reasonable.
     */

    /*
     * Dump any existing contents of buffer.
     */
    buffer.clear();

    /*
     * Start the search by capturing all of the functions connected to the leaf node first. We may
     * over shoot the link connecting this to the node code chain, but the alternatives are tricky
     * to implement.
     */
    I next = leaf;
    F function = findFirstValid(next, time);

    /*
     * As long as we continue finding connected functions, keep adding them to the chain.
     */
    while (function != null) {

      buffer.add(function);
      next = provider.getNodeCode(function);

      /*
       * If we reach the desired node code, then we're done.
       */
      if (next.equals(node)) {
        return true;
      }

      function = findFirstValid(next, time);
    }

    /*
     * If we reach this point, then we have climbed as far up the tree from the supplied leaf node.
     * Now, start from the node side. First, capture the current index of the last forward function
     * as we will be storing the results of our reverse traversal in the same buffer.
     */
    int lastForwardFunctionIndex = buffer.size() - 1;
    int insertionIndex = buffer.size();

    next = node;
    int index;

    /*
     * Start traversing the chain from node up the tree. Each uncovered node requires us to examine
     * the entire forward node to see if there are any relative code connections that would link the
     * chain together.
     */
    do {

      function = findFirstValid(next, time);

      /*
       * If we are unable to find the desired function, then return.
       */
      if (function == null) {
        buffer.add(insertionIndex, brokenLink);
        return false;
      }

      /*
       * Otherwise add the new link into the chain, and continue up the tree.
       */
      buffer.add(insertionIndex, function);

      /*
       * Check to see if we are finished. If the new value of next matches leaf, then we are done.
       */
      next = provider.getNodeCode(function);

      if (next.equals(leaf)) {
        buffer.subList(0, lastForwardFunctionIndex + 1).clear();
        buffer.add(0, separator);
        return true;
      }

      index = locateConnection(next, buffer, lastForwardFunctionIndex);

    } while (index == -1);

    /*
     * If we reached here, then there is a connection between the forward chain and the backwards
     * one. Remove the unnecessary elements from the forward chain.
     */
    buffer.add(insertionIndex, separator);

    if (index <= lastForwardFunctionIndex) {
      buffer.subList(index, lastForwardFunctionIndex + 1).clear();
    }

    return true;
  }

  /**
   * Traverse the supplied list of functions up to the supplied lastFunctionsIndex, searching for a
   * connection between the specified ID.
   * 
   * @param id the ID for which a connection is sought
   * 
   * @param functions the list of functions to search
   * 
   * @param lastFunctionsIndex the index of the last function to examine from the head of the
   *        functions list for a connection
   * 
   * @return the index of the connecting function, lastFunctionsIndex+1 if the node code of the last
   *         function matches ID, or -1 if no such connection is found
   */
  private int locateConnection(I id, List<F> functions, int lastFunctionsIndex) {

    /*
     * Check to see if there are no forward functions in the list.
     */
    if (lastFunctionsIndex < 0) {
      return -1;
    }

    int i;
    F function = null;

    for (i = 0; i <= lastFunctionsIndex; i++) {

      function = functions.get(i);

      if (provider.getLeafCode(function).equals(id)) {
        return i;
      }

    }

    /*
     * If we reach here, then i is already one past lastFunctionsIndex. Check to see if the last
     * function's node code matches the ID we seek. Indicate the linkage by returning one larger
     * than the lastFunctionsIndex.
     */
    if (provider.getNodeCode(function).equals(id)) {
      return i;
    }

    return -1;

  }

  /**
   * Locate the highest priority function valid at the specified time from the elements map.
   * 
   * @param key the key of interest to locate in the elements map
   * @param time the time of interest
   * 
   * @return the highest priority function defining key at time, or null if no valid function is
   *         found
   */
  private F findFirstValid(I key, double time) {

    if (!elements.containsKey(key)) {
      return null;
    }

    List<F> list = elements.get(key);

    for (F function : list) {
      if (provider.validAt(function, time)) {
        return function;
      }
    }

    return null;

  }

  /**
   * A simple interface to abstract the retrieval of information that the chaining algorithm
   * requires from the functions
   * 
   * @param <I> ID codes which the supplied function connects
   * @param <F> Function to be chained
   */
  public interface CodeProvider<I, F> {

    public I getLeafCode(F function);

    public I getNodeCode(F function);

    public boolean validAt(F function, double time);

  }


}


