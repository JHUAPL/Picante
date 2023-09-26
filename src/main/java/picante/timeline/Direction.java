package picante.timeline;

/**
 * Enumeration indicating which direction to &quot;look&quot; when querying the time of a state
 * transition.
 */
public enum Direction {

  /**
   * Requests the state just prior to the transition time.
   */
  PREVIOUS,

  /**
   * Requests the state just after the transition time.
   */
  NEXT;
}
