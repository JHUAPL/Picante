package picante.spice.daf.content.recordtables;

import static com.google.common.base.Preconditions.checkArgument;
import picante.data.list.AbstractFixedLengthGaugedRetrievableWithExceptions;
import picante.mechanics.StateVector;

/**
 * Implementation of the record table interface, backed by DAF, providing access to a table of state
 * vectors ordered in (x, y, z, dx, dy, dz).
 */
public class DAFStateVectorRecordTable
    extends AbstractFixedLengthGaugedRetrievableWithExceptions<StateVector> {

  /**
   * The record list used to provide records.
   */
  private final DAFStateVectorRecordList list;

  /**
   * The time table used to provide times associated with each record.
   */
  private final DAFTimeListTable table;

  public DAFStateVectorRecordTable(DAFStateVectorRecordList list, DAFTimeListTable table) {
    super(table.size());
    checkArgument(list.size() == table.size(),
        "Attempt to construct a DAFStateVectorRecordTable from the "
            + "DAFStateVectorRecordList and the DAFTimeListTable has "
            + "failed since they are not the same size.");
    this.list = list;
    this.table = table;
  }

  @Override
  protected StateVector obtainRecord(int index, StateVector buffer) {
    return list.get(index, buffer);
  }

  @Override
  protected double obtainTime(int index) {
    return table.obtainTime(index);
  }
}
