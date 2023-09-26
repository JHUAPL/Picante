package picante.spice;

import static org.junit.Assert.assertEquals;
import java.util.LinkedList;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import picante.spice.kernel.ck.CK;
import picante.spice.kernel.spk.SPK;
import picante.spice.kernel.tk.TextKernel;
import picante.spice.kernelpool.BasicKernelPool;

public class KernelManagerTest {

  private List<SPK> spks;
  private List<CK> cks;
  private List<TextKernel> tks;
  private List<String> identifiers;

  private KernelManager manager;

  @Before
  public void setUp() throws Exception {

    spks = new LinkedList<SPK>();
    cks = new LinkedList<CK>();
    tks = new LinkedList<TextKernel>();

    spks.add(new TestSpk("SPKA"));
    spks.add(new TestSpk("SPKB"));
    spks.add(new TestSpk("SPKC"));

    cks.add(new TestCk("CKA"));
    cks.add(new TestCk("CKB"));
    cks.add(new TestCk("CKC"));

    tks.add(new TestTk("TKA"));
    tks.add(new TestTk("TKB"));
    tks.add(new TestTk("TKC"));

    identifiers = new LinkedList<String>();
    manager = new KernelManager();

    manager.add("SPKA", spks.get(0));
    identifiers.add("SPKA");
    manager.add("CKA", cks.get(0));
    identifiers.add("CKA");
    manager.add("SPKB", spks.get(1));
    identifiers.add("SPKB");
    manager.add("TKA", tks.get(0));
    identifiers.add("TKA");
    manager.add("TKB", tks.get(1));
    identifiers.add("TKB");
    manager.add("CKB", cks.get(1));
    identifiers.add("CKB");
    manager.add("SPKC", spks.get(2));
    identifiers.add("SPKC");
    manager.add("CKC", cks.get(2));
    identifiers.add("CKC");
    manager.add("TKC", tks.get(2));
    identifiers.add("TKC");
  }

  @Test
  public void testAddSPK() {

    TestSpk spkd = new TestSpk("SPKD");
    manager.add("SPKD", spkd);

    spks.add(spkd);

    assertEquals(spks, manager.getSpks());
  }

  @Test
  public void testAddCK() {

    TestCk ckd = new TestCk("CKD");
    manager.add("CKD", ckd);

    cks.add(ckd);

    assertEquals(cks, manager.getCks());
  }

  @Test
  public void testAddTextKernel() {

    TestTk tkd = new TestTk("TKD");
    manager.add("TKD", tkd);

    tks.add(tkd);
    assertEquals(tks, manager.getTextKernels());

  }

  @Test
  public void testRemove() {

    manager.remove("SPKB");
    spks.remove(1);

    assertEquals(spks, manager.getSpks());

  }

  @Test
  public void testRemoveDuplicateIdentifier() {

    TestSpk spkdup = new TestSpk("SPKD");
    manager.add("SPKA", spkdup);
    manager.remove("SPKA");

    assertEquals(spks, manager.getSpks());

    manager.remove("SPKA");
    spks.remove(0);

    assertEquals(spks, manager.getSpks());
  }

  @Test
  public void testGetSpks() {
    assertEquals(spks, manager.getSpks());
  }

  @Test
  public void testGetCks() {
    assertEquals(cks, manager.getCks());
  }

  @Test
  public void testGetTextKernels() {
    assertEquals(tks, manager.getTextKernels());
  }

  @Test
  public void testGetIdentifiers() {
    assertEquals(identifiers, manager.getIdentifiers());
  }

  @Test
  public void testGetIdentifiersDuplicateEntry() {
    manager.add("SPKA", tks.get(0));
    identifiers.add("SPKA");
    assertEquals(identifiers, manager.getIdentifiers());
  }

  class TestSpk extends SPK {
    public TestSpk(String name) {
      super(name, null);
    }
  }

  class TestCk extends CK {
    public TestCk(String name) {
      super(name, null);
    }
  }

  class TestTk extends TextKernel {
    public TestTk(String name) {
      super(name, new LinkedList<String>(), new BasicKernelPool());
    }
  }

}
