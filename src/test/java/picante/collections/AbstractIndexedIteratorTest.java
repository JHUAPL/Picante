package picante.collections;

import static org.easymock.EasyMock.createMockBuilder;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertEquals;
import java.util.NoSuchElementException;
import org.junit.Test;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Lists;

public class AbstractIndexedIteratorTest {

  @Test
  public void testIterator() {
    AbstractIndexedIterator<String> iterator =
        createMockBuilder(AbstractIndexedIterator.class).createMock();
    expect(iterator.elements()).andReturn(3).anyTimes();
    expect(iterator.element(0)).andReturn("ONE");
    expect(iterator.element(1)).andReturn("TWO");
    expect(iterator.element(2)).andReturn("THREE");

    ImmutableList<String> expected = ImmutableList.of("ONE", "TWO", "THREE");

    replay(iterator);
    assertEquals(expected, Lists.newArrayList(iterator));
    verify(iterator);
  }

  @Test(expected = NoSuchElementException.class)
  public void testIteratorNoSuchElementException() {
    AbstractIndexedIterator<String> iterator =
        createMockBuilder(AbstractIndexedIterator.class).createMock();
    expect(iterator.elements()).andReturn(3).anyTimes();
    expect(iterator.element(0)).andReturn("ONE");
    expect(iterator.element(1)).andReturn("TWO");
    expect(iterator.element(2)).andReturn("THREE");

    replay(iterator);
    iterator.hasNext();
    assertEquals("ONE", iterator.next());
    assertEquals("TWO", iterator.next());
    assertEquals("THREE", iterator.next());
    iterator.next();

    verify(iterator);
  }

}
