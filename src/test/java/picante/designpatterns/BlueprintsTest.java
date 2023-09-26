package picante.designpatterns;

import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertSame;
import org.junit.Test;

public class BlueprintsTest {

  @Test
  public void testNoConfiguration() {

    Builder<String, RuntimeException> builder = createMock(Builder.class);
    Blueprint<Builder<String, RuntimeException>> blueprint = Blueprints.empty();
    replay(builder);

    Builder<String, RuntimeException> result = blueprint.configure(builder);
    assertSame(result, builder);

    verify(builder);
  }

  @Test
  public void testConcatBlueprintOfBBlueprintOfB() {

    Builder<String, RuntimeException> builder = createMock(Builder.class);
    Blueprint<Builder<String, RuntimeException>> first = createMock(Blueprint.class);
    Blueprint<Builder<String, RuntimeException>> second = createMock(Blueprint.class);

    expect(first.configure(builder)).andReturn(builder);
    expect(second.configure(builder)).andReturn(builder);

    replay(builder, first, second);

    Blueprint<Builder<String, RuntimeException>> result = Blueprints.concat(first, second);
    Builder<String, RuntimeException> returned = result.configure(builder);

    assertSame(builder, returned);

    verify(builder, first, second);

  }

  @Test
  public void testConcatBlueprintOfBBlueprintOfBArray() {

    Builder<String, RuntimeException> builder = createMock(Builder.class);
    Blueprint<Builder<String, RuntimeException>> first = createMock(Blueprint.class);
    Blueprint<Builder<String, RuntimeException>> second = createMock(Blueprint.class);
    Blueprint<Builder<String, RuntimeException>> third = createMock(Blueprint.class);

    expect(first.configure(builder)).andReturn(builder);
    expect(second.configure(builder)).andReturn(builder);
    expect(third.configure(builder)).andReturn(builder);

    replay(builder, first, second, third);

    Blueprint<Builder<String, RuntimeException>> result = Blueprints.concat(first, second, third);
    Builder<String, RuntimeException> returned = result.configure(builder);

    assertSame(builder, returned);

    verify(builder, first, second, third);
  }

}
