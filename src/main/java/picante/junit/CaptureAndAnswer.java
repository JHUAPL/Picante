package picante.junit;

import org.easymock.Capture;
import org.easymock.EasyMock;
import org.easymock.IAnswer;
import org.easymock.IExpectationSetters;
import picante.designpatterns.Writable;

/**
 * This class exists to solve the problem in that arises when attempting to create mock
 * implementations of classes or interfaces implementing the &quot;buffer pattern&quot;.
 * <p>
 * Specifically, if you have a class or interface that you want to mock that has methods that look
 * like this:
 * </p>
 * 
 * <pre>
 * <code>
 *    interface BufferPatternExample {
 *    
 *       public Value get(double otherArg, Value buffer);
 *    
 *    }
 * </code>
 * </pre>
 * 
 * <p>
 * where the get method, by contract, is to mutate the contents of buffer and return a reference to
 * it for convenience, it turns out mocking can be a bit tricky.
 * </p>
 * <p>
 * This simple abstract class presents the solution to the problem:
 * </p>
 * 
 * <pre>
 * <code>
 *    BufferPatternExample mock = createMock(BufferPatternExample.class);
 *    
 *    CaptureAndAnswer&lt;Value&gt; capture = new CaptureAndAnswer&lt;Value&gt;() {
 *       public void set(Value captured) {
 *          ...mutate the contents of captured...
 *       }
 *    };
 * 
 *    expect(mock.get(eq(1.0), capture(capture.getCapture()))).andAnswer(capture);
 *    replay(mock);
 *    
 *    ...execute test code that triggers method call...
 *    
 *    verify(mock);
 * </pre>
 * 
 * </code>
 * <p>
 * Another, arguably awful, use case of this is for when the value is not returned. This is
 * counter-intuitive, as it requires a certain ordering of methods when interfacing with EasyMock.
 * Consider the following, similar interface:
 * </p>
 * 
 * <pre>
 * <code>
 *    interface BufferPatternExample2 {
 *    
 *       public void get(double otherArg, Value buffer);
 *    
 *    }
 * </code>
 * </pre>
 * <p>
 * Since the get function does not return a value, this complicates the &quot;andAnswer&quot; part
 * of the response. Fortunately EasyMock has a mechanism that provides for this, as an answer may
 * generate an exception, so the code will be invoked even if the return type is not present in the
 * function signature. For example:
 * </p>
 * 
 * <pre>
 * <code>
 *    BufferPatternExample2 mock = createMock(BufferPatternExample2.class);
 *    
 *    CaptureAndAnswer&lt;Value&gt; capture = new CaptureAndAnswer&lt;Value&gt;() {
 *       public void set(Value captured) {
 *          ...mutate the contents of captured...
 *       }
 *    };
 * 
 *    mock.get(eq(1.0), capture(capture.getCapture()));
 *    expectLastCall().andAnswer(capture);
 *    replay(mock);
 *    
 *    ...execute test code that triggers method call...
 *    
 *    verify(mock);
 * </code>
 * </pre>
 * <p>
 * Note: in our experience working with EasyMock the {@link IExpectationSetters#andAnswer(IAnswer)}
 * method must be invoked immediately after the {@link EasyMock#expectLastCall()} and prior to any
 * of the other chained method options from the {@link IExpectationSetters} interface.
 * </p>
 * 
 * @author G.K.Stephens
 * 
 * @param <T> the value of the buffer pattern function argument
 */
public abstract class CaptureAndAnswer<T> implements IAnswer<T> {

  private final Capture<T> capture = EasyMock.newCapture();

  /**
   * Method that mutates the contents of the capture into the desired result, simulating the effect
   * of executing the method.
   * 
   * @param captured the captured argument
   */
  public abstract void set(T captured);

  @Override
  public T answer() throws Throwable {
    set(capture.getValue());
    return capture.getValue();
  }

  public Capture<T> getCapture() {
    return capture;
  }

  /**
   * Creates a {@link CaptureAndAnswer} for classes that implement the {@link Writable} interface.
   * <p>
   * Many of the classes and interfaces that are typically utilized in the buffer pattern this
   * supporting class is designed to enable mocking support. So, in most cases, this static creation
   * method should be sufficient to serve as a simple means of using this class.
   * 
   * @param <U> The &quot;parent&quot; class or interface used in {@link Writable} interface
   * @param <W> The subclass of P that is to be captured.
   * 
   * @param valueToSet the value to set when the mocked function is invoked.
   * 
   * @return a newly created capture and answer
   */
  public static <U, W extends Writable<? super U, W>> CaptureAndAnswer<W> create(
      final U valueToSet) {

    return new CaptureAndAnswer<W>() {

      @Override
      public void set(W captured) {
        captured.setTo(valueToSet);
      }
    };

  }

}
