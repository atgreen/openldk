// Tags: not-a-test
// Uses: cmInfoHolder _GreetingsStub

/*
* This file is part of the CORBA 2_3 tests, the test executable
* class being gnu.testlet.org.omg.CORBA_2_3.ORB.ValueTypeTest.
* Due large number of the required classes, they are moved into
* a separate package, Valuetype.
*
* @author Audrius Meskauskas (AudriusA@bluewin.ch)
*/
package gnu.testlet.org.omg.CORBA_2_3.ORB.Valtype;

import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.InvokeHandler;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.ResponseHandler;

public abstract class _GreetingsImplBase
  extends ObjectImpl
  implements Greetings, InvokeHandler
{
  public _GreetingsImplBase()
  {
  }

  /**
   * As there is only one method supported, we can use the simplified
   * layout without switch between methods.
   */
  public OutputStream _invoke(String method, InputStream in, ResponseHandler rh)
  {
    cmInfoHolder w1 = new cmInfoHolder();
    w1.value = cmInfoHelper.read(in);

    InfoHolder w2 = new InfoHolder();
    w2.value = InfoHelper.read(in);

    this.hello(w1, w2);

    OutputStream out = rh.createReply();

    cmInfoHelper.write(out, w1.value);
    InfoHelper.write(out, w2.value);

    return out;
  }

  public String[] _ids()
  {
    return _GreetingsStub.__ids;
  }
}
