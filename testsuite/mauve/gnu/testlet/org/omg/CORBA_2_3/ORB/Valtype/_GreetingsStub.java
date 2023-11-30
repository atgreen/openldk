// Tags: not-a-test
// Uses: cmInfoHelper InfoHolder

/*
* This file is part of the CORBA 2_3 tests, the test executable
* class being gnu.testlet.org.omg.CORBA_2_3.ORB.ValueTypeTest.
* Due large number of the required classes, they are moved into
* a separate package, Valuetype.
*
* @author Audrius Meskauskas (AudriusA@bluewin.ch)
*/

package gnu.testlet.org.omg.CORBA_2_3.ORB.Valtype;

import org.omg.CORBA.portable.ApplicationException;
import org.omg.CORBA.portable.Delegate;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.RemarshalException;

public class _GreetingsStub
  extends org.omg.CORBA.portable.ObjectImpl
  implements Greetings
{
  public static final String[] __ids =
    { "IDL:gnu/testlet/org/omg/CORBA_2_3/ORB/Valtype/Greetings:1.0" };

  public _GreetingsStub()
  {
  }

  public _GreetingsStub(Delegate delegate)
  {
    super();
    _set_delegate(delegate);
  }

  public void hello(cmInfoHolder w1, InfoHolder w2)
  {
    InputStream _in = null;
    try
      {
        OutputStream _out = _request("hello", true);
        cmInfoHelper.write(_out, w1.value);
        InfoHelper.write(_out, w2.value);
        _in = _invoke(_out);
        w1.value = cmInfoHelper.read(_in);
        w2.value = InfoHelper.read(_in);
      }
    catch (ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (RemarshalException _rm)
      {
        hello(w1, w2);
      }
    finally
      {
        _releaseReply(_in);
      }
  }

  public String[] _ids()
  {
    return (String[]) __ids.clone();
  }
}
