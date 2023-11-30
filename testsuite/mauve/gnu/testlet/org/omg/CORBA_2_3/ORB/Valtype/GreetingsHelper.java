// Tags: not-a-test
/*
* This file is part of the CORBA 2_3 tests, the test executable
* class being gnu.testlet.org.omg.CORBA_2_3.ORB.ValueTypeTest.
* Due large number of the required classes, they are moved into
* a separate package, Valuetype.
*
* @author Audrius Meskauskas (AudriusA@bluewin.ch)
*/


package gnu.testlet.org.omg.CORBA_2_3.ORB.Valtype;

import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.portable.Delegate;
import org.omg.CORBA.portable.ObjectImpl;

public abstract class GreetingsHelper
{
  public static String id()
  {
    return "IDL:gnu/testlet/org/omg/CORBA_2_3/ORB/Valtype/Greetings:1.0";
  }

  /**
   * We only need a narrow() method from this helper.
   */
  public static Greetings narrow(org.omg.CORBA.Object obj)
  {
    if (obj == null)
      return null;
    else if (obj instanceof Greetings)
      return (Greetings) obj;
    else if (!obj._is_a(id()))
      throw new BAD_PARAM();
    else
      {
        Delegate delegate = ((ObjectImpl) obj)._get_delegate();
        return new _GreetingsStub(delegate);
      }
  }
}
