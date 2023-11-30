// Tags: not-a-test
// Uses: cmInfo cmInfoHelper

/*
* This file is part of the CORBA 2_3 tests, the test executable
* class being gnu.testlet.org.omg.CORBA_2_3.ORB.ValueTypeTest.
* Due large number of the required classes, they are moved into
* a separate package, Valuetype.
*
* @author Audrius Meskauskas (AudriusA@bluewin.ch)
*/

package gnu.testlet.org.omg.CORBA_2_3.ORB.Valtype;

public final class cmInfoHolder
  implements org.omg.CORBA.portable.Streamable
{
  public cmInfo value = null;

  public cmInfoHolder()
  {
  }

  public cmInfoHolder(cmInfo initialValue)
  {
    value = initialValue;
  }

  public void _read(org.omg.CORBA.portable.InputStream i)
  {
    value = cmInfoHelper.read(i);
  }

  public void _write(org.omg.CORBA.portable.OutputStream o)
  {
    cmInfoHelper.write(o, value);
  }

  public org.omg.CORBA.TypeCode _type()
  {
    return cmInfoHelper.type();
  }
}