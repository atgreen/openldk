// Tags: not-a-test
// Uses: Info InfoImpl InfoHelper

/*
* This file is part of the CORBA 2_3 tests, the test executable
* class being gnu.testlet.org.omg.CORBA_2_3.ORB.ValueTypeTest.
* Due large number of the required classes, they are moved into
* a separate package, Valuetype.
*
* @author Audrius Meskauskas (AudriusA@bluewin.ch)
*/

package gnu.testlet.org.omg.CORBA_2_3.ORB.Valtype;

public final class InfoHolder
  implements org.omg.CORBA.portable.Streamable
{
  public static int testMode;
  public Info value;

  public InfoHolder()
  {
  }

  public InfoHolder(Info initialValue)
  {
    value = initialValue;
  }

  public void _read(org.omg.CORBA.portable.InputStream i)
  {
    // Some IDL compilers generate the line below, forcing to search
    // the implementing class via classloader.
    if (testMode == 0)
      value = InfoHelper.read(i);
    else if (testMode == 1)

      // Despite it would be a lot faster to call:
      value =
        (Info) ((org.omg.CORBA_2_3.portable.InputStream) i).read_value(new InfoImpl());

    // And also should work:
    else
      value =
        (Info) ((org.omg.CORBA_2_3.portable.InputStream) i).read_value(InfoImpl.class);

    // The test checks the correct work of both cases.
  }

  public void _write(org.omg.CORBA.portable.OutputStream o)
  {
    InfoHelper.write(o, value);
  }

  public org.omg.CORBA.TypeCode _type()
  {
    return InfoHelper.type();
  }
}