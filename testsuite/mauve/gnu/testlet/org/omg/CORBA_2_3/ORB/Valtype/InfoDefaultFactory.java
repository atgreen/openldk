/*
* This file is part of the CORBA 2_3 tests, the test executable
* class being gnu.testlet.org.omg.CORBA_2_3.ORB.ValueTypeTest.
* Due large number of the required classes, they are moved into
* a separate package, Valuetype.
*
* @author Audrius Meskauskas (AudriusA@bluewin.ch)
*/

package gnu.testlet.org.omg.CORBA_2_3.ORB.Valtype;

public class InfoDefaultFactory
  implements InfoValueFactory
{
  public Info create(String name, String message)
  {
    return new InfoImpl(name, message);
  }

  public java.io.Serializable read_value(org.omg.CORBA_2_3.portable.InputStream is)
  {
    return is.read_value(new InfoImpl());
  }
}