/*
* This file is part of the CORBA 2_3 tests, the test executable
* class being gnu.testlet.org.omg.CORBA_2_3.ORB.ValueTypeTest.
* Due large number of the required classes, they are moved into
* a separate package, Valuetype.
*
* @author Audrius Meskauskas (AudriusA@bluewin.ch)
*/

package gnu.testlet.org.omg.CORBA_2_3.ORB.Valtype;

import java.io.Serializable;

public class cmInfoDefaultFactory
  implements cmInfoValueFactory
{
  public cmInfo create(String name, String message)
  {
    return new cmInfoImpl(name, message);
  }

  public Serializable read_value(org.omg.CORBA_2_3.portable.InputStream is)
  {
    return is.read_value(new cmInfoImpl());
  }
}