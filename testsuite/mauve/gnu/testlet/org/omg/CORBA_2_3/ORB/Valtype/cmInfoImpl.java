/*
* This file is part of the CORBA 2_3 tests, the test executable
* class being gnu.testlet.org.omg.CORBA_2_3.ORB.ValueTypeTest.
* Due large number of the required classes, they are moved into
* a separate package, Valuetype.
*
* @author Audrius Meskauskas (AudriusA@bluewin.ch)
*/

package gnu.testlet.org.omg.CORBA_2_3.ORB.Valtype;

public class cmInfoImpl
  extends cmInfo
{
  public cmInfoImpl()
  {
    name = message = "Unitialised";
  }

  public cmInfoImpl(String a, String b)
  {
    name = a;
    message = b;
  }

  public String _toString()
  {
    return name+";"+message;
  }
}