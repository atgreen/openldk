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

public interface InfoValueFactory
  extends org.omg.CORBA.portable.ValueFactory
{
  Info create(String name, String message);
}
