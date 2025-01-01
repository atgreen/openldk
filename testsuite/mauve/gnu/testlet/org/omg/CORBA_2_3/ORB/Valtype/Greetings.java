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

import org.omg.CORBA.portable.IDLEntity;

public interface Greetings
  extends org.omg.CORBA.Object, IDLEntity
{
  void hello(cmInfoHolder w1,
             InfoHolder w2
            );
}
