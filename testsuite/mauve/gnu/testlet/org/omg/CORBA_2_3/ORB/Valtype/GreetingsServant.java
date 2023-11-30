// Tags: not-a-test
// Uses: _GreetingsImplBase Greetings cmInfoHolder InfoHolder

/*
* This file is part of the CORBA 2_3 tests, the test executable
* class being gnu.testlet.org.omg.CORBA_2_3.ORB.ValueTypeTest.
* Due large number of the required classes, they are moved into
* a separate package, Valuetype.
*
* @author Audrius Meskauskas (AudriusA@bluewin.ch)
*/

package gnu.testlet.org.omg.CORBA_2_3.ORB.Valtype;

public class GreetingsServant
  extends _GreetingsImplBase
  implements Greetings
{
  /**
   * Make manipulations ensuring the values were received.
   */
  public void hello(cmInfoHolder w1, InfoHolder w2)
  {
    w1.value.message = "Names: " + w1.value.name + "+" + w2.value._name;
    w2.value._message =
      "Messages: " + w2.value._message + "+" + w1.value.message;

    w1.value.name += "+";
    w2.value._name += "+";
  }
}