package gnu.testlet.org.omg.CORBA;

import gnu.testlet.TestHarness;


public class Asserter {

  protected TestHarness h;

  public void assertTrue(String why, boolean a)
  {
    h.check(a, why);
  }

  public void assertEquals(String why, Object a, Object b)
  {
    if (a==null && b==null) return;
    h.check(a, b, why);
  }

  public void assertEquals(String why, long a, long b)
  {
    h.check(a, b, why);
  }

  public void assertEquals(String why, boolean a, boolean b)
  {
    h.check(a, b, why);
  }


  public void assertEquals(String why, double a, double b, double delta)
  {
    if (Math.abs(a-b)>delta)
      h.fail(a+" and "+b+", "+why);
  }


  public void fail(String why)
  {
    h.fail(why);
  }

  public void assertFalse(String why, boolean x_false)
  {
    h.check(!x_false, why);
  }

}