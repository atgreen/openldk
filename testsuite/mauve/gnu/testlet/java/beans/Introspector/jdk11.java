// Tags: JDK1.1
// Uses: A B C
package gnu.testlet.java.beans.Introspector;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.beans.*;

public class jdk11 implements Testlet {
  public void tryone (TestHarness harness, Class k1, Class k2, boolean force,
		      int dlen, int evlen, int gmlen)
    {
      try
	{
	  BeanInfo b;
	  if (! force && k2 == null)
	    b = Introspector.getBeanInfo (k1);
	  else
	    b = Introspector.getBeanInfo (k1, k2);
	  harness.debug (k1 + "/" + k2 + ":");
	  harness.debug ("BeanInfo.getPropertyDescriptors().length = " 
			+ b.getPropertyDescriptors().length + " ?= " + dlen);
	  harness.check (b.getPropertyDescriptors().length, dlen);
	  harness.debug ("BeanInfo.getEventSetDescriptors().length = "
			 + b.getEventSetDescriptors().length + " ?= " + evlen);
	  harness.check (b.getEventSetDescriptors().length, evlen);
	  harness.debug ("BeanInfo.getMethodDescriptors().length = " 
			 + b.getMethodDescriptors().length + " ?= " + gmlen);
	  harness.check (b.getMethodDescriptors().length, gmlen);
	}
      catch (Throwable e)
	{
	  harness.check (false);
	  harness.debug (e);
	}
    }

  public void tryone (TestHarness harness, Class k1, Class k2,
		      int dlen, int evlen, int gmlen)
    {
      tryone (harness, k1, k2, false, dlen, evlen, gmlen);
    }

  public void tryone (TestHarness harness, Class k,
		      int dlen, int evlen, int gmlen)
    {
      tryone (harness, k, null, false, dlen, evlen, gmlen);
    }

  public void test (TestHarness harness)
    {
      harness.checkPoint ("decapitalize");
      harness.check (Introspector.decapitalize ("FooBar"), "fooBar");
      harness.check (Introspector.decapitalize ("Foo"), "foo");
      harness.check (Introspector.decapitalize ("X"), "x");
      harness.check (Introspector.decapitalize ("BAR"), "BAR");

      harness.checkPoint ("getBeanInfo");
      tryone (harness, gnu.testlet.java.beans.Introspector.jdk11.class, 1, 0, 13);
      tryone (harness, gnu.testlet.java.beans.Introspector.A.class, 1, 0, 11);
      tryone (harness, gnu.testlet.java.beans.Introspector.B.class, 1, 0, 11);
      tryone (harness, gnu.testlet.java.beans.Introspector.C.class, 1, 0, 12);
      tryone (harness, gnu.testlet.java.beans.Introspector.C.class,
	      gnu.testlet.java.beans.Introspector.B.class, 0, 0, 1);

      harness.checkPoint ("getBeanInfoSearchPath");
      String search[] = Introspector.getBeanInfoSearchPath ();
      for (int i = 0; i < search.length; i++)
	harness.debug ("getBeanInfoSearchPath value[" + i + "]: " + search[i]);
      harness.check (search.length > 0);
 
      harness.checkPoint ("setBeanInfoSearchPath");
      String path[] = {"a.b.c", "d.e.f"};
      Introspector.setBeanInfoSearchPath (path);
      harness.check (path.length == Introspector.getBeanInfoSearchPath().length);

      Introspector.setBeanInfoSearchPath (search);
      
    }
}
