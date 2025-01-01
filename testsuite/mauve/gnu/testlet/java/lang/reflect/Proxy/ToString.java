/* ToString.java -- Tests which checks that the toString method on a proxy is
 correctly forwarded to the InvocationHandler
 Copyright (C) 2006 olivier jolly <olivier.jolly@pcedev.com>
 This file is part of Mauve.

 Mauve is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2, or (at your option)
 any later version.

 Mauve is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with Mauve; see the file COPYING.  If not, write to the
 Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 02110-1301 USA.

 */

// Tags: JDK1.3


package gnu.testlet.java.lang.reflect.Proxy;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.Serializable;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

/**
 * Test which ensure that the toString method is properly forwarded to the
 * InvocationHandler. This tests notably fails with cacao 0.94
 * (http://b2.complang.tuwien.ac.at/cgi-bin/bugzilla/show_bug.cgi?id=17)
 * @author olivier jolly <olivier.jolly@pcedev.com>
 * @see java.lang.reflect.InvocationHandler
 */
public class ToString implements Testlet
{

  public void test(TestHarness harness)
  {
    InvocationHandler handler = new FacadeInvocationHandler(new Foo());
    Object proxy = Proxy.newProxyInstance(this.getClass().getClassLoader(),
                                          new Class[] { Serializable.class },
                                          handler);
    harness.check(proxy.toString(), "foo toString() result", "toString() test");
  }

  /**
   * Very simple facade which delegates all calls to a target object.
   */
  private static class FacadeInvocationHandler implements InvocationHandler
  {

    Object facaded;

    public FacadeInvocationHandler(Object facaded)
    {
      this.facaded = facaded;
    }

    public Object invoke(Object proxy, Method method, Object[] args)
        throws Throwable
    {
      return method.invoke(facaded, args);
    }

  }

  /**
   * Very simple class with a predictable toString() result.
   */
  private static class Foo
  {

    public String toString()
    {
      return "foo toString() result";
    }

  }

}
