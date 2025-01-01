/* DeclaringClass.java -- Checks for the declaring class of the special 
 methods in Object, namely toString, Equals and hashCode
 Copyright (C) 2006 Olivier Jolly <olivier.jolly@pcedev.com>
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
// Uses: ProxyUtils


package gnu.testlet.java.lang.reflect.Proxy;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.Serializable;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

/**
 * Checks that the public non final methods of Objects (ie equals, toString and
 * hashCode) are built in Proxy with Object as declaring class whatever the
 * method definition of the interfaces used for the proxy creation
 * @author Olivier Jolly <olivier.jolly@pcedev.com>
 */
public class DeclaringClass implements Testlet
{

  public void test(TestHarness harness)
  {
    Class[] testableInterfaces = { Serializable.class,
                                  WithObjectOverrides.class,
                                  WithoutObjectOverrides.class, Base.class,
                                  Derived.class };
    for (int i = 0; i < testableInterfaces.length; i++)
      {
        Class interfaceItem = testableInterfaces[i];
        Object proxy = Proxy.newProxyInstance(
                                              this.getClass().getClassLoader(),
                                              new Class[] { interfaceItem },
                                              new ExpectObjectDeclaringClassIfPossibleHandler(
                                                                                              harness));
        harness.checkPoint("Testing " + interfaceItem);
        proxy.equals(new Object());
        proxy.hashCode();
        proxy.toString();
      }
  }

  /**
   * Handler which checks that invoked public non final methods of Object have
   * their declared class set to Object.class
   */
  private static class ExpectObjectDeclaringClassIfPossibleHandler implements
      InvocationHandler
  {

    static Collection objectMethods;

    static
      {
        objectMethods = new ArrayList();
        try
          {
            objectMethods.add(Object.class.getMethod(
                                                     "equals",
                                                     new Class[] { Object.class }));
            objectMethods.add(Object.class.getMethod("hashCode", null));
            objectMethods.add(Object.class.getMethod("toString", null));
          }
        catch (NoSuchMethodException e)
          {
            e.printStackTrace();
            throw new Error("Missing core methods in Object");
          }
      }

    TestHarness harness;

    public ExpectObjectDeclaringClassIfPossibleHandler(TestHarness harness)
    {
      this.harness = harness;
    }

    public Object invoke(Object proxy, Method method, Object[] args)
        throws Throwable
    {
      boolean expectObjectDeclaringClass = false;
      for (Iterator iter = objectMethods.iterator(); iter.hasNext();)
        {
          Method objectMethod = (Method) iter.next();
          if (ProxyUtils.compareMethodOnNameAndParameterTypes(objectMethod,
                                                              method))
            {
              expectObjectDeclaringClass = true;
            }
        }

      harness.check((method.getDeclaringClass() == Object.class) == expectObjectDeclaringClass);

      return ProxyUtils.getNeutralValue(method.getReturnType());
    }

  }

  /**
   * Interface redefining the same public non final methods as Object
   */
  private static interface WithObjectOverrides
  {

    public boolean equals(Object obj);

    public int hashCode();

    public String toString();

  }

  /**
   * Interface redefining similar methods than Object
   */
  private static interface WithoutObjectOverrides
  {
    public boolean equals();

    public long hashCode(Object obj);

    public void toString(long foo);
  }

  /**
   * Simple interface defining a method
   */
  private static interface Base
  {
    public void foo();
  }

  /**
   * Simple interface overriding a non Object method
   */
  private static interface Derived extends Base
  {
    public void foo();
  }

}
