/* ExceptionRaising.java -- Tests which check that the exceptions raised in an
 InvocationHandler are correct.
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


package gnu.testlet.java.lang.reflect.Proxy;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.Serializable;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.lang.reflect.UndeclaredThrowableException;

/**
 * Tests about the exceptions throwable when using proxies
 * @author Olivier Jolly <olivier.jolly@pcedev.com>
 * @see java.lang.reflect.Proxy
 */
public class ExceptionRaising implements Testlet
{

  public void test(TestHarness harness)
  {
    testWrappedException(harness);
    testReturnNull(harness);
    testClassCastException(harness);

  }

  /**
   * Test the behaviour of proxy the invocation of which returns
   * <code>null</code>
   * @param harness
   *          the test harness
   */
  private void testReturnNull(TestHarness harness)
  {
    Object proxy = Proxy.newProxyInstance(this.getClass().getClassLoader(),
                                          new Class[] { Serializable.class },
                                          new NullInvocationHandler());

    try
      {
        proxy.getClass();
        harness.checkPoint("Passed returning null when a non primitive object is expected");
      }
    catch (Exception e)
      {
        harness.fail("Returning null should be safe when a non primitive object is expected");
      }

    try
      {
        proxy.hashCode();
        harness.fail("Returning null when a primitive return value is expected should have thrown an exception");
      }
    catch (Exception e)
      {
        harness.check(e instanceof NullPointerException,
                      "Checking that exception thrown is a NullPointerException");
      }

  }

  /**
   * Tests the behaviour of proxy the invocation of which throws an exception
   * @param harness
   *          the test harness
   */
  private void testWrappedException(TestHarness harness)
  {
    Exception exception = new Exception();
    Object proxy = Proxy.newProxyInstance(
                                          this.getClass().getClassLoader(),
                                          new Class[] { Serializable.class },
                                          new ExceptionInvocationHandler(
                                                                         exception));

    try
      {
        proxy.toString();
        harness.fail("Call to toString via a proxy should have failed with an exception");
      }
    catch (UndeclaredThrowableException e)
      {
        harness.check(e.getUndeclaredThrowable(), exception,
                      "Exception thrown check");
      }
  }

  /**
   * Tests that if the return value is incorrect, a ClassCastException is thrown
   * @param harness
   *          the test harness
   */
  private void testClassCastException(TestHarness harness)
  {
    Object proxy = Proxy.newProxyInstance(this.getClass().getClassLoader(),
                                          new Class[] { Serializable.class },
                                          new ThisInvocationHandler());

    try
      {
        proxy.toString();
      }
    catch (Exception e)
      {
        harness.check(e instanceof ClassCastException,
                      "Checking that the raised exception was a ClassPathException");
      }
  }

  private static class ExceptionInvocationHandler implements InvocationHandler
  {

    Exception exception;

    public ExceptionInvocationHandler(Exception exception)
    {
      this.exception = exception;
    }

    public Object invoke(Object proxy, Method method, Object[] args)
        throws Throwable
    {
      throw exception;
    }

  }

  private static class NullInvocationHandler implements InvocationHandler
  {

    public Object invoke(Object proxy, Method method, Object[] args)
        throws Throwable
    {
      return null;
    }

  }

  private static class ThisInvocationHandler implements InvocationHandler
  {

    public Object invoke(Object proxy, Method method, Object[] args)
        throws Throwable
    {
      return this;
    }

  }

}
