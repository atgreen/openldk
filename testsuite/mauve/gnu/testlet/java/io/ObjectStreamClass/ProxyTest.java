// Tags: JDK1.3

/* ProxyTest.java -- Tests ObjectStreamClass class for Proxy classes

   Copyright (c) 2003 by Free Software Foundation, Inc.
   Written by Mark Wielaard <mark@klomp.org>.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation, version 2. (see COPYING)

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation
   Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307 USA */

package gnu.testlet.java.io.ObjectStreamClass;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.io.ObjectStreamClass;
import java.io.ObjectStreamField;

public class ProxyTest implements Testlet, InvocationHandler
{

  /** Do nothing method to implement InvocationHandler */
  public Object invoke(Object p, Method m, Object[] os) { return null; }

  public void test (TestHarness harness)
  {
    Class pc = Proxy.getProxyClass(this.getClass().getClassLoader(),
				   new Class[] { Comparable.class });
    ObjectStreamClass osc = ObjectStreamClass.lookup (pc);

    harness.check(osc.getSerialVersionUID(), 0, "zero serialVersionUID");
    ObjectStreamField[] osfs = osc.getFields();
    harness.check(osfs != null && osfs.length == 0, "zero ObjectStreamFields");
    harness.check(osc.getField("any"), null, "getField(any) returns null");
    harness.check(osc.getField("h"), null, "getField(h) returns null");
  }
}
