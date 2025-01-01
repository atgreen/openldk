//Tags: JDK1.3

//Copyright (C) 2005 Robert Schuster <thebohemian@gmx.net>

//This file is part of Mauve.

//Mauve is free software; you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation; either version 2, or (at your option)
//any later version.

//Mauve is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.

//You should have received a copy of the GNU General Public License
//along with Mauve; see the file COPYING.  If not, write to
//the Free Software Foundation, 59 Temple Place - Suite 330,
//Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.lang.reflect.Proxy;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.IOException;
import java.io.Serializable;
import java.lang.reflect.Proxy;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;

/** A basic test for the proxy mechanism that tests whether the arguments are delivered correctly.
 * 
 * @author Robert Schuster
 */
public class check13 implements Testlet, InvocationHandler, Serializable {
	
	transient Object proxy;
	transient Object[] args;
	transient Method method;
	
	public void test(TestHarness harness) {
		// Creates a Proxy implementation of an ActionListener that will
		// call the invoke() method whenever a method of al is called.
		ActionListener al = (ActionListener) Proxy.newProxyInstance(
				getClass().getClassLoader(),
				new Class[] { ActionListener.class }, this);

		Method expectedMethod = null;
		try {
			// Note: Proxy API uses the method that is declared in the interface not in the Proxy itself!
			expectedMethod = ActionListener.class.getMethod("actionPerformed", new Class[] { ActionEvent.class });
		} catch(NoSuchMethodException nsme) {
			harness.fail("test setup failed");
		}

		ActionEvent event = new ActionEvent(this, 0, "GNU yourself!");
		
		// Provokes a call to invoke().
		al.actionPerformed(event);

		// Note: Referential equality checks are used to really make sure we have
		// the same instance.
		
		harness.check(proxy == al, "proxy method called");
		harness.check(method, expectedMethod);
		harness.check(args.length, 1);
		harness.check(args[0] == event);

                // Test serialization
                harness.checkPoint("serialization");
                try {
                    ByteArrayOutputStream baos = new ByteArrayOutputStream();
                    ObjectOutputStream oos = new ObjectOutputStream(baos);
                    oos.writeObject(proxy);
                    oos.close();
                    ByteArrayInputStream bais = new ByteArrayInputStream(baos.toByteArray());
                    ObjectInputStream ois = new ObjectInputStream(bais);
                    Proxy p = (Proxy)ois.readObject();
                    harness.check(p.getClass() == proxy.getClass());
                    harness.check(Proxy.getInvocationHandler(p).getClass() == Proxy.getInvocationHandler(proxy).getClass());
                } catch(Exception x) {
                    harness.debug(x);
                    harness.fail("Unexpected exception");
                }
	}

	public Object invoke(Object proxy, Method method, Object[] args)
			throws Throwable {
		
		this.proxy = proxy;
		this.method = method;
		this.args = args;
		
		return null;
	}

}
