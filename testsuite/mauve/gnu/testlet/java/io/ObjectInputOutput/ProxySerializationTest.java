/* ProxySerializationTest.java -- Tests serialization of a Proxy
 Copyright (C) 2006 by Free Software Foundation, Inc. 
 Written by Olivier Jolly <olivier.jolly@pcedev.com>
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


package gnu.testlet.java.io.ObjectInputOutput;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

/**
 * Check that proxies are correctly serialized and doesn't cause reference
 * offset.
 * @author Olivier Jolly <olivier.jolly@pcedev.com>
 */
public class ProxySerializationTest implements Testlet
{

  public void test(TestHarness harness)
  {

    SerBaseInterface proxy = (SerBaseInterface) Proxy.newProxyInstance(
                                                                       SerBaseInterface.class.getClassLoader(),
                                                                       new Class[] { SerBaseInterface.class },
                                                                       new DummyInvocationHandler());
    SerializableLoopA serializableLoopA = new SerializableLoopA();
    SerializableLoopB serializableLoopB = new SerializableLoopB();

    // Create data which will force serialization references to be used
    serializableLoopA.setB(serializableLoopB);
    serializableLoopB.setA(serializableLoopA);

    harness.checkPoint("ProxySerializationTest");

    harness.check(proxy.getA(), -25679, "Proxy interception checking");

    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    ObjectOutputStream objectOutputStream = null;
    try
      {
        objectOutputStream = new ObjectOutputStream(baos);
        objectOutputStream.writeObject(proxy);
        objectOutputStream.writeObject(serializableLoopA);
        objectOutputStream.writeObject(serializableLoopB);
      }
    catch (IOException e)
      {
        harness.debug(e);
        harness.fail("Error while serialiazing a proxy");
      }

    ByteArrayInputStream bais = new ByteArrayInputStream(baos.toByteArray());

    try
      {
        ObjectInputStream objectInputStream = new ObjectInputStream(bais);
        SerBaseInterface serialized = (SerBaseInterface) objectInputStream.readObject();
        harness.check(serialized.getA(), -25679,
                      "Reserialized proxy working checking");

        // Get other object off the object stream and force them to be actually
        // used
        SerializableLoopA serializableLoopA2 = (SerializableLoopA) objectInputStream.readObject();
        SerializableLoopB serializableLoopB2 = (SerializableLoopB) objectInputStream.readObject();

        harness.check(serializableLoopA.getB(), serializableLoopA2.getB());        

      }
    catch (Exception e)
      {
        // If the reference counter got messed up, we should received a
        // ClassCastException or something similar
        harness.debug(e);
        harness.fail("Error while deserialiazing a proxy");
      }

  }

  private static class DummyInvocationHandler implements InvocationHandler,
      Serializable
  {

    private static final long serialVersionUID = -6475900781578075262L;

    public Object invoke(Object proxy, Method method, Object[] args)
        throws Throwable
    {
      if ("getA".equals(method.getName()))
        {
          return new Integer(-25679);
        }
      return method.invoke(proxy, args);
    }

  }

  private interface SerBaseInterface
  {
    int getA();
  }

}
