// Not a test, required by RMI_IIOP.java.

// Copyright (C) 2005 Audrius Meskauskas (AudriusA@Bioinformatics.org)

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.  */


package gnu.testlet.javax.rmi.CORBA.Tie;

import gnu.testlet.org.omg.CORBA_2_3.ORB.Valtype.Info;
import gnu.testlet.org.omg.CORBA_2_3.ORB.Valtype.cmInfo;

import java.io.Serializable;
import java.rmi.ConnectException;
import java.rmi.RemoteException;
import java.util.Collection;
import java.util.Iterator;

import javax.rmi.PortableRemoteObject;
import javax.xml.parsers.ParserConfigurationException;

import org.omg.CORBA.Object;
import org.omg.CORBA.portable.ObjectImpl;

/**
 * This file is part of the CORBA RMI over IIOP the test executable
 * class being gnu.testlet.javax.rmi.CORBA.Tie.RMI_IIOP.
 *
 * @author Audrius Meskauskas (AudriusA@bluewin.ch)
 */
public class RMI_testImpl
  extends PortableRemoteObject
  implements RMI_test, Serializable
{
  String ego = "<unassigned>";

  RMI_test other = null;

  public String getEgo()
    throws RemoteException
  {
    return ego;
  }

  public String passCorbaCMValueType(cmInfo info)
  {
    return "" + info;
  }

  public String passCorbaValueType(Info info)
  {
    return "" + info;
  }

  public String passCorbaValueTypeArray(Info[] infos)
  {
    if (infos == null)
      return "null";
    StringBuffer b = new StringBuffer();
    for (int i = 0; i < infos.length; i++)
      {
        b.append(infos[i]);
        b.append(":");
      }
    return b.toString();
  }

  public String passStructure(myStructure s)
    throws RemoteException
  {
    return "" + s;
  }

  public String passStructureArray(myStructure[] infos)
  {
    if (infos == null)
      return "null";
    StringBuffer b = new StringBuffer();
    for (int i = 0; i < infos.length; i++)
      {
        b.append(infos[i]);
        b.append(":");
      }
    return b.toString();
  }

  public RMI_testImpl()
    throws RemoteException
  {
    super();
  }

  public String joinStrings(String a, String b)
    throws RemoteException
  {
    if (a != null && a.equals("throw_remote"))
      {
        RemoteException t;
        try
          {
            Throwable cause = new ParserConfigurationException("Uje!");
            cause.initCause(new OutOfMemoryError("OOO!"));
            cause.fillInStackTrace();
            t = new RemoteException("Thrown remote AUDRIUS" + b, cause);

          }
        catch (Exception ex)
          {
            return "Unable to instantiate " + b + ": " + ex;
          }
        throw t;
      }
    else if (a != null && a.equals("throw_runtime"))
      {
        RuntimeException t;
        try
          {
            t = new ArrayIndexOutOfBoundsException(
              "Thrown ArrayIndexOutOfBoundsException AUDRIUS");
          }
        catch (Exception ex)
          {
            return "Unable to instantiate " + b + ": " + ex;
          }
        throw t;
      }
    else if (a != null && a.equals("throw_error"))
      {
        Error t;
        try
          {
            t = new InternalError("Thrown InternalError Audrius");
          }
        catch (Exception ex)
          {
            return "Unable to instantiate " + b + ": " + ex;
          }
        throw t;
      }
    else if (a != null && a.equals("throw_cex"))
      {
        throw new ConnectException("Connect exception message");
      }
    else

      return "'" + a + "' and '" + b + "'";
  }

  public long multiply(byte a, long b)
    throws RemoteException
  {
    return a * b;
  }

  public int passArray(int[] array)
    throws RemoteException
  {
    int s = 0;
    for (int i = 0; i < array.length; i++)
      {
        s += array[i];
      }
    return s;
  }

  public String passPrimitives(byte b, double d, int i, String s, float f,
    char c, short sh)
    throws RemoteException
  {
    return "byte " + b + ", double " + d + ", int " + i + ", string  " + s
      + ", float " + f + ", char " + c + "(" + Long.toHexString(c) + ")"
      + ", short " + sh;
  }

  public String passStringArray(String[] array)
    throws RemoteException
  {
    StringBuffer b = new StringBuffer();

    for (int i = 0; i < array.length; i++)
      {
        b.append(array[i]);
        b.append(".");
      }

    return b.toString();
  }

  public String sayHello(RMI_test h)
    throws RemoteException
  {
    if (h == null)
      return "null";
    else
      return h.getEgo();
  }

  public String passCorbaObject(Object object)
  {
    if (object == null)
      return "null passed";
    return ((ObjectImpl) object)._ids()[0];
  }

  public NodeObject exchangeNodeObject(NodeObject nx)
    throws RemoteException
  {
    try
      {
        if (nx.z_anotherTest != null)
          {
            nx.z_anotherTest = this;
            return nx;
          }
        else if (nx.anotherTestArray != null)
          {
            StringBuffer rv = new StringBuffer();

            for (int i = 0; i < nx.anotherTestArray.length; i++)
              {
                if (nx.anotherTestArray[i] != null)
                  rv.append(nx.anotherTestArray[i].getEgo());
                else
                  rv.append("null");
                rv.append(".");
              }

            nx.label = rv.toString();
            nx.z_anotherTest = this;
            return nx;
          }
        else
          {
            if (!nx.toString().equals(NodeObject.create1().toString()))
              {
                String msg = "Incorrect graph received " + nx + " expected "
                  + NodeObject.create1();
                System.out.println();
                System.out.println(msg);

                throw new RemoteException(msg);
              }
            else
              return NodeObject.create2();
          }
      }
    catch (Exception e)
      {
        throw new RemoteException("Exception has been thrown: " + e);
      }
  }

  /**
   * Same, the idea is just to test arrays.
   */
  public String passArrayOfRemotes(RMI_test[] tests)
    throws RemoteException
  {
    String[] expected = new String[] { "Local client object",
      "Client implementation instance", null, "Local client object",
      "Server side object" };

    if (tests.length != expected.length)
      return "Length mismatch, must be " + expected.length + " but "
        + tests.length;

    for (int i = 0; i < tests.length; i++)
      {
        if (tests[i] == null)
          {
            if (expected[i] != null)
              return i + ":" + tests[i] + " versus " + expected[i];
          }
        else
          {
            if (!expected[i].equals(tests[i].getEgo()))
              return i + ":" + tests[i].getEgo() + " versus " + expected[i];
          }
      }
    return "ok";
  }

  /**
   * Create and return new Remote.
   */
  public RMI_test passReturnRemote(RMI_test test)
    throws RemoteException
  {
    if (test == null)
      return null;
    else
      {
        RMI_testImpl impl = new RMI_testImpl();
        impl.ego = "ab (" + ego + ":" + (test == null ? "null" : test.getEgo())
          + ")";
        return impl;
      }
  }

  public String passCollection(Collection cx)
    throws RemoteException
  {
    StringBuffer b = new StringBuffer();
    b.append(cx.getClass().getName() + ":");

    Iterator iter = cx.iterator();

    while (iter.hasNext())
      {
        b.append(iter.next());
        b.append(".");
      }
    return b.toString();
  }

}