// Tags: JDK1.4
// Uses: RMI_test RMI_testImpl ../../../../org/omg/CORBA_2_3/ORB/Valtype/Info ../../../../org/omg/CORBA_2_3/ORB/Valtype/InfoImpl ../../../../org/omg/CORBA_2_3/ORB/Valtype/cmInfo ../../../../org/omg/CORBA_2_3/ORB/Valtype/cmInfoImpl ../../../../org/omg/CORBA_2_3/ORB/Valtype/cmInfoHelper

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

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.org.omg.CORBA_2_3.ORB.Valtype.Info;
import gnu.testlet.org.omg.CORBA_2_3.ORB.Valtype.InfoImpl;
import gnu.testlet.org.omg.CORBA_2_3.ORB.Valtype.cmInfoImpl;

import java.util.ArrayList;
import java.util.Collection;

import javax.rmi.PortableRemoteObject;
import javax.rmi.CORBA.Tie;
import javax.rmi.CORBA.Util;

import org.omg.CORBA.ORB;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;
import org.omg.PortableServer.Servant;

/**
 * The RMI over IIOP test. While formally possible since jdk 1.4 (all classes
 * declared), the Sun's jdk 1.4.2 seems not complete enough. The test passes
 * Sun's releases since 1.5.0_04-b05.
 * 
 * @author Audrius Meskauskas (AudriusA@bluewin.ch)
 */
public class RMI_IIOP
  implements Testlet
{
  public void test(TestHarness harness)
  {
    ORB client_orb = null;
    // Set the loader of this class as a context class loader, ensuring that the
    // CORBA implementation will be able to locate the RMI stubs and ties.
    ClassLoader previous = Thread.currentThread().getContextClassLoader();
    Thread.currentThread().setContextClassLoader(getClass().getClassLoader());
    try
      {
        client_orb = ORB.init(new String[0], null);

        String ior = startServer(harness);

        org.omg.CORBA.Object object = client_orb.string_to_object(ior);

        RMI_test r = (RMI_test) PortableRemoteObject.narrow(object,
          RMI_test.class);

        harness.check("null", r.sayHello(null), "First call");

        harness.check(r.multiply((byte) 4, 6), 24, "Multiplying 4*6=");

        harness.check("'String one' and 'String two'", r.joinStrings(
          "String one", "String two"), "Joining strings");

        harness.check("'null' and 'null'", r.joinStrings(null, null),
          "Passing null");

        harness.check(

          "byte 1, double 2.34, int 5, string  six, float 7.89, char A(41), short 11",
          r.passPrimitives((byte) 1, 2.34, 5, "six", (float) 7.89, 'A',
            (short) 11), "Passing primitives");

        harness.check(25, r.passArray(new int[] { 1, 2, 4, 8, 10 }),
          "Passing primitive array");

        String[] arr = new String[] { "a", "b", null, "abc" };
        harness.check("a.b.null.abc.", r.passStringArray(arr),
          "Passing String array");

        harness.check("1 2 three", r.passStructure(new myStructure()),
          "Passing structure");

        cmInfoImpl c1 = new cmInfoImpl();
        c1.message = "msg";
        c1.name = "nm";

        harness.check(r.passCorbaCMValueType(c1), "nm;msg", "CustomMarshal");

        InfoImpl c2 = new InfoImpl();
        c2._message = "_msg";
        c2._name = "_nm";

        harness.check(r.passCorbaValueType(c2), "_nm--_msg", "Streamable");

        myStructure[] a = new myStructure[4];

        // First element will be null.
        for (int i = 1; i < a.length; i++)
          {
            a[i] = new myStructure();
            a[i].a = 100 * i;
            a[i].c = "c" + i;
          }
        harness.check("null:100 2 c1:200 2 c2:300 2 c3:",
          r.passStructureArray(a), "Structure array");

        Info i1 = new InfoImpl();
        i1._message = "m1";
        i1._name = "n1";
        Info i2 = new InfoImpl();
        i2._message = "m2";
        i2._name = "n2";

        harness.check("n1--m1:null:n2--m2:",
          r.passCorbaValueTypeArray(new Info[] { i1, null, i2 }),
          "Value type array");

        harness.check("null passed", r.passCorbaObject(null),
          "null as CORBA object");

        String s = r.exchangeNodeObject(NodeObject.create1()).toString();
        harness.check(s, NodeObject.create2().toString(), "Graph");

        // Instantiate another RMI_test here.
        POA rootPOA = POAHelper.narrow(client_orb.resolve_initial_references("RootPOA"));

        rootPOA.the_POAManager().activate();

        RMI_testImpl impl = new RMI_testImpl();
        impl.ego = "Local client object";

        NodeObject n = new NodeObject("x");

        Tie tie = Util.getTie(impl);

        org.omg.CORBA.Object l_object = rootPOA.servant_to_reference((Servant) tie);

        RMI_test l_r = (RMI_test) PortableRemoteObject.narrow(l_object,
          RMI_test.class);

        n.z_anotherTest = l_r;

        harness.check("Local client object", l_r.getEgo(), "Local client");

        // The server should returns its own object in return:
        RMI_test rt = r.exchangeNodeObject(n).z_anotherTest;
        harness.check("Server side object", rt.getEgo(), "Server side object");

        // The server should echo the name of the passed object:
        harness.check("Local client object", r.sayHello(l_r), "Local client");

        RMI_testImpl impl2 = new RMI_testImpl();
        impl2.ego = "Client implementation instance";

        harness.check("Client implementation instance", r.sayHello(impl2),
          "Client implementation");

        n.anotherTestArray = new RMI_test[] { impl, null, impl2, rt };
        n.z_anotherTest = null;

        // Verifying array of remotes that is a field in the structure being
        // passed.
        String rts = r.exchangeNodeObject(n).label;
        harness.check(
          "Local client object.null.Client implementation instance.Server side object.",
          rts, "Passed array of 4 Remotes in a structure field.");

        harness.check("ab (Server side object:Local client object)",
          r.passReturnRemote(l_r).getEgo(), "Pass/return remote, stub");

        harness.check("ab (Server side object:Client implementation instance)",
          r.passReturnRemote(impl2).getEgo(),
          "Pass/return remote, implementation");

        harness.check(r.passReturnRemote(null) == null, "pass/get null");

        // If the verification of the server side succeeds, the "ok" is
        // returned. Otherwise, the mismatching entry is returned.
        harness.check("ok", r.passArrayOfRemotes(new RMI_test[] { impl, impl2,
          null, l_r, rt }), "Pass Remote[]");

        Collection ar = new ArrayList();

        ar.add("one");
        ar.add("two");
        ar.add("three");

        Externa e = new Externa();
        e.a = 17;
        e.b = 64;
        ar.add(e);

        ar.add(null);
        // Make a graph.
        ar.add(e);
        ar.add(e);

        Externa b = new Externa();
        b.a = 55;
        b.b = 56;
        ar.add(b);
        ar.add(e);
        ar.add("last");

        harness.check(
          "java.util.ArrayList:one.two.three.(ex 17:64).null.(ex 17:64).(ex 17:64).(ex 55:56).(ex 17:64).last.",
          r.passCollection(ar), "Pass ArrayList");

      }
    catch (Exception e)
      {
        harness.fail("Exception: "
          + e
          + ". If this is Sun's jre, note theat at least jdk 1.5.0_04-b05 required.");
      }
    finally
      {
        Thread.currentThread().setContextClassLoader(previous);
        try
          {
            if (server_orb != null)
              server_orb.destroy();
            if (client_orb != null)
              client_orb.destroy();
          }
        catch (Throwable t)
          {
            // Failed to destroy. 
            harness.fail("Unable to destroy the ORBs: "+t);
          }
      }
  }

  ORB server_orb;

  public String startServer(TestHarness harness)
  {
    try
      {
        server_orb = ORB.init(new String[0], null);

        new Thread()
        {
          public void run()
          {
            server_orb.run();
          }
        }.start();

        // Wait for 500 ms for the sever to start.
        Thread.sleep(500);

        POA rootPOA = POAHelper.narrow(server_orb.resolve_initial_references("RootPOA"));

        rootPOA.the_POAManager().activate();

        RMI_testImpl impl = new RMI_testImpl();
        impl.ego = "Server side object";
        Tie tie = Util.getTie(impl);

        org.omg.CORBA.Object object = rootPOA.servant_to_reference((Servant) tie);

        String ior = server_orb.object_to_string(object);

        return ior;
      }
    catch (Exception e)
      {
        harness.fail("Unable to initalise ORB: " + e);
        return null; // Unreachable.
      }    
  }
}
