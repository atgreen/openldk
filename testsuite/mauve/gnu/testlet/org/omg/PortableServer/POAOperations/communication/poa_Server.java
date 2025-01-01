// Tags: not-a-test
// Uses: ../poa_POA_test

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

package gnu.testlet.org.omg.PortableServer.POAOperations.communication;

import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.TreeMap;

import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.LocalObject;
import org.omg.CORBA.ORB;
import org.omg.CORBA.Policy;
import org.omg.CORBA.Request;
import org.omg.CORBA.TCKind;
import org.omg.PortableServer.ForwardRequest;
import org.omg.PortableServer.IdUniquenessPolicyValue;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;
import org.omg.PortableServer.RequestProcessingPolicyValue;
import org.omg.PortableServer.Servant;
import org.omg.PortableServer.ServantActivator;
import org.omg.PortableServer.ServantLocator;
import org.omg.PortableServer.ServantRetentionPolicyValue;
import org.omg.PortableServer.ServantLocatorPackage.CookieHolder;
import gnu.testlet.TestHarness;
import gnu.testlet.org.omg.PortableServer.POAOperations.*;

public class poa_Server
  extends LocalObject
  implements ServantActivator, ServantLocator
{
  TestHarness t;
  public poa_Servant s2a;
  public poa_Server m575;
  public poa_Server once_activated;
  public ArrayList incarnations = new ArrayList();
  public ArrayList etherializations = new ArrayList();
  public ArrayList preinvokes = new ArrayList();
  public ArrayList postinvokes = new ArrayList();
  String poaName;
  public boolean started;
  public static TreeMap references = new TreeMap();

  public poa_Server()
  {
    poaName = "???";
  }

  public poa_Server(String n)
  {
    poaName = n;
  }

  public ORB start_server(TestHarness a_t)
  {
    poaName = "main";
    t = a_t;
    try
      {
        // Create and initialize the ORB.
        final ORB orb = org.omg.CORBA.ORB.init(new String[ 0 ], null);

        // Create the servant and register it with the ORBs root POA.
        POA root_poa =
          POAHelper.narrow(orb.resolve_initial_references("RootPOA"));

        POA poa = root_poa.create_POA("1", null, policies_1(root_poa));

        poa.the_activator(new gnuAdapterActivator());

        POA npoa = poa.find_POA("xactivated", true);

        npoa.activate_object_with_id(new byte[] { 7, 5 }, new poa_Servant());
        write_reference(orb, npoa.id_to_reference(new byte[] { 7, 5 }),
                        "xactivated"
                       );

        root_poa.the_POAManager().activate();
        poa.the_POAManager().activate();

        poa_Servant once_activated_servant = new poa_Servant();

        org.omg.CORBA.Object servantObject =
          poa.create_reference_with_id(new byte[] { 4, 5, 2, 5, 7, 2 },
                                       poa_comTesterHelper.id()
                                      );

        org.omg.CORBA.Object localTestServant =
          poa.create_reference_with_id(new byte[] { 1, 2, 3, 4 },
                                       poa_comTesterHelper.id()
                                      );

        once_activated = new poa_Server(poa.the_name());
        poa.set_servant_manager(once_activated);

        write_reference(orb, servantObject, "IOR.txt");

        // Add a single servant POA.
        POA sspoa = root_poa.create_POA("2", null, policies_2(root_poa));

        poa_Servant always_same_servant = new poa_Servant();
        sspoa.set_servant(always_same_servant);

        org.omg.CORBA.Object sservantObject1 =
          sspoa.create_reference_with_id(new byte[] { 5, 4, 3, 2, 1 },
                                         poa_comTesterHelper.id()
                                        );

        write_reference(orb, sservantObject1, "ssIOR1.txt");

        org.omg.CORBA.Object sservantObject2 =
          sspoa.create_reference_with_id(new byte[] { 1, 2, 3, 4, 5 },
                                         poa_comTesterHelper.id()
                                        );

        write_reference(orb, sservantObject2, "ssIOR2.txt");

        sspoa.the_POAManager().activate();

        // Add a single servant POA that also allows to introduce the
        // explicitly activated alternative servant.
        POA sspoa_a = root_poa.create_POA("2a", null, policies_2a(root_poa));

        s2a = new poa_Servant();
        sspoa_a.set_servant(s2a);

        org.omg.CORBA.Object sservantObject1adefault1 =
          sspoa_a.create_reference_with_id(new byte[] { 4, 4, 4, 1 },
                                           poa_comTesterHelper.id()
                                          );

        org.omg.CORBA.Object sservantObject1adefault2 =
          sspoa_a.create_reference_with_id(new byte[] { 4, 4, 4, 2 },
                                           poa_comTesterHelper.id()
                                          );

        sspoa_a.activate_object_with_id(new byte[] { 4, 4, 4, 5, 5, 5, 5 },
                                        new poa_Servant()
                                       );

        org.omg.CORBA.Object sservantObject1aother =
          sspoa_a.id_to_reference(new byte[] { 4, 4, 4, 5, 5, 5, 5 });

        sspoa_a.the_POAManager().activate();

        // Activate the servant that would handle the redirections.
        poa_Servant redirection_handler =
          new poa_Servant()
          {
            /**
             * Just prints the hello message.
             */
            public String sayHello()
            {
              return "{redirection handler} " + array(super._object_id());
            }
          };
        poa.activate_object_with_id(new byte[] { 7, 7, 7 }, redirection_handler);
        write_reference(orb, poa.servant_to_reference(redirection_handler),
                        "redirector"
                       );

        org.omg.CORBA.Object o1;
        org.omg.CORBA.Object o2;
        org.omg.CORBA.Object o3;

        poa_comTester t1 =
          poa_comTesterHelper.narrow(o1 = sservantObject1adefault1);
        poa_comTester t2 =
          poa_comTesterHelper.narrow(o2 = sservantObject1adefault2);
        poa_comTester tx =
          poa_comTesterHelper.narrow(o3 = sservantObject1aother);

        String s1 = t1.sayHello();
        String s2 = t2.sayHello();
        String sx = tx.sayHello();

        String h1 = s1.substring(s1.lastIndexOf(":"));
        String h2 = s2.substring(s2.lastIndexOf(":"));
        String hx = sx.substring(sx.lastIndexOf(":"));

        t.check(h1, h2, "Must be same default servant");
        t.check(!h1.equals(hx), "Must be different servant");
        t.check(!h2.equals(hx), "Must be different servant");

        // Save them.
        write_reference(orb, t1, "T1");
        write_reference(orb, t2, "T2");
        write_reference(orb, tx, "TX");

        remotePoaControlServant control = new remotePoaControlServant();

        control.target_object_id = poa.reference_to_id(servantObject);

        org.omg.CORBA.Object controlObject =
          root_poa.servant_to_reference(control);

        write_reference(orb, controlObject, "Control.txt");

        POA subpoa = sspoa.create_POA("sub", null, policies_3(sspoa));

        m575 = new poa_Server(subpoa.the_name());
        subpoa.set_servant_manager(m575);

        org.omg.CORBA.Object sub =
          subpoa.create_reference_with_id(new byte[] { 5, 7, 5 },
                                          poa_comTesterHelper.id()
                                         );
        subpoa.the_POAManager().activate();
        write_reference(orb, sub, "ssIOR3.txt");

        poa_comTester s = poa_comTesterHelper.narrow(localTestServant);

        try
          {
            s.throwException(64);
            t.fail("LOCAL:User exception is not thrown");
          }
        catch (ourUserException ex)
          {
            t.check(64, ex.ourField, "LOCAL: user exception field value");
          }

        Request rq =
          s._create_request(null, "passCharacters", orb.create_list(2), null);
        rq.add_in_arg().insert_wstring("wide string");
        rq.add_in_arg().insert_string("narrow string");

        rq.set_return_type(orb.get_primitive_tc(TCKind.tk_wstring));

        rq.invoke();

        String rt = rq.result().value().extract_wstring();
        t.check("return 'narrow string' and 'wide string'", rt, "LOCAL:DII:");

        poa_comTesterHelper.narrow(sub).sayHello();

        s.theField(55);
        t.check(55, s.theField(), "LOCAL: field accessing");
        s.theField(17);

        String r = s.passCharacters("abba", "baba");
        t.check("return 'baba' and 'abba'", r);

        // The objects with such key will receive redirection exceptions.
        // We force the redirection exception the be throw by activator...
        org.omg.CORBA.Object willBeRedirected =
          poa.create_reference_with_id(new byte[] { 0x7F, 0, 0, 0, 8 },
                                       poa_comTesterHelper.id()
                                      );

        write_reference(orb, willBeRedirected, "willRedirActivator.txt");

        // ..and by locator.
        willBeRedirected =
          subpoa.create_reference_with_id(new byte[] { 0x7F, 0, 0, 0, 9 },
                                          poa_comTesterHelper.id()
                                         );

        write_reference(orb, willBeRedirected, "willRedirLocator.txt");

        new Thread()
          {
            public void run()
            {
              // Start the thread, serving the invocations from clients.
              orb.run();
            }
          }.start();

        started = true;

        return orb;
      }
    catch (Exception e)
      {
        throw new RuntimeException(e);
      }
  }

  /**
   * Pass the IOR reference. As all test run on the same jre, we can
   * just use the map, stored in the static field.
   */
  private static void write_reference(ORB orb, org.omg.CORBA.Object object,
                                      String file
                                     )
                               throws FileNotFoundException, BAD_PARAM
  {
    references.put(file, orb.object_to_string(object));
  }

  /**
   * The "standard" policy set, identical to the policies of the root
   * poa apart that the servant manager is used to locate the servant
   * and activate an object.
   */
  static Policy[] policies_1(POA poa)
  {
    return new Policy[]
           {
             poa.create_request_processing_policy(RequestProcessingPolicyValue.USE_SERVANT_MANAGER)
           };
  }

  /**
   * The policy set, defining that it will be a single servant per all
   * objects in this POA.
   */
  static Policy[] policies_2(POA poa)
  {
    return new Policy[]
           {
             poa.create_id_uniqueness_policy(IdUniquenessPolicyValue.MULTIPLE_ID),
             poa.create_servant_retention_policy(ServantRetentionPolicyValue.NON_RETAIN),
             poa.create_request_processing_policy(RequestProcessingPolicyValue.USE_DEFAULT_SERVANT),
           };
  }

  /**
   * The policy set, defining that it will be a single servant per all
   * objects in this POA, but due RETAIN it is also possible to activate
   * an object, explicitly indicating the alternative servant.
   */
  static Policy[] policies_2a(POA poa)
  {
    return new Policy[]
           {
             poa.create_id_uniqueness_policy(IdUniquenessPolicyValue.MULTIPLE_ID),
             poa.create_servant_retention_policy(ServantRetentionPolicyValue.RETAIN),
             poa.create_request_processing_policy(RequestProcessingPolicyValue.USE_DEFAULT_SERVANT),
           };
  }

  /**
   * The policy set, defining that the servant must each time incarnated
   * by the servant manager.
   */
  static Policy[] policies_3(POA poa)
  {
    return new Policy[]
           {
             poa.create_servant_retention_policy(ServantRetentionPolicyValue.NON_RETAIN),
             poa.create_request_processing_policy(RequestProcessingPolicyValue.USE_SERVANT_MANAGER),
           };
  }

  public Servant incarnate(byte[] key, POA poa)
                    throws org.omg.PortableServer.ForwardRequest
  {
    // Handle "hello" on redirector for all objects with key starting 0xFF.
    if (key [ 0 ] == 0x7F)
      {
        ORB orb = ORB.init(new String[ 0 ], null);
        org.omg.CORBA.Object redir = poa_POA_test.readIOR("redirector", orb);
        throw new ForwardRequest("redirecting", redir);
      }

    String s = array(key) + "/" + poa.the_name();
    incarnations.add(s);
    return new poa_Servant();
  }

  public void etherealize(byte[] key, POA poa, Servant servant, boolean b1,
                          boolean b2
                         )
  {
    String s = array(key) + "/" + poa.the_name();
    etherializations.add(s);
  }

  public static String array(byte[] a)
  {
    StringBuffer b = new StringBuffer();
    for (int i = 0; i < a.length; i++)
      {
        b.append(Integer.toHexString(a [ i ] & 0xFF));
        b.append(' ');
      }
    return b.toString();
  }

  public void postinvoke(byte[] key, POA poa, String method,
                         java.lang.Object cookie, Servant servant
                        )
  {
    String s =
      array(key) + method + "/" + poa.the_name() + "/" + cookie + ":" +
      (servant == null ? 0 : servant.hashCode());
    postinvokes.add(s);
  }

  public Servant preinvoke(byte[] key, POA poa, String method,
                           CookieHolder cholder
                          )
                    throws org.omg.PortableServer.ForwardRequest
  {
    // Handle "hello" on redirector for all objects with key starting 0xFF.
    if (key [ 0 ] == 0x7F)
      {
        ORB orb = ORB.init(new String[ 0 ], null);
        org.omg.CORBA.Object redir = poa_POA_test.readIOR("redirector", orb);
        throw new ForwardRequest("redirecting", redir);
      }

    String s = array(key) + method + "/" + poa.the_name();
    cholder.value = "cook";
    preinvokes.add(s);
    return new poa_Servant();
  }
}