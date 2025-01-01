// Tags: not-a-test
// Uses: ../../CORBA_2_3/ORB/Valtype/GreetingsServant

/* comServer.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package gnu.testlet.org.omg.CORBA.ORB;

import gnu.testlet.org.omg.CORBA.ORB.communication.comServant;
import gnu.testlet.org.omg.CORBA_2_3.ORB.Valtype.GreetingsServant;

import org.omg.CORBA.ORB;

/**
 * The ORB server class, used for test communication.
 * To avoid pausing for each test, all CORBA tests, related to the
 * client - server communication, are connected to this server.
 *
 * Warning: this server starts on ports 1126 and up.
 *
 * Modified, converting Classpath example into test.
 *
 * @author Audrius Meskauskas (AudriusA@bluewin.ch)
 */
public class comServer
{
  public static String[] IORs;
  public static ORB orb;

  public static void main(String[] args)
  {
    start_server(args);
  }

  /**
   * Start the ORB server and return the servant IOR reference. In this
   * test, both server and client are started on the same virtual machine,
   * just in the different threads.
   */
  public static synchronized String[] start_server(String[] args)
  {
    // Check maybe is already running.
    if (IORs != null)
      return IORs;
    try
      {
        IORs = new String[ 2 ];

        // Create and initialize the ORB.
        orb = org.omg.CORBA.ORB.init(args, null);

        // Create the test 1 servant and register it with the ORB.
        comServant tester = new comServant();
        orb.connect(tester);

        // Create the test 2 servant and register it with the ORB.
        GreetingsServant tester2 = new GreetingsServant();
        orb.connect(tester2);

        // Storing the IOR references.
        IORs [ 0 ] = orb.object_to_string(tester);
        IORs [ 1 ] = orb.object_to_string(tester2);

        new Thread()
          {
            public void run()
            {
              // wait for invocations from clients
              orb.run();
            }
          }.start();

        // Sleep for 3 seconds, allowing the server to start.
        try
          {
            Thread.sleep(3000);
          }
        catch (InterruptedException ex)
          {
          }

        return IORs;
      }
    catch (Exception e)
      {
        IORs = null;
        return null;
      }
  }
}