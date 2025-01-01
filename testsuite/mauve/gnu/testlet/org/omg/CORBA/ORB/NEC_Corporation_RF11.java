// Tags: JDK1.2

// Copyright (c) 2000, 2001 NEC Corporation.

// Adapted for Mauve by Audrius Meskauskas <audriusa@bluewin.ch>

// This file is part of Mauve.

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
// Boston, MA 02111-1307, USA.

/*
This code originally came from the OMG's CORBA Open Source Testing project,
which lived at cost.omg.org. That site no longer exists.

All the contributing companies agreed to release their tests under the
terms of the GNU Lesser General Public License, available in the file
COPYING.LIB.

The code has been modified integrating into Mauve test environment and
removing tests that are not yet supported by Suns jre 1.4. Hence the license
is now GPL.

We downloaded the code from http://sourceforge.net/projects/corba-cost/,
administrated by Duncan Grisby.
*/


package gnu.testlet.org.omg.CORBA.ORB;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.org.omg.CORBA.ORB.RF11.NEC_RF11;
import gnu.testlet.org.omg.CORBA.ORB.RF11.rf11Caller;
import gnu.testlet.org.omg.CORBA.ORB.RF11.rf11Helper;
import gnu.testlet.org.omg.CORBA.ORB.RF11.rf11Servant;

import org.omg.CORBA.ORB;

import java.util.Iterator;

/**
 * This is a main test class for the RF11 test. It uses classes in the
 * underlying package RF11.
 */
public class NEC_Corporation_RF11
  implements Testlet
{
  public void test(TestHarness harness)
  {
    // Start the server.
    // Initializing ORB/POA and server object
    final ORB server_orb = ORB.init(new String[ 0 ], null);

    rf11Servant servant = new rf11Servant();

    server_orb.connect(servant);

    // Writing stringified IOR to file specified by IORfilename
    String ior = server_orb.object_to_string(servant);

    new Thread()
      {
        public void run()
        {
          server_orb.run();
        }
      }.start();

    // Wait for 3 seconds for the server to start.
    try
      {
        Thread.sleep(3000);
      }
    catch (InterruptedException ex)
      {
      }

    // Start the client.
    ORB client_orb = ORB.init(new String[ 0 ], null);
    NEC_RF11 obj = rf11Helper.narrow(client_orb.string_to_object(ior));
    rf11Caller ccall = new rf11Caller();

    ccall.init(client_orb, obj);
    ccall.run_all(harness);

    // Test for the server messages
    Iterator iter = servant.error_messages.iterator();
    while (iter.hasNext())
      {
        harness.fail("Server side error: " + iter.next());
      }

    // Shutdown bot ORBs.
    server_orb.shutdown(false);
    client_orb.shutdown(false);
  }
}