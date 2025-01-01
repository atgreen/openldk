// Tags: JDK1.4

// Copyright (c) Object Oriented Concepts, Inc. Billerica, MA, USA

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

package gnu.testlet.org.omg.PortableServer.POA;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class testForwarding
  extends TestBase
  implements Testlet
{
  boolean fs_terminated;

  // This test demands write access to the folder where the program
  // is running.
  public void testForward()
  {
    try
      {
        new Thread()
          {
            public void run()
            {
              fs_terminated = false;
              TestLocationForwardServerMain.main(new String[ 0 ]);
              fs_terminated = true;
            }
          }.start();

        while (TestLocationForwardServerMain.ior == null)
          {
            try
              {
                Thread.sleep(100);
              }
            catch (InterruptedException ex)
              {
              }
          }
        try
          {
            Thread.sleep(200);
          }
        catch (InterruptedException ex)
          {
          }

        TestLocationForwardClient.main(new String[ 0 ]);
       
        // Wait at most 2 seconds for the termination of the server thread.
        long from = System.currentTimeMillis();
        do
          {
              Thread.sleep(100);
          }
        while (System.currentTimeMillis() - from < 2000 && !fs_terminated);

        harness.check(fs_terminated, "Server thread must exit");
      }
    catch (Exception ex)
      {
        fail(ex);
      }
  }

  public void test(TestHarness a_harness)
  {
    harness = a_harness;
    testForward();
  }
}