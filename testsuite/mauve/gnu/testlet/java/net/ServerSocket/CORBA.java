// Tags: JDK1.2
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


package gnu.testlet.java.net.ServerSocket;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.IOException;
import java.net.BindException;
import java.net.ServerSocket;
import java.util.Random;

/**
 * This class tests the socket functionality, required by
 * CORBA server to work properly. If this does not work,
 * CORBA server does not work either.
 *
 * @author Audrius Meskauskas (AudriusA@bluewin.ch)
 */
public class CORBA
  implements Testlet
{
  static int NONE = Integer.MIN_VALUE;

  public void test(TestHarness harness)
  {
    // Try 54 times to bind into random port
    // between 1000 and 3000.
    Random r = new Random();

    int port = NONE;
    ServerSocket s = null;

    Search:
    for (int i = 0; i < 54; i++)
      {
        port = 1000 + r.nextInt(2000);
        try
          {
            s = new ServerSocket(port);
            break Search;
          }

        // Unable to bind, probably the port is in use.
        catch (IOException ex)
          {
            // repeat the loop.
          }
      }

    if (port == NONE)
      {
        harness.fail("Cannot find any port " +
                     "between 1000 and 3000 in 54 random attempts"
                    );
        return;
      }

    harness.check(port, s.getLocalPort(), "getLocalPort, opened");

    // Try another socket on the same port.
    try
      {
        ServerSocket s2 = new ServerSocket(port);
        harness.fail("BindException must be thrown");
      }
    catch (Exception ex)
      {
        harness.check(ex instanceof BindException, "Not a BindException: " +
                      ex
                     );
      }

    // The closed socket must hold its port number.
    try
      {
        s.close();
      }
    catch (IOException ex)
      {
        harness.fail("Exception while closing the socket" + ex);
      }

    harness.check(port, s.getLocalPort(),
                  "getLocalPort, closed, " + s.getLocalPort()
                 );

   // Try another socket on the same port.
   try
     {
       ServerSocket s2 = new ServerSocket(port);
       harness.check(port, s2.getLocalPort(), "Port mismatch");
     }
   catch (Exception ex)
     {
       harness.fail("Unable to reuse the port "+port);
     }



  }
}