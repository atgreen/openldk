// Tags: JDK1.4

// Copyright (c) Audrius Meskauskas <audriusa@bluewin.ch>

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


package gnu.testlet.java.net.ServerSocket;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.SocketException;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Test if the ServerSocket.accept returns (throws SocketException) after closing 
 * that socket from another thread. The behavior is documented in java API 
 * and confirmed for Sun and IBM implementations.
 */
public class ReturnOnClose implements Testlet
{
  ServerSocket socket;
  Throwable exception;

  public synchronized void test(final TestHarness harness)
  {
    Thread t = new Thread()
    {
      public void run()
      {
        boolean opened = false;

        for (int i = 1; i < 100 && !opened; i++)
          {
            int port = 1000 + (int) (Math.random() * 2000);
            try
              {
                socket = new ServerSocket(port);
                opened = true;
              }
            catch (Exception ex)
              {
                // next try
              }
          }

        // This should suspend the thread.
        try
          {
            socket.accept();
          }
        catch (IOException ex)
          {
            exception = ex;
          }
        socket = null;
      }
    };
    t.start();

    // Wait at most for 10 seconds to open a socket.
    long s = System.currentTimeMillis();
    while (socket == null && System.currentTimeMillis() - s < 10000)
      {
        try
          {
            Thread.sleep(100);
          }
        catch (InterruptedException e)
          {
          }
      }
    harness.check(socket != null, " Socket must be opened");

    try
      {
        socket.close();
        t.interrupt();
      }
    catch (IOException e)
      {
      }

    // Wait at most for 3 seconds for a socket to close.
    s = System.currentTimeMillis();
    while (socket != null && System.currentTimeMillis() - s < 3000)
      {
        try
          {
            Thread.sleep(100);
          }
        catch (InterruptedException e)
          {
          }
      }
    harness.check(socket == null, 
      " Socket thread must be resumed. POA.testForwarding will also fail.");
    harness.check(exception instanceof SocketException,"Must be SocketException");
  }

}
