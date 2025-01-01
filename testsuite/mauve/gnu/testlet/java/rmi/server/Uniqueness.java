/* Uniqueness.java -- The UID uniqueness : describe
   Copyright (C) 2006 Audrius Meskauskas
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

// Tags: 1.4

package gnu.testlet.java.rmi.server;

import java.rmi.server.UID;
import java.util.HashSet;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class Uniqueness implements Testlet
{

  static int complete = 0;
  
  static class TesterThread extends Thread
  {
    UID [] result;
    
    public void run()
    {
      result = new UID[200];
      for (int i = 0; i < result.length; i++)
        {
          result[i] = new UID();
        }
      synchronized (Uniqueness.class)
        {
          complete++;
        }
    }
  }

  public void test(TestHarness harness)
  {
    TesterThread[] tt = new TesterThread[20];
    for (int i = 0; i < tt.length; i++)
      {
        tt[i] = new TesterThread();
        tt[i].start();
      }

    // Wait till all complete:
    do
      {
        try
          {
            Thread.currentThread().sleep(200);
          }
        catch (InterruptedException e)
          {
          }
      }
    while (complete < tt.length);

    HashSet ids = new HashSet();

    for (int i = 0; i < tt.length; i++)
      {
        for (int j = 0; j < tt[i].result.length; j++)
          {
            UID id = tt[i].result[j];
            if (ids.contains(id))
              harness.fail("Duplicate ID " + id);
            else
              ids.add(id);
          }
      }
  }
}
