// Tags: JDK1.0
// Depends: destroy_child

// Copyright (C) 2008 Christian Thalinger

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


package gnu.testlet.java.lang.Process;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

public class destroy implements Testlet
{
  public void test(TestHarness harness)
  {
    try
      {
        Process p = Runtime.getRuntime().exec(harness.getTestJava() + " gnu.testlet.java.lang.Process.destroy_child");
        BufferedReader in = new BufferedReader(new InputStreamReader(p.getInputStream()));

        String line = in.readLine();

        // Wait until the child process is up and running.
        if (line.equals("UP"))
          {
            harness.check(true);
            // Now destroy it.
            p.destroy();
          }
        else
          harness.check(false);

        // Wait until the child Process is going down.
        try
          {
            p.waitFor();
            harness.check(true);
          }
        catch (InterruptedException e)
          {
            harness.debug(e);
            harness.check(false);
          }
      }
    catch (IOException e)
      {
        harness.debug(e);
        harness.check(false);
      }	
  }
}
