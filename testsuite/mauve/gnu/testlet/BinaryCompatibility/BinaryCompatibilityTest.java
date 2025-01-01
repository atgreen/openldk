// Tags: JDK1.1

// Copyright (C) 2003 Red Hat, Inc.
// Written by aph@redhat.com
// Copyright (C) 2005 Mark J. Wielaard  <mark@klomp.org>

// A set of tests for Chapter 13 of the JLS, "Binary Compatibility".
// We run a script that compiles every test case twice, the second
// time with some classes replcaed by versions in the subdirectory
// called "altered".  The outputs of both runs should be the same: if
// not, we fail the test.

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

package gnu.testlet.BinaryCompatibility;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.*;

public class BinaryCompatibilityTest implements Testlet
{
  protected static TestHarness harness;

  public void testall()
  {
    try
      {
        String command;
        String path = (harness.getSourceDirectory() +
                       "/gnu/testlet/BinaryCompatibility/tests");
        char sep = harness.getSeparator().charAt(0);
        FileReader fr = new FileReader(path);
        BufferedReader tests = new BufferedReader(fr);
        while ((command = tests.readLine()) != null)
          {
            String srcdir = "/gnu/testlet/BinaryCompatibility";
            srcdir = srcdir.replace('/', sep);
            File dir = new File(harness.getSourceDirectory() + srcdir);
            harness.debug("Execing external command: " + command);
            Process p = Runtime.getRuntime().exec(command, null, dir);
            BufferedReader result =
              new BufferedReader(new InputStreamReader(p.getInputStream()));
            String s;
            while ((s = result.readLine()) != null)
              {
                harness.verbose(s);
                if (s.startsWith("PASS "))
                  harness.check(true, s.substring(s.indexOf("// ") + 3));
                else if (s.startsWith("FAIL "))
                  harness.check(false, s.substring(s.indexOf("// ") + 3));
              }
          }
      }
    catch (Throwable t)
      {
        harness.debug(t);
        harness.check(false);
      } 
  }

  public void test (TestHarness the_harness)
  {
    harness = the_harness;
    testall();
  }

}

