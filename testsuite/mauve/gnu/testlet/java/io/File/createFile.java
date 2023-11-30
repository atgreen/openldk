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


package gnu.testlet.java.io.File;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.File;
import java.io.IOException;
import java.util.TreeSet;

/**
 * This test checks if the File.createTempFile may return the same file
 * (wrong) if called from the parallel threads.
 */
public class createFile implements Testlet
{
  // How many files to ask?
  static int N_FILES = 100;
  
  // How many threads?
  static int N_THREADS = 10;
  
  String[][] returned;
  TestHarness harness;
  int completed;
  File tempDir;

  public void test(TestHarness a_harness)
  {
    try
      {
        harness = a_harness;
        tempDir = new File(harness.getTempDirectory());
        returned = new String[N_THREADS][];
        completed = 0;

        // Start threads.
        for (int thread = 0; thread < N_THREADS; thread++)
          {
            new tester(thread).start();
          }

        int n = 0;
        // Wait:
        while (completed < N_THREADS && (n++ < 600))
          {
            try
              {
                Thread.sleep(100);
              }
            catch (InterruptedException iex)
              {
              }
          }
        if (completed < N_THREADS)
          harness.fail("Failed in 60 seconds. Probably hangs.");

        // Check for shared occurences:
        TreeSet allReturned = new TreeSet();
        String x;
        for (int thread = 0; thread < N_THREADS; thread++)
          {
            for (int file = 0; file < N_FILES; file++)
              {
                x = (String) returned[thread][file];
                if (allReturned.contains(x))
                  {
                    harness.fail("Multiple occurence of " + x);
                    return;
                  }
                else
                  allReturned.add(x);
              }
          }
      }
    catch (Exception ex)
      {
        ex.printStackTrace();
      }
  }  
    
  class tester extends Thread
  {
    int thread_number;
    
    tester(int a_thread_number)
    {
      thread_number = a_thread_number;
      returned [thread_number] = new String[N_FILES];
    }
    
    public void run()
    {
      try
        {
          for (int file = 0; file < N_FILES; file++)
            {
              try
                {
                  File tempFile = File.createTempFile("mauve", "cft", tempDir);
                  String s = tempFile.getAbsolutePath();
                  returned[thread_number][file] = s;
                  tempFile.delete();
                }
              catch (IOException ioex)
                {
                  harness.fail("IOException " + ioex);
                  // Force termination.
                  completed = N_THREADS + 1;
                }
            }
        }
      finally
        {
          completed++;
        }
    }
  }
  
}

