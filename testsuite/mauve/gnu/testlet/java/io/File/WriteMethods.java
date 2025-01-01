/* WriteMethods.java -- Test File write methods of classpath 1.6
   Copyright (C) 2007 Mario Torre <neugens@limasoftware.net>
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

//Tags: JDK1.6

package gnu.testlet.java.io.File;

import java.io.File;
import java.io.IOException;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Test File write methods of classpath 1.6
 * 
 * @author Mario Torre <neugens@limasoftware.net>
 */
public class WriteMethods
    implements Testlet
{
  /*
   * (non-Javadoc)
   * 
   * @see gnu.testlet.Testlet#test(gnu.testlet.TestHarness)
   */
  public void test(TestHarness harness)
  {
    String tmp = harness.getTempDirectory();
    File tmpfile = new File(tmp, "mauve-testfile");

    try
      {
        tmpfile.createNewFile();
      }
    catch (IOException e)
      {
        harness.fail("cannot create file for test.");
        return;
      }

    boolean write = tmpfile.canWrite();

    harness.check(write);

    write = tmpfile.setWritable(false);
    harness.check(write == true);

    write = tmpfile.canWrite();
    harness.check(write == false);

    tmpfile.delete();
  }
}
