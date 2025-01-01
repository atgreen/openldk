/* TestAllocateDirect.java -- test direct byte buffer operations.
   Copyright (C) 2006  Casey Marshall <csm@gnu.org>

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

// Tags: JDK1.4

package gnu.testlet.java.nio.ByteBuffer;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.nio.ByteBuffer;

/**
 * Test of various operations work for direct byte buffers.
 *
 * @author Casey Marshall (csm@gnu.org)
 */
public class TestAllocateDirect implements Testlet
{

  /* (non-Javadoc)
   * @see gnu.testlet.Testlet#test(gnu.testlet.TestHarness)
   */
  public void test(TestHarness harness)
  {
    ByteBuffer direct = ByteBuffer.allocateDirect(32);
    
    // Test PR 28608.
    harness.checkPoint("PR 28608");
    ByteBuffer duplicate = null;
    try
      {
        duplicate = direct.duplicate();
      }
    catch (Exception x)
      {
        harness.debug(x);
      }
    harness.check(duplicate != null);
    
    // Data should be zeroed out for new buffers.
    harness.checkPoint("initial data");
    boolean result = true;
    boolean sawData = false;
    while (direct.hasRemaining())
      {
        result = result && (direct.get() == 0);
        sawData = true;
      }
    harness.check(result && sawData);
  }
}
