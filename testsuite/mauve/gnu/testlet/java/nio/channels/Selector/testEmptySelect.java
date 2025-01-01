/* testEmptySelect.java -- test if selecting no channels succeeds.
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

package gnu.testlet.java.nio.channels.Selector;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.IOException;
import java.nio.channels.Selector;
import java.nio.channels.spi.SelectorProvider;

/**
 * @author csm
 *
 */
public class testEmptySelect
  implements Testlet
{

  /* (non-Javadoc)
   * @see gnu.testlet.Testlet#test(gnu.testlet.TestHarness)
   */
  public void test(TestHarness harness)
  {
    harness.checkPoint("openSelector");
    Selector sel = null;
    try
      {
        sel = SelectorProvider.provider().openSelector();
      }
    catch (IOException ioe)
      {
        harness.fail("openSelector");
        harness.debug(ioe);
        return;
      }
    
    harness.checkPoint("select");
    try
      {
        int ret = sel.select(100);
        harness.check(ret, 0);
      }
    catch (IOException ioe)
      {
        harness.fail("select");
        harness.debug(ioe);
      }
  }
}
