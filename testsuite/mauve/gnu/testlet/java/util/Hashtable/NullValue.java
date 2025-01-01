// Tags: JDK1.2

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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.util.Hashtable;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.util.Hashtable;
import java.util.HashMap;

/**
 * Check that Hashtable rejects null keys & values.
 */
public class NullValue extends Hashtable implements Testlet
{
  public void test (TestHarness harness)
  {
    HashMap m = new HashMap();
    m.put("a", "1");
    m.put("b", null);

    try
    {
      Hashtable h = new Hashtable(m);
      harness.fail("Should throw NullPointerException");
    }
    catch (NullPointerException x)
    {
      harness.check(true);
    }

    m = new HashMap();
    m.put("a", "1");
    m.put(null, "2");

    try
    {
      Hashtable h = new Hashtable(m);
      harness.fail("Should throw NullPointerException");
    }
    catch (NullPointerException x)
    {
      harness.check(true);
    }
  }
}

