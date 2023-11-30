// Tags: JDK1.2

// Copyright (C) 2003 Mark J. Wielaard

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

/**
 * Test whether overriding contains and the new 1.2 (Map interface)
 * containsValue work properly.
 */
public class HashContains extends Hashtable implements Testlet
{
  public void test (TestHarness harness)
  {
    Object a = new Object();
    Object b = new Object();
    Object c;

    c = put(a, b);
    harness.check(null, c);
    harness.check(!contains(a));
    harness.check(contains(b));

    harness.check(!containsValue(a));
    harness.check(containsValue(b));
  }

  // Override and call super.
  public boolean contains(Object value) {
    return super.contains(value);
  }

  // Override and call contains.
  public boolean containsValue(Object value) {
    return this.contains(value);
  }
}

