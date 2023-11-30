// Simple tests for IdentityHashMap.

// Copyright (C) 2001 Red Hat, Inc.

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

// Tags: JDK1.4

package gnu.testlet.java.util.IdentityHashMap;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.util.*;

public class simple implements Testlet
{
  public void test (TestHarness harness)
  {
    // Create 1000 Integer objects.
    Integer[] is = new Integer[1000];
    Integer[] vs = new Integer[1000];
    IdentityHashMap map = new IdentityHashMap ();
    for (int i = 0; i < 1000; ++i)
      {
	is[i] = new Integer (i);
	vs[i] = new Integer (2000 + i);
	map.put (is[i], vs[i]);
      }

    harness.check (map.size (), 1000, "size");

    harness.checkPoint ("values");
    for (int i = 0; i < 1000; ++i)
      {
	Object k = map.get (is[i]);
	harness.check (k, vs[i]);
      }

    // Now remove some elements and recheck.
    harness.checkPoint ("remove");
    for (int i = 0; i < 1000; i += 2)
      {
    	Object v = map.remove (is[i]);
    	harness.check(v, vs[i]);
      }
    harness.checkPoint("post remove");
    for (int i = 1; i < 1000; i += 2)
      {
	Object k = map.get (is[i]);
	harness.check (k, vs[i]);
      }
  }
}
