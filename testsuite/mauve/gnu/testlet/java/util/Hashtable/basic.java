// Tags: JDK1.0

// Copyright (C) 1998 Cygnus Solutions

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

public class basic implements Testlet
{
  public void test (TestHarness harness)
    {
      // The toString tests have been commented out as they currently
      // print in reverse order from the std JDK.  Uncomment these if
      // we change our implementation to output in the same order.

      Hashtable hash = new Hashtable(13, 0.25F);

      harness.check (hash.toString(), "{}");
      harness.check (hash.isEmpty());

      hash.put(new Integer(1), "one");
      hash.put(new Integer(2), "two");
      hash.put(new Integer(3), "three");
      hash.put(new Integer(4), "four");
      hash.put(new Integer(5), "five");
      // Rehash should have just happened.
      hash.put(new Integer(6), "six");
      hash.put(new Integer(7), "seven");
      // Rehash should have just happened.
      hash.put(new Integer(8), "eight");
      hash.put(new Integer(9), "nine");
      hash.put(new Integer(10), "ten");
      hash.put(new Integer(11), "eleven");
      hash.put(new Integer(12), "twelve");
      hash.put(new Integer(13), "thirteen");
      hash.put(new Integer(14), "fourteen");
      // Rehash should have just happened.
      hash.put(new Integer(15), "fifteen");

      // harness.check (hash.toString());
      harness.check (! hash.isEmpty());
      harness.check (hash.size(), 15);

      Integer key = new Integer(13);
      String val = (String) hash.get(key);
      hash.put(key, val.toUpperCase());
      // harness.check (hash.toString());
      harness.check (hash.size(), 15);

      harness.check (hash.containsKey(key));
      harness.check (! hash.contains("thirteen"));
      harness.check (hash.contains("THIRTEEN"));

      hash.remove(key);
      // harness.check (hash.toString());
      harness.check (hash.size(), 14);

      Hashtable copy = (Hashtable) hash.clone();
      hash.clear();
      harness.check (hash.toString(), "{}");
      harness.check (hash.size(), 0);
      // harness.check (copy.toString());
      harness.check (copy.size(), 14);
    }
}

