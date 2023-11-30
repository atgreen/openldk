// Test character iteration of BreakIterator.

// Copyright (c) 1999  Cygnus Solutions
// Written by Tom Tromey <tromey@cygnus.com>

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

// Tags: JDK1.1

package gnu.testlet.java.text.BreakIterator;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.text.BreakIterator;
import java.util.Locale;

public class chariter implements Testlet
{
  public void test (TestHarness harness)
  {
    // Just to be explicit: we're only testing the US locale here.
    Locale loc = Locale.US;
    Locale.setDefault (loc);

    String t1 = "How much time is left?  We don't know.";
    BreakIterator bi = BreakIterator.getCharacterInstance (loc);

    bi.setText (t1);
    int x = bi.current();

    harness.check (x, 0);
    int i = 0;
    while (x != BreakIterator.DONE && i <= t1.length() + 1)
      {
	x = bi.next();
	++i;
	harness.check (x, i <= t1.length() ? i : BreakIterator.DONE);
      }
  }
}
