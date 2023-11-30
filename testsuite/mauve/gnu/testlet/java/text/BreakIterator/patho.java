// Test pathological behavior of BreakIterator.

// Copyright (c) 2004 Red Hat, Inc.
// Written by Tom Tromey <tromey@redhat.com>

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

public class patho implements Testlet
{
  public void check (String name, TestHarness harness, BreakIterator bi)
  {
    harness.checkPoint (name + " pathological cases");
    // This isn't mentioned in the spec, but at least one real program
    // (Eclipse) relies on an iterator not throwing an exception
    // before setText() is called.
    harness.check (bi.getText () != null);
  }

  public void test (TestHarness harness)
  {
    check ("word", harness, BreakIterator.getWordInstance());
    check ("character", harness, BreakIterator.getCharacterInstance());
    check ("line", harness, BreakIterator.getLineInstance());
    check ("word", harness, BreakIterator.getSentenceInstance());
  }
}
