// iter.java - Test StringCharacterIterator iteration.

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

// Note that we test 1.2 semantics, not 1.1 semantics.
// Tags: JDK1.2

package gnu.testlet.java.text.StringCharacterIterator;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.text.CharacterIterator;
import java.text.StringCharacterIterator;

public class iter implements Testlet
{
  public void test (TestHarness harness)
    {
      harness.checkPoint ("spot checks");

      String recherche = "recherche";
      StringCharacterIterator sci = new StringCharacterIterator (recherche);

      harness.check (sci.getIndex (), 0);
      harness.check (sci.current (), 'r');
      harness.check (sci.getIndex (), 0);

      harness.check (sci.previous (), CharacterIterator.DONE);
      harness.check (sci.getIndex (), 0);

      int idx = recherche.length () - 1;
      harness.check (sci.setIndex (idx), 'e');
      harness.check (sci.getIndex (), idx);
      harness.check (sci.next (), CharacterIterator.DONE);
      harness.check (sci.current (), CharacterIterator.DONE);
      harness.check (sci.getIndex (), recherche.length ());

      harness.check (sci.first (), 'r');
      harness.check (sci.getIndex (), 0);

      harness.checkPoint ("full iteration");
      for (int i = 0; i < recherche.length () - 1; ++i)
	harness.check (sci.next (), recherche.charAt (i + 1));
      harness.check (sci.next (), CharacterIterator.DONE);
      harness.check (sci.setIndex (sci.getEndIndex ()),
		     CharacterIterator.DONE);

      sci = new StringCharacterIterator ("");
      // 1.2, not 1.1.
      harness.check (sci.current (), CharacterIterator.DONE);
      harness.check (sci.previous (), CharacterIterator.DONE);
      harness.check (sci.next (), CharacterIterator.DONE);
    }
}
