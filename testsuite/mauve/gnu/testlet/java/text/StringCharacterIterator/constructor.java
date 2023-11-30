// constructor.java - Test StringCharacterIterator constructors.

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

package gnu.testlet.java.text.StringCharacterIterator;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.text.StringCharacterIterator;

public class constructor implements Testlet
{
  public void test (TestHarness harness)
    {
      StringCharacterIterator sci = null;
      harness.checkPoint ("failing constructors");

      try
	{
	  sci = new StringCharacterIterator (null);
	}
      catch (NullPointerException x)
	{
	}
      harness.check (sci, null);

      sci = null;
      try
	{
	  sci = new StringCharacterIterator (null, 0);
	}
      catch (NullPointerException x)
	{
	}
      harness.check (sci, null);

      sci = null;
      try
	{
	  sci = new StringCharacterIterator (null, 0, 0, 0);
	}
      catch (NullPointerException x)
	{
	}
      harness.check (sci, null);

      sci = null;
      try
	{
	  sci = new StringCharacterIterator (null, 0);
	}
      catch (NullPointerException x)
	{
	}
          harness.check (sci, null);

      sci = null;
      try
	{
	  sci = new StringCharacterIterator (null, 0);
	}
      catch (NullPointerException x)
	{
	}
      harness.check (sci, null);

      sci = null;
      try
	{
	  sci = new StringCharacterIterator ("ontology", -1);
	}
      catch (IllegalArgumentException x)
	{
	}
      harness.check (sci, null);

      sci = null;
      try
	{
	  sci = new StringCharacterIterator ("ontology", 9);
	}
      catch (IllegalArgumentException x)
	{
	}
      harness.check (sci, null);

      sci = null;
      try
	{
	  sci = new StringCharacterIterator ("ontology", -9, 0, 1);
	}
      catch (IllegalArgumentException x)
	{
	}
      harness.check (sci, null);

      sci = null;
      try
	{
	  sci = new StringCharacterIterator ("ontology", 0, -5, 1);
	}
      catch (IllegalArgumentException x)
	{
	}
      harness.check (sci, null);

      sci = null;
      try
	{
	  sci = new StringCharacterIterator ("ontology", 0, 1, -1);
	}
      catch (IllegalArgumentException x)
	{
	}
      harness.check (sci, null);

      sci = null;
      try
	{
	  sci = new StringCharacterIterator ("ontology", 5, 2, 3);
	}
      catch (IllegalArgumentException x)
	{
	}
      harness.check (sci, null);

      sci = null;
      try
	{
	  sci = new StringCharacterIterator ("ontology", 2, 5, 7);
	}
      catch (IllegalArgumentException x)
	{
	}
      harness.check (sci, null);

      // You could add a few more failure tests to be a bit more
      // complete, I suppose.  Feel free to add more to regression
      // test your implementation.

      harness.checkPoint ("successful constructors");

      sci = new StringCharacterIterator ("ontology");
      harness.check (sci.getBeginIndex (), 0);
      harness.check (sci.getEndIndex (), 8);
      harness.check (sci.getIndex (), 0);

      sci = new StringCharacterIterator ("ontology", 5);
      harness.check (sci.getBeginIndex (), 0);
      harness.check (sci.getEndIndex (), 8);
      harness.check (sci.getIndex (), 5);

      sci = new StringCharacterIterator ("ontology", 0, 7, 3);
      harness.check (sci.getBeginIndex (), 0);
      harness.check (sci.getEndIndex (), 7);
      harness.check (sci.getIndex (), 3);

      harness.checkPoint ("clone");
      StringCharacterIterator s2 = (StringCharacterIterator) sci.clone ();
      harness.check (s2.getBeginIndex (), 0);
      harness.check (s2.getEndIndex (), 7);
      harness.check (s2.getIndex (), 3);
      harness.check (sci.equals (s2));
    }
}
