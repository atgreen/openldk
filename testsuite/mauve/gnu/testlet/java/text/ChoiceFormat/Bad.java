// Test handling of bad ChoiceFormats.

// Copyright (c) 2009 Red Hat
// Written by Andrew John Hughes (ahughes@redhat.com)

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

package gnu.testlet.java.text.ChoiceFormat;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.text.ChoiceFormat;

public class Bad
  implements Testlet
{
  public void test (TestHarness h)
  {
    try
      {
	ChoiceFormat f = new ChoiceFormat("0#zero|1#one|1>many");
	h.fail("Failed to catch bad limit 1>");
      }
    catch (IllegalArgumentException e)
      {
	h.check(true);
      }
    try
      {
	ChoiceFormat f = new ChoiceFormat("0#zero|1#one|1>many|");
	h.fail("Failed to catch bad limit 1> with trailing |");
      }
    catch (IllegalArgumentException e)
      {
	h.check(true);
      }
    try
      {
	ChoiceFormat f = new ChoiceFormat("0#zero|1#one|1<many|");
	h.fail("Failed to catch trailing |");
      }
    catch (IllegalArgumentException e)
      {
	h.check(true);
      }
    try
      {
	ChoiceFormat f = new ChoiceFormat("0#zero|1#one|1>many|1<many");
	h.fail("Failed to catch enclosed bad limit");
      }
    catch (IllegalArgumentException e)
      {
	h.check(true);
      }
  }
}

