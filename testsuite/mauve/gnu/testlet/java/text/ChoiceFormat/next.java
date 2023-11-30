// Test nextDouble and previousDouble.

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

package gnu.testlet.java.text.ChoiceFormat;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.text.ChoiceFormat;

public class next implements Testlet
{
  public void test (TestHarness harness)
    {
      String oneplus = "1.0000000000000002";
      harness.check (ChoiceFormat.nextDouble (1.0) + "", oneplus);
      harness.check (ChoiceFormat.nextDouble (1.0, true) + "", oneplus);
    }
}
