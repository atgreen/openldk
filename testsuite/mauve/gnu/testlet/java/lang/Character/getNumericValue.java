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

package gnu.testlet.java.lang.Character;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class getNumericValue implements Testlet
{
  public void test (TestHarness harness)
    {
      harness.check (Character.getNumericValue('0'), 0);
      harness.check (Character.getNumericValue('\u0be8'), 2);
      harness.check (Character.getNumericValue('\u246d'), 14);
      harness.check (Character.getNumericValue('\u2182'), 10000);
      harness.check (Character.getNumericValue('\u00bd'), -2);
      harness.check (Character.getNumericValue('A'), 10);
      harness.check (Character.getNumericValue('\u2155'), -2);
      harness.check (Character.getNumericValue('\u221e'), -1);
    }
}
