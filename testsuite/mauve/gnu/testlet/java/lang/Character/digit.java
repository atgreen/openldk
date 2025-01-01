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

public class digit implements Testlet
{
  public void test (TestHarness harness)
    {
      harness.check (Character.digit ('9', 10), 9);
      harness.check (Character.digit ('9', 8), -1);
      harness.check (Character.digit ('A', 16), 10);
      harness.check (Character.digit ('a', 11), 10);
      harness.check (Character.digit ((char) ('Z' + 1), 36), -1);
      harness.check (Character.digit ('Z', 36), 35);
      harness.check (Character.digit ('\u0be7', 2), 1);
      harness.check (Character.digit ('\u0f27', 19), 7);
      harness.check (Character.digit ('0', 99), -1);
      harness.check (Character.digit ('0', -5), -1);
      harness.check (Character.digit ('\uffda', 10), -1);
      harness.check (Character.digit ('\u0000', 10), -1);
      harness.check (Character.digit ('A', 10), -1);
      harness.check (Character.digit ('y', 36), 34);
      harness.check (Character.digit ('\u2070', 36), -1);
    }
}
