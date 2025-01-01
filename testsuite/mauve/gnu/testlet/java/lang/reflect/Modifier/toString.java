// Tags: JDK1.1

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

package gnu.testlet.java.lang.reflect.Modifier;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.lang.reflect.Modifier;

public class toString implements Testlet
{
  public void test (TestHarness harness)
    {
      harness.check (Modifier.toString (Modifier.PUBLIC),
		     "public");
      harness.check (Modifier.toString (Modifier.PRIVATE),
		     "private");
      harness.check (Modifier.toString (Modifier.PROTECTED),
		     "protected");
      harness.check (Modifier.toString (Modifier.STATIC),
		     "static");
      harness.check (Modifier.toString (Modifier.FINAL),
		     "final");
      harness.check (Modifier.toString (Modifier.SYNCHRONIZED),
		     "synchronized");
      harness.check (Modifier.toString (Modifier.VOLATILE),
		     "volatile");
      harness.check (Modifier.toString (Modifier.TRANSIENT),
		     "transient");
      harness.check (Modifier.toString (Modifier.NATIVE),
		     "native");
      harness.check (Modifier.toString (Modifier.INTERFACE),
		     "interface");
      harness.check (Modifier.toString (Modifier.ABSTRACT),
		     "abstract");

      // Spot-check a few combinations.  Add more as desired.
      harness.check (Modifier.toString (Modifier.PRIVATE | Modifier.INTERFACE),
		     "private interface");
      harness.check (Modifier.toString (Modifier.ABSTRACT | Modifier.NATIVE),
		     "abstract native");
    }
}
