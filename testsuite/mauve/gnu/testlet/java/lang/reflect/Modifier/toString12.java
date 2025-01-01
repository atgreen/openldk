// Tags: JDK1.2

// Copyright (C) 1999 Cygnus Solutions

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

public class toString12 implements Testlet
{
  public void test (TestHarness harness)
    {
      harness.check (Modifier.toString (Modifier.STRICT),
		     "strictfp");
      harness.check (Modifier.toString (Modifier.FINAL | Modifier.STRICT),
		     "final strictfp");

      int allFlags = Modifier.PUBLIC |
                     Modifier.PRIVATE |
                     Modifier.PROTECTED |
                     Modifier.STATIC |
                     Modifier.FINAL |
                     Modifier.SYNCHRONIZED |
                     Modifier.VOLATILE |
                     Modifier.TRANSIENT |
                     Modifier.NATIVE |
                     Modifier.INTERFACE |
                     Modifier.ABSTRACT |
                     Modifier.STRICT;
      // Note that order matters.
      harness.check (Modifier.toString (allFlags),
		     "public protected private abstract static final transient volatile synchronized native strictfp interface",
		     "check order of all flags");
    }
}
