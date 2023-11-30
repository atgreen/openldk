// Tags: JDK1.5

// Copyright (C) 2011 Pekka Enberg

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

package gnu.testlet.java.util.regex.Matcher;

import gnu.testlet.*;
import java.util.regex.*;

public class quoteReplacement implements Testlet
{
  private Matcher matcher;

  public void test (TestHarness harness)
  {
    try
    {
      Matcher.quoteReplacement(null);
      harness.check(false);
    }
    catch (NullPointerException e)
    {
      harness.check(true);
    }
    harness.check(Matcher.quoteReplacement("hello, world"), "hello, world");
    harness.check(Matcher.quoteReplacement("$"), "\\$");
    harness.check(Matcher.quoteReplacement("\\"), "\\\\");
  }
}
