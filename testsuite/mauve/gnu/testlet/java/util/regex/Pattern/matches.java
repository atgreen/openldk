// Tags: JDK1.4

// Copyright (C) 2005 Mark J. Wielaard (mark@klomp.org)

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

package gnu.testlet.java.util.regex.Pattern;

import gnu.testlet.*;
import java.util.regex.*;

public class matches implements Testlet
{
  private TestHarness harness;

  /**
   * Tests whether things match completely (not just partially) as
   * suggested by Timo Juhani Lindfors (timo.lindfors@iki.fi).
   */
  public void test (TestHarness harness)
  {
    try
      {
	harness.check(!Pattern.matches("b", "ab"));
	harness.check(Pattern.matches("ab", "ab"));
	harness.check(!Pattern.matches("abab", "abababab"));
	harness.check(Pattern.matches("abababab", "abababab"));

	harness.check(!Pattern.matches("(\\w,)+", "a,b,c,d,e"));
	harness.check(Pattern.matches("(\\w,)+", "a,b,c,d,e,"));

	harness.check(!Pattern.matches("\\d+", "123,456"));
	harness.check(Pattern.matches("\\d+,\\d+", "123,456"));
	harness.check(!Pattern.matches("\\d+,\\d+", "123,456,789"));
	harness.check(Pattern.matches("\\d+,\\d+,\\d+", "123,456,789"));
	harness.check(!Pattern.matches("\\d+,\\d+,\\d+,", "123,456,789"));
	harness.check(Pattern.matches("\\d+,\\d+,\\d+,", "123,456,789,"));

	harness.check(!Pattern.matches("[a-c]", "abc"));
	harness.check(!Pattern.matches("[a-c][a-c]", "abc"));
	harness.check(Pattern.matches("[a-c][a-c][a-c]", "abc"));
	harness.check(!Pattern.matches("[a-c][a-c][a-c][a-c]", "abc"));

	harness.check(!Pattern.matches("[a-z]*", "abc1defZghi"));
	harness.check(!Pattern.matches("([a-z]|\\d)*", "abc1defZghi"));
	harness.check(Pattern.matches("([a-z]|\\d|[A-Z])*", "abc1defZghi"));
	harness.check(!Pattern.matches("([a-z]|\\d|[A-Z])*", ",abc1defZghi"));
	harness.check(!Pattern.matches("([a-z]|\\d|[A-Z])*", "abc1defZghi,"));
	harness.check(!Pattern.matches("([a-z]|\\d|[A-Z])*", ",abc1defZghi,"));

	harness.check(Pattern.matches("()*", ""));
	harness.check(!Pattern.matches("()*", "x"));
	harness.check(Pattern.matches("(b*c*)*", ""));
	harness.check(Pattern.matches("(b*c*)*", "cbbcbbb"));
	harness.check(Pattern.matches("(b*c*)+", ""));
	harness.check(Pattern.matches("(b*c*)+", "cbbcbbb"));
	harness.check(Pattern.matches("(b*c*){3,}", "cbbcbbb"));
	harness.check(Pattern.matches("(b*c*){10,}", "cbbcbbb"));
      }
    catch(PatternSyntaxException pse)
      {
	harness.debug(pse);
	harness.check(false, pse.toString());
      }
  }
}
