// Tags: JDK1.4
// Uses: TestHelper

// Copyright (C) 2004 Mark Wielaard

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

package gnu.testlet.java.util.regex;

import gnu.testlet.*;
import java.util.regex.*;

/**
 * Tests character classes, negated character classes and class set
 * operations.
 */
public class CharacterClasses implements Testlet
{
  private TestHarness harness;
  private TestHelper helper;

  public void test (TestHarness harness)
  {
    this.harness = harness;
    this.helper = new TestHelper(harness);

    test("a", 'a', 'b');
    test("ab", 'b', 'c');
    test("ba", 'a', 'c');
    test("abcdefghijklmnopqrstuvwxyz123456789ABCDEFGHIJKLMNOPQRSTUVW",
	 'a', '*');
    test("abcdefghijklmnopqrstuvwxyz123456789ABCDEFGHIJKLMNOPQRSTUVW",
	 '1', '*');
    test("abcdefghijklmnopqrstuvwxyz123456789ABCDEFGHIJKLMNOPQRSTUVW",
	 'A', '*');

    test("a-z", 'a', 'A');
    test("A-Z", 'Z', 'z');
    test("a-zA-Z", 'a', '1');
    test("1-9a-zA-Z", 'A', ' ');

    test("-", '-', '*');
    test(".", '.', '^');
    test("*", '*', '$');
    test("$", '$', '*');
    // Sun's JDK does not accept the traditional expression "[[]"
    // maybe because it must support new expressions such as "[a-d[m-p]]"
    // and "[a-z&&[^m-p]]".  So the following test has been commented out.
    // test("[", '[', ']');
    test("\\[", '[', ']');
    test("\\]", ']', '[');

    helper.testNotPattern("[]");
    helper.testNotPattern("[^]");
  }

  void test(String range, char c, char nc)
  {
    // Positive range
    String pat = '[' + range + ']';
    harness.checkPoint("test: " + pat);
    try
      {
	Pattern pattern = Pattern.compile(pat);
	harness.check(pat, pattern.pattern());
	helper.testEmpty(pattern, false);
	helper.testMatchComplete(pattern, Character.toString(c));
      }
    catch(PatternSyntaxException pse)
      {
	harness.debug(pse);
	harness.check(false);
      }

    // Negative range
    pat = "[^" + range + ']';
    harness.checkPoint("test: " + pat);
    try
      {
	Pattern pattern = Pattern.compile(pat);
	harness.check(pat, pattern.pattern());
	helper.testEmpty(pattern, false);
	helper.testMatchComplete(pattern, Character.toString(nc));
      }
    catch(PatternSyntaxException pse)
      {
	harness.debug(pse);
	harness.check(false);
      }


  }
}
