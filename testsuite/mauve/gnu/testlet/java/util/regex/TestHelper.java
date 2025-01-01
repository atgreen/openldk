// Tags: not-a-test

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
import java.nio.CharBuffer;
import java.util.Arrays;
import java.util.regex.*;

/**
 * package private helper class for testing regular expressions.
 */

class TestHelper
{

  private final TestHarness harness;

  TestHelper(TestHarness harness)
  {
    this.harness = harness;
  }

  /**
   * Test wheter the given Pattern matches against a empty string or not.
   */
  void testEmpty(Pattern pattern, boolean matches)
  {
    // Try static matches method
    harness.checkPoint("testEmpty static Pattern.matches("
		       + pattern.pattern() + ")");
    try
      {
	harness.check(matches == Pattern.matches(pattern.pattern(), ""));
      }
    catch (PatternSyntaxException pse)
      {
	harness.debug(pse);
	harness.check(false);
      }

    // Try Matcher methods
    harness.checkPoint("testEmpty '" + pattern.pattern()
		       + "' flags " + pattern.flags());
    Matcher matcher = pattern.matcher("");
    testEmpty(matcher, matches);

    // Try reset Matcher with a CharBuffer wrapped around a empty StringBuffer.
    harness.checkPoint("testEmpty '" + pattern.pattern()
		       + "' flags " + pattern.flags()
		       + " (reset)");
    matcher.reset(CharBuffer.wrap(new StringBuffer()));
    testEmpty(matcher, matches);

    // Try split methods
    harness.checkPoint("testEmpty split '" + pattern.pattern()
                       + "' flags " + pattern.flags());
    String[] expected = new String[] { "" };
    harness.check(Arrays.equals(expected, pattern.split("")));
    harness.check(Arrays.equals(expected, pattern.split("", 1)));
  }

  void testEmpty(Matcher matcher, boolean matches)
  {
    harness.check(matches == matcher.matches());
    harness.check(matches == matcher.lookingAt());
    harness.check(!matcher.find());
    matcher.reset();
    harness.check(matches == matcher.find());
    if (matches)
    {
      harness.check(matcher.start() == 0);
      harness.check(matcher.end() == 0);
      harness.check("", matcher.group());

      int groups = matcher.groupCount();
      harness.check(groups >= 0);
      for (int i = 0; i < groups; i++)
	{
	  harness.check(matcher.start(i) == 0);
	  harness.check(matcher.end(i) == 0);
	  harness.check("", matcher.group(0));
	}
      
      harness.check("cat", matcher.replaceAll("cat"));
      harness.check("dog", matcher.replaceFirst("dog"));
      
      matcher.reset();
      StringBuffer sb = new StringBuffer();
      while (matcher.find())
	matcher.appendReplacement(sb, "blue");
      matcher.appendTail(sb);
      harness.check("blue", sb.toString());
    }
   }

  /**
   * Test wheter the given Pattern matches against the complete given string.
   */
  void testMatchComplete(Pattern pattern, String string)
  {
    // Try static matches method
    harness.checkPoint("testMatchComplete static Pattern.matches("
		       + pattern.pattern() + ")");
    try
      {
	harness.check(Pattern.matches(pattern.pattern(), string));
      }
    catch (PatternSyntaxException pse)
      {
	harness.debug(pse);
	harness.check(false);
      }

    // Try Matcher methods
    harness.checkPoint("testMatchComplete '" + pattern.pattern()
		       + "' flags " + pattern.flags());
    Matcher matcher = pattern.matcher(string);
    testMatchComplete(matcher, string);

    // Try reset Matcher with a CharBuffer wrapped around new StringBuffer.
    harness.checkPoint("testComplete '" + pattern.pattern()
		       + "' flags " + pattern.flags()
		       + " (reset)");
    matcher.reset(CharBuffer.wrap(new StringBuffer(string)));
    testMatchComplete(matcher, string);

    // Try split methods
    harness.checkPoint("testComplete split '" + pattern.pattern()
                       + "' flags " + pattern.flags());
    String[] expected = new String[] { };
    String[] split = pattern.split(string);
    harness.check(Arrays.equals(expected, split));
    split = pattern.split(string, 0);
    harness.check(Arrays.equals(expected, split));

    expected = new String[] { string };
    split = pattern.split(string, 1);
    harness.check(Arrays.equals(expected, split));

    expected = new String[] { "", "" };
    split = pattern.split(string, -1);
    harness.check(Arrays.equals(expected, split));

    split = pattern.split(string, 2);
    harness.check(Arrays.equals(expected, split));

    split = pattern.split(string, 3);
    harness.check(Arrays.equals(expected, split));
  }

  void testMatchComplete(Matcher matcher, String string)
  {
    try
      {
	harness.check(matcher.matches());
	harness.check(matcher.lookingAt());
	harness.check(!matcher.find());
	matcher.reset();
	harness.check(matcher.find());
    
	// We should be able to add extra parens and have group 1 for certain.
	Pattern pattern = Pattern.compile("("
					  + matcher.pattern().pattern()
					  + ")");
	matcher = pattern.matcher(string);
	harness.check(matcher.matches());
	harness.check(matcher.lookingAt());
	harness.check(!matcher.find());
	matcher.reset();
	harness.check(matcher.find());

	harness.check(matcher.start() == 0);
	harness.check(matcher.end() == string.length());
	harness.check(string, matcher.group());

	int groups = matcher.groupCount();
	harness.check(groups >= 1);
	harness.check(matcher.start(0) == 0);
	harness.check(matcher.start(1) == 0);
	harness.check(matcher.end(0) == string.length());
	harness.check(matcher.end(1) == string.length());
	harness.check(string, matcher.group(0));
	harness.check(string, matcher.group(1));

	harness.check("cat", matcher.replaceAll("cat"));
	harness.check("dog", matcher.replaceFirst("dog"));
	
	harness.check("cat" + string + "dog",
		      matcher.replaceAll("cat$0dog"));
	harness.check("dog" + string + "cat",
		      matcher.replaceFirst("dog$1cat"));

	matcher.reset();
	StringBuffer sb = new StringBuffer();
	while (matcher.find())
	  matcher.appendReplacement(sb, "blue");
	matcher.appendTail(sb);
	harness.check("blue", sb.toString());
      }
    catch (PatternSyntaxException pse)
      {
        harness.debug(pse);
        harness.check(false);
      }
    catch(IllegalStateException ise)
      {
	harness.debug(ise);
	harness.check(false);
      }
  }

  void testNotPattern(String pat)
  {
    harness.checkPoint("testNotPattern: " + pat);
    boolean exception = false;
    try
      {
	Pattern pattern = Pattern.compile(pat);
      }
    catch(PatternSyntaxException pse)
      {
	exception = true;
      }
    harness.check(exception);
  }
}
