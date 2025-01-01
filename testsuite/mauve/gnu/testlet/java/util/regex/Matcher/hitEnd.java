// Tags: JDK1.5

// Copyright (C) 2006 Ito Kazumitsu (kaz@maczuka.gcd.org)

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

public class hitEnd implements Testlet
{
  private TestHarness harness;
  // There seems to be some bug in gnu.java.util.regex and
  // Pattern object cannot be reused for more than one matchers.
  // So we compile the pattern string every time.
  private Pattern pattern;
  private String patternStr;
  private Matcher matcher;

  public void test (TestHarness harness)
  {
    this.harness = harness;
    try
      {
	patternStr = "abcd";
        testFind("xyzabcd", false);
        testFind("XYZabcdxyz", false);
        testFind("xyzabc", true);
        testFind("xyzxyz", true);
        testMatches("abcd", false);
	testMatches("abc", true);
        testMatches("abcdxyz", false);
	testMatches("xyzabcd", false);
	testMatches("xyz", false);
	testLookingAt("abcd", false);
	testLookingAt("abcdxyz", false);
	testLookingAt("abc", true);
	testLookingAt("xyzabcd", false);

	patternStr = "abcd$";
        testFind("xyzabcd", true);
        testFind("XYZabcdxyz", true);
        testFind("xyzabc", true);
        testFind("xyzxyz", true);
        testMatches("abcd", true);
	testMatches("abc", true);
        testMatches("abcdxyz", false);
	testMatches("xyzabcd", false);
	testMatches("xyz", false);
	testLookingAt("abcd", true);
	testLookingAt("abcdxyz", false);
	testLookingAt("abc", true);
	testLookingAt("xyzabcd", false);

	patternStr = "a+b";
	testFind("xyzaaab", false);
	testFind("xyzaaabb", false);
	testFind("xyzaaa", true);
	testFind("xyzxyz", true);
	testMatches("aaab", false);
	testMatches("aaaa", true);
	testMatches("aaabx", false);
	testMatches("xaaab", false);
	testLookingAt("aaab", false);
	testLookingAt("aaaa", true);
	testLookingAt("aaabxyz", false);
	testLookingAt("xyzxyz", false);
	
	patternStr = "(?:a+b)|(?:aa)";
	testFind("xyzaaab", false);
	testFind("xyzaa", true);
	testFind("xyzaaa", true);
	testFind("xyzaax", false);
	testMatches("aaab", false);
	testMatches("aaaa", true);
	testMatches("aa", true);
	testLookingAt("aaab", false);
	testLookingAt("aaaa", true);
	testLookingAt("aa", true);
	testLookingAt("aax", false);

	patternStr = "(?:aa)|(?:a+b)";
	testFind("xyzaaab", false);
	testFind("xyzaa", false);
	testFind("xyzaaa", false);
	testFind("xyzaax", false);
	testMatches("aaab", false);
	testMatches("aaaa", true);
	testMatches("aa", false);
	testLookingAt("aaab", false);
	testLookingAt("aaaa", false);
	testLookingAt("aa", false);
	testLookingAt("aax", false);
      }
    catch(PatternSyntaxException pse)
      {
	harness.debug(pse);
	harness.check(false, pse.toString());
      }
  }

  private void testFind(String s, boolean expected)
  {
    pattern = Pattern.compile(patternStr);
    matcher = pattern.matcher(s);
    matcher.find();
    boolean result = matcher.hitEnd();
    if (result != expected) debugMsg(s, "find", expected, result);
    harness.check(result == expected);
  }

  private void testMatches(String s, boolean expected)
  {
    pattern = Pattern.compile(patternStr);
    matcher = pattern.matcher(s);
    matcher.matches();
    boolean result = matcher.hitEnd();
    if (result != expected) debugMsg(s, "matches", expected, result);
    harness.check(result == expected);
  }

  private void testLookingAt(String s, boolean expected)
  {
    pattern = Pattern.compile(patternStr);
    matcher = pattern.matcher(s);
    matcher.lookingAt();
    boolean result = matcher.hitEnd();
    if (result != expected) debugMsg(s, "lookingAt", expected, result);
    harness.check(result == expected);
  }

  private void debugMsg(String s, String method, boolean expected, boolean result)
  {
    harness.debug("pattern=" + pattern.pattern() + " input=" + s + " method=" + method + " matcher=" + matcher + " expected=" + expected + " hitEnd=" + result);
  }

}
