// Tags: JDK1.4

// Copyright (C) 2004, 2005 Mark Wielaard

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
import java.util.Arrays;
import java.util.regex.*;

public class PatternSplit implements Testlet
{
  private TestHarness harness;

  public void test (TestHarness harness)
  {
    this.harness = harness;
    test("@", "test@example.com", new String[] { "test", "example.com" });
    test("\\.", "192.168.0.1", new String[] { "192", "168", "0", "1" });

    test(",", "a,b,c,d,e", new String[] { "a", "b", "c", "d", "e" });

    test("-", "a-", new String[] { "a", "" });
    test(";", ";b", new String[] { "", "b" });

    test(":", ":b:", new String[] { "", "b", "" });

    test(" ", " ", new String[] { "", "" });
    test("0", "00", new String[] { "", "", "" });

    test(",", "a,b,c,d,e", new String[] { "a", "b", "c", "d", "e" });

    test("\\w", "a,b,c,d,e", new String[] { "", ",", ",", ",", ",", "" });
    test("\\d+", "123,456,789", new String[] { "", ",", ",", "" });

    test("[^a-z]", "abc1defZghi", new String[] { "abc", "def", "ghi" });

    test("^[a-c]", "abc", new String[] { "", "bc" });
    test("[a-c]$", "abc", new String[] { "ab", "" });

    test("(?=[a-z])", "123abc", new String[] { "123", "a", "b", "c" });

    test(",", "a,,,b", new String[] { "a", "", "", "b" });

    // No match
    test("waku", "", new String[] { "" });
    test("waku", "wapu", new String[] { "wapu" });
    test("\\d+", "abc,def", new String[] { "abc,def" });

  }

  // Tests a pattern on a string with the given result
  // (result should include all trailing empty strings)
  void test(String pat, String str, String[] expected)
  {
    harness.checkPoint("test: " + pat);
    try
      {
	Pattern pattern = Pattern.compile(pat);
	String[] result = pattern.split(str, -1);
	harness.check(Arrays.equals(expected, result));

	result = pattern.split(str, Integer.MIN_VALUE);
	harness.check(Arrays.equals(expected, result));

	result = pattern.split(str);
	String[] result0 = pattern.split(str, 0);
	harness.check(Arrays.equals(result, result0));

	// Strip trailing space or just use str as result when we don't match.
	int total_len = expected.length;
	String[] expected0;
	if (pattern.matcher(str).find())
	  {
	    int trailing_empties = 0;
	    for (int i = 0; i < total_len; i++)
	      {
		if ("".equals(expected[i]))
		  trailing_empties++;
		else
		  trailing_empties = 0;
	      }
	    expected0 = new String[total_len - trailing_empties];
	    for (int i = 0; i < expected0.length; i++)
	      expected0[i] = expected[i];
	  }
	else
	  expected0 = new String[] { str };
	
	harness.check(Arrays.equals(expected0, result0));

	// A limit of one is lame. Either it doesn't match and the
	// result is the given string, or it matches zero (1 - 1) times
	// and the result is the whole given string (trailing part).
	String[] result1 = pattern.split(str, 1);
	harness.check(result1.length == 1 && str.equals(result1[0]));
	
	for (int i = 2; i <= total_len; i++)
	  {
	    result = pattern.split(str, i);
	    boolean equal = (result.length == i);
	    for (int j = 0; equal && j < i - 1; j++)
	      equal = (expected[j].equals(result[j]));
	    harness.check(equal);

	    // The tail should start with the first remaining element
	    harness.check(result.length > i - 1 &&
			  result[i - 1].startsWith(expected[i - 1]));
	    harness.check(result.length > i -1 &&
			  result[i - 1].endsWith(expected[total_len - 1]));
	  }

	result = pattern.split(str, total_len + 1);
	harness.check(Arrays.equals(expected, result));

	result = pattern.split(str, Integer.MAX_VALUE);
	harness.check(Arrays.equals(expected, result));
      }
    catch(PatternSyntaxException pse)
      {
	harness.debug(pse);
	harness.check(false);
      }
  }
}
