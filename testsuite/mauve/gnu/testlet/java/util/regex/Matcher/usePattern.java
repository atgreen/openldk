// Tags: JDK1.5

// Copyright (C) 2011 Red Hat, Inc.

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

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class usePattern implements Testlet
{

  public void test (TestHarness harness)
  {
    Pattern pattern = Pattern.compile("^h");
    Pattern pattern2 = Pattern.compile(".*o$");
    Matcher matcher = pattern.matcher("hello");
    harness.check(matcher.lookingAt(), "Match ^h with original pattern");
    harness.check(!matcher.hitEnd(), "Matcher has not hit the end");
    try
    {
      matcher.usePattern(null);
      harness.check(false, "Failed to throw IllegalArgumentException");
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true, "Threw IllegalArgumentException");
    }
    harness.check(matcher.usePattern(pattern2) == matcher,
		  "usePattern returns same matcher");
    harness.check(matcher.lookingAt(), "Match .*o$ with new pattern");
    harness.check(matcher.hitEnd(), "Matcher has hit end");
  }
}
