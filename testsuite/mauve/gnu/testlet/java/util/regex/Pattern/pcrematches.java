// Tags: JDK1.4

// Copyright (C) 2005 Ziga Mahkovec (ziga.mahkovec@klika.si)

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
import java.io.*;
import java.util.*;
import java.util.regex.*;

/**
 * Tests the java.util.regex regular expression engine.  The test cases are
 * adapted from PCRE (www.pcre.org).  Each test cases if formatted as:
 * <pre>
 * /regular expression 1/
 *     test string 1
 *     0: matching group 0
 *     1: matching group 1
 *     test string 2
 * No match
       test string 3
 *     ...
 *
 * /regular expression 2/
 * ...
 * </pre>
 */
public class pcrematches implements Testlet
{
  private TestHarness harness;

  /** Regex test suites from adapted from PCRE (http://www.pcre.org/license.txt). */
  private static final String[] TEST_SUITES = {"testdata1", /*"testdata2",*/ "testdata3"};

  /**
   * Regex test case (containing a single regular expression and a list of tests).
   */
  private static class RETestcase {
    String regex;
    List tests; // list of RETestcaseTest instances
  }

  /**
   * Regex test (containing a single text string and a list of resulting groups).
   */
  private static class RETestcaseTest {
    String text;
    List groups;
  }

  public void test (TestHarness harness)
  {
    this.harness = harness;
    try { 
      for (int i=0; i<TEST_SUITES.length; i++) {
        String suite = "gnu#testlet#java#util#regex#Pattern#" + TEST_SUITES[i];
        BufferedReader reader = new BufferedReader(harness.getResourceReader(suite));
        RETestcase tc = null;
        while ((tc = readTestcase(reader)) != null) {
          try
          {
            test(tc);
          }
          catch (PatternSyntaxException e)
          {
            harness.check(false, tc.regex);
          }
        }
      }
    }
    catch (gnu.testlet.ResourceNotFoundException _)
    {
        harness.check(false, "All tests failed (ResourceNotFoundException)");
    }
    catch (IOException _)
    {
        harness.check(false, "All tests failed (IOException)");
    }
  }
  
  private void test(RETestcase tc) throws PatternSyntaxException {
    String regex = tc.regex.substring(tc.regex.indexOf('/') + 1, 
                                      tc.regex.lastIndexOf('/'));
    String qual = tc.regex.substring(tc.regex.lastIndexOf('/') + 1);
    int flags = 0;
    if (qual.indexOf("i") != -1) {
      flags |= Pattern.CASE_INSENSITIVE;
    }
    if (qual.indexOf("m") != -1) {
      flags |= Pattern.MULTILINE;
    }
    Pattern pat = Pattern.compile(regex, flags);
    
    for (Iterator i=tc.tests.iterator(); i.hasNext(); ) {
      RETestcaseTest t = (RETestcaseTest)i.next();
      Matcher mat = pat.matcher(decode(t.text));
      if (mat.find()) {
        int groups = mat.groupCount();
        if (groups != t.groups.size() - 1) {
	  harness.debug("groups=" + groups + " expected=" + (t.groups.size() - 1));
          harness.check(false, regex);
          return;
        }
        boolean failed = false;
        for (int j=0; j<=groups; j++) {
          String g = decode((String)t.groups.get(j));
          String g2 = mat.group(j);
          if (!g.trim().equals(g2.trim())) {
	    harness.debug("j=" + j + " expected=" + g + " found=" + g2);
            harness.check(false, regex);
            return;
          }
        }
      } else if (!t.groups.isEmpty()) {
	harness.debug("match not found: regex=/" + regex + "/ text=\"" + t.text +"\"");
        harness.check(false, regex);
        return;
      }
    }
    harness.check(true, regex);
  }
  
  private static RETestcase readTestcase(BufferedReader reader) throws IOException {
    String line = reader.readLine();
    if (line == null)
      return null;
    line = line.trim();
    while (!line.startsWith("/") || line.lastIndexOf("/") == 0) {
      line = reader.readLine();
      if (line == null)
        return null;
    }
    RETestcase tc = new RETestcase();
    tc.regex = line;
    tc.tests = new ArrayList();
    while ((line = reader.readLine()) != null) {
      line = line.trim();
      if (line.length() == 0)
        break;
      RETestcaseTest test = new RETestcaseTest();
      test.text = line;
      test.groups = new ArrayList();
      reader.mark(8096);
      while ((line = reader.readLine()) != null) {
        if (line.length() == 0)
          break;
        else if (line.startsWith("    ")) {
          reader.reset();
          break;
        }
        if (line.equals("No match"))
          break;
        line = line.substring(line.indexOf(':') + 1);
        if (line.length() > 0)
          line = line.substring(1);
        test.groups.add(line);
        reader.mark(8096);
      }
      tc.tests.add(test);
      if (line == null || line.length() == 0)
        break;
    }
    return tc;
  }

  private static String decode(String s) {
    StringBuffer sb = new StringBuffer();
    int p = 0;
    int q = 0;
    while (true) {
      p = s.indexOf("\\u", q);
      if (p == -1) {
	sb.append(s.substring(q));
	break;
      }
      sb.append(s.substring(q, p));
      if (p + 6 <= s.length()) {
	String hex = s.substring(p+2, p+6);
	try {
	  int c = Integer.parseInt(hex, 16);
	  sb.append((char)c);
	}
	catch (NumberFormatException _) {
	  sb.append(s.substring(p, p+6));
	}
	q = p + 6;
      }
      else {
	sb.append(s.substring(p, p+2));
	q = p + 2;
      }
    }
    return sb.toString();
  }
}
