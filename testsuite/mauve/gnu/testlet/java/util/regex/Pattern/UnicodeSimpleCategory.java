// Tags: JDK1.4

// Copyright (C) 2006 Mark J. Wielaard (mark@klomp.org)

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

public class UnicodeSimpleCategory implements Testlet
{
  private TestHarness harness;

  /**
   * Tests simple unicode categories are correctly matched.
   */
  public void test (TestHarness harness)
  {
    try
      {
	String r;
	String t;
	String t3;
	Matcher m;
	
	harness.checkPoint("L");
	r = "(\\p{L}+)(\\p{Lu})(\\p{Ll})(\\p{Lt})(\\p{Lm})(\\p{Lo})(\\p{L}+)";
	t = "Aa\u01C5\u02B0\u05D0";
	t3 = t + t + t;
	m = Pattern.compile(r).matcher(t3);
	harness.check(m.find());
	harness.check(m.group(1), t);
	harness.check(m.group(2), "A");
	harness.check(m.group(3), "a");
	harness.check(m.group(4), "\u01C5");
	harness.check(m.group(5), "\u02B0");
	harness.check(m.group(6), "\u05D0");
	harness.check(m.group(7), t);

	harness.checkPoint("M");
	r = "(\\p{M}+)(\\p{Mn})(\\p{Mc})(\\p{Me})(\\p{M}+)";
	t = "\u064B\u0903\u20DD";
	t3 = t + t + t;
	m = Pattern.compile(r).matcher(t3);
	harness.check(m.find());
        harness.check(m.group(1), t);
	harness.check(m.group(2), "\u064B");
	harness.check(m.group(3), "\u0903");
	harness.check(m.group(4), "\u20DD");
	harness.check(m.group(5), t);

	harness.checkPoint("N");
	r = "(\\p{N}+)(\\p{Nd})(\\p{Nl})(\\p{No})(\\p{N}+)";
	t = "0\u2160\u3289";
	t3 = t + t + t;
	m = Pattern.compile(r).matcher(t3);
	harness.check(m.find());
        harness.check(m.group(1), t);
	harness.check(m.group(2), "0");
	harness.check(m.group(3), "\u2160");
	harness.check(m.group(4), "\u3289");
	harness.check(m.group(5), t);

	harness.checkPoint("S");
	r = "(\\p{S}+)(\\p{Sm})(\\p{Sc})(\\p{Sk})(\\p{So})(\\p{S}+)";
	t = "+\u00A5\u00B8\u0482";
	t3 = t + t + t;
	m = Pattern.compile(r).matcher(t3);
	harness.check(m.find());
        harness.check(m.group(1), t);
	harness.check(m.group(2), "+");
	harness.check(m.group(3), "\u00A5");
	harness.check(m.group(4), "\u00B8");
	harness.check(m.group(5), "\u0482");
	harness.check(m.group(6), t);

	harness.checkPoint("P");
	r = "(\\p{P}+)(\\p{Pc})(\\p{Pd})(\\p{Ps})(\\p{Pe})(\\p{Pi})(\\p{Pf})"
	  + "(\\p{Po})(\\p{P}+)";
	t = "_-()\u00AB\u00BB!";
	t3 = t + t + t;
	m = Pattern.compile(r).matcher(t3);
	harness.check(m.find());
        harness.check(m.group(1), t);
	harness.check(m.group(2), "_");
	harness.check(m.group(3), "-");
	harness.check(m.group(4), "(");
	harness.check(m.group(5), ")");
	harness.check(m.group(6), "\u00AB");
	harness.check(m.group(7), "\u00BB");
	harness.check(m.group(8), "!");
	harness.check(m.group(9), t);

	harness.checkPoint("Z");
	r = "(\\p{Z}+)(\\p{Zs})(\\p{Zl})(\\p{Zp})(\\p{Z}+)";
	t = " \u2028\u2029";
	t3 = t + t + t;
	m = Pattern.compile(r).matcher(t3);
	harness.check(m.find());
        harness.check(m.group(1), t);
	harness.check(m.group(2), " ");
	harness.check(m.group(3), "\u2028");
	harness.check(m.group(4), "\u2029");
	harness.check(m.group(5), t);

	// Don't include unassigned Cn since we aren't sure about those.
	harness.checkPoint("C");
	r = "(\\p{C}+)(\\p{Cc})(\\p{Cf})(\\p{Cs})(\\p{Co})(\\p{C}+)";
	t = "\t\u070F\uD800\uE000";
	t3 = t + t + t;
	m = Pattern.compile(r).matcher(t3);
	harness.check(m.find());
	harness.check(m.group(1), t);
	harness.check(m.group(2), "\t");
	harness.check(m.group(3), "\u070F");
	harness.check(m.group(4), "\uD800");
	harness.check(m.group(5), "\uE000");
	harness.check(m.group(6), t);
      }
    catch(PatternSyntaxException pse)
      {
	harness.debug(pse);
	harness.check(false, pse.toString());
      }
  }
}
