// Tags: JDK1.5

// Copyright (C) 2008 Andrew John Hughes (gnu_andrew@member.fsf.org)

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

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Regions
  implements Testlet
{

  public void test (TestHarness harness)
  {
    String s = "food bar fool";
    Matcher m = Pattern.compile("^foo.").matcher(s);
    
    harness.check(m.lookingAt(), "Match foo at start of " + s);
    harness.check(m.group(), "food");

    m.reset();
    m.region(9, s.length());
    harness.check(m.lookingAt(), "Match foo at start of " + s.substring(9));
    harness.check(m.group(), "fool");

    m.reset();
    m.region(9, 10);
    harness.check(m.lookingAt(), false,
		  "Match foo at start of " + s.substring(9,10));
    
  }
}
