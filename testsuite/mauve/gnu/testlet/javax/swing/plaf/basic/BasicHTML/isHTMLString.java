/* isHTMLString.java -- some checks for the isHTMLString() method in the
       BasicHTML class.
   Copyright (C) 2006 David Gilbert <david.gilbert@object-refinery.com>
This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/

// Tags: JDK1.4

package gnu.testlet.javax.swing.plaf.basic.BasicHTML;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.plaf.basic.BasicHTML;

public class isHTMLString implements Testlet
{
  public void test(TestHarness harness)
  {
    harness.check(BasicHTML.isHTMLString(""), false);
    harness.check(BasicHTML.isHTMLString("A"), false);
    harness.check(BasicHTML.isHTMLString("<b>A</b>"), false);
    harness.check(BasicHTML.isHTMLString("<html>A</html>"), true);
    
    // the following special cases are interesting:
    harness.check(BasicHTML.isHTMLString(null), false);
    harness.check(BasicHTML.isHTMLString(" <html>A</html> "), false);
    harness.check(BasicHTML.isHTMLString("<html>A..."), true);
    harness.check(BasicHTML.isHTMLString("<HtMl>"), true);
  }
}
