/* testInvalidConstructorValues.java  
 Copyright (C) 2006 Tania Bento <tbento@redhat.com>
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

// Tags: 1.4


package gnu.testlet.java.awt.TextArea;

import java.awt.TextArea;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class testInvalidConstructorValues
    implements Testlet
{

  /**
   * Runs the test using the specified harness. 
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)
  {
    TextArea a = new TextArea(1, 1);
    harness.check(a.getRows(), 1);
    harness.check(a.getColumns(), 1);

    TextArea b = new TextArea(0, 0);
    harness.check(b.getRows(), 0);
    harness.check(b.getColumns(), 0);

    TextArea c = new TextArea(- 1, - 1);
    harness.check(c.getRows(), 0);
    harness.check(c.getColumns(), 0);

    TextArea d = new TextArea("", 1, 1, - 1);
    harness.check(d.getScrollbarVisibility(), d.SCROLLBARS_BOTH);

    TextArea e = new TextArea("", 1, 1, 0);
    harness.check(e.getScrollbarVisibility(), e.SCROLLBARS_BOTH);
    
    TextArea f = new TextArea("", 1, 1, 1);
    harness.check(f.getScrollbarVisibility(), f.SCROLLBARS_VERTICAL_ONLY);
    
    TextArea g = new TextArea("", 1, 1, 2);
    harness.check(g.getScrollbarVisibility(), g.SCROLLBARS_HORIZONTAL_ONLY);
    
    TextArea h = new TextArea("", 1, 1, 3);
    harness.check(h.getScrollbarVisibility(), h.SCROLLBARS_NONE);
    
    TextArea i = new TextArea("", 1, 1, 10);
    harness.check(i.getScrollbarVisibility(), i.SCROLLBARS_BOTH);

    TextArea j = new TextArea(null);
    harness.check(j.getText(), "");
  }

}
