/* constructors.java -- some checks for the constructors in the TextField
       class.
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

// Tags: JDK1.1

package gnu.testlet.java.awt.TextArea;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.TextArea;

public class constructors implements Testlet
{
  public void test(TestHarness harness)
  {
      testConstructor1(harness);
      testConstructor2(harness);
      testConstructor3(harness);
      testConstructor4(harness);
      testConstructor5(harness);
  }
  
  public void testConstructor1(TestHarness harness)
  {
    harness.checkPoint("()");
    TextArea ta = new TextArea();
    harness.check(ta.getText(), "");
    harness.check(ta.isEditable());
    harness.check(ta.getColumns(), 0);
    harness.check(ta.getScrollbarVisibility(), TextArea.SCROLLBARS_BOTH);
  }
  
  public void testConstructor2(TestHarness harness)
  {
    harness.checkPoint("(int, int)");
    TextArea ta = new TextArea(3, 7);
    harness.check(ta.getText(), "");
    harness.check(ta.isEditable());
    harness.check(ta.getColumns(), 7);
    harness.check(ta.getRows(), 3);
    harness.check(ta.getScrollbarVisibility(), TextArea.SCROLLBARS_BOTH);
    
    // try negative rows
    ta = new TextArea(-1, 7);
    harness.check(ta.getText(), "");
    harness.check(ta.isEditable());
    harness.check(ta.getRows(), 0);
    harness.check(ta.getColumns(), 7);
    harness.check(ta.getScrollbarVisibility(), TextArea.SCROLLBARS_BOTH);

    // try negative columns
    ta = new TextArea(3, -7);
    harness.check(ta.getText(), "");
    harness.check(ta.isEditable());
    harness.check(ta.getRows(), 3);
    harness.check(ta.getColumns(), 0);  
    harness.check(ta.getScrollbarVisibility(), TextArea.SCROLLBARS_BOTH);
  }
  
  public void testConstructor3(TestHarness harness)
  {
    harness.checkPoint("(String)");
    TextArea ta = new TextArea("ABC");
    harness.check(ta.getText(), "ABC");
    harness.check(ta.isEditable());
    harness.check(ta.getRows(), 0);
    harness.check(ta.getColumns(), 0);
    harness.check(ta.getScrollbarVisibility(), TextArea.SCROLLBARS_BOTH);
    
    // try null
    ta = new TextArea(null);
    harness.check(ta.getText(), "");
    harness.check(ta.isEditable());
    harness.check(ta.getRows(), 0);
    harness.check(ta.getColumns(), 0);
    harness.check(ta.getScrollbarVisibility(), TextArea.SCROLLBARS_BOTH);
  }
  
  public void testConstructor4(TestHarness harness)
  {
    harness.checkPoint("(String, int, int)");
    TextArea ta = new TextArea("ABC", 3, 7);
    harness.check(ta.getText(), "ABC");
    harness.check(ta.isEditable());
    harness.check(ta.getRows(), 3);
    harness.check(ta.getColumns(), 7);
    harness.check(ta.getScrollbarVisibility(), TextArea.SCROLLBARS_BOTH);
    
    // try null
    ta = new TextArea(null, 3, 7);
    harness.check(ta.getText(), "");
    harness.check(ta.isEditable());
    harness.check(ta.getRows(), 3);
    harness.check(ta.getColumns(), 7);
    harness.check(ta.getScrollbarVisibility(), TextArea.SCROLLBARS_BOTH);
    
    // try negative rows
    ta = new TextArea("ABC", -3, 7);
    harness.check(ta.getText(), "ABC");
    harness.check(ta.isEditable());
    harness.check(ta.getRows(), 0);
    harness.check(ta.getColumns(), 7);
    harness.check(ta.getScrollbarVisibility(), TextArea.SCROLLBARS_BOTH);

    // try negative columns
    ta = new TextArea("ABC", 3, -7);
    harness.check(ta.getText(), "ABC");
    harness.check(ta.isEditable());
    harness.check(ta.getRows(), 3);
    harness.check(ta.getColumns(), 0);
    harness.check(ta.getScrollbarVisibility(), TextArea.SCROLLBARS_BOTH);
  }

  public void testConstructor5(TestHarness harness)
  {
    harness.checkPoint("(String, int, int, int)");
    TextArea ta = new TextArea("ABC", 3, 7, TextArea.SCROLLBARS_VERTICAL_ONLY);
    harness.check(ta.getText(), "ABC");
    harness.check(ta.isEditable());
    harness.check(ta.getRows(), 3);
    harness.check(ta.getColumns(), 7);
    harness.check(ta.getScrollbarVisibility(), TextArea.SCROLLBARS_VERTICAL_ONLY);
    
    // try null
    ta = new TextArea(null, 3, 7, TextArea.SCROLLBARS_VERTICAL_ONLY);
    harness.check(ta.getText(), "");
    harness.check(ta.isEditable());
    harness.check(ta.getRows(), 3);
    harness.check(ta.getColumns(), 7);
    harness.check(ta.getScrollbarVisibility(), TextArea.SCROLLBARS_VERTICAL_ONLY);
    
    // try negative rows
    ta = new TextArea("ABC", -3, 7, TextArea.SCROLLBARS_VERTICAL_ONLY);
    harness.check(ta.getText(), "ABC");
    harness.check(ta.isEditable());
    harness.check(ta.getRows(), 0);
    harness.check(ta.getColumns(), 7);
    harness.check(ta.getScrollbarVisibility(), TextArea.SCROLLBARS_VERTICAL_ONLY);

    // try negative columns
    ta = new TextArea("ABC", 3, -7, TextArea.SCROLLBARS_VERTICAL_ONLY);
    harness.check(ta.getText(), "ABC");
    harness.check(ta.isEditable());
    harness.check(ta.getRows(), 3);
    harness.check(ta.getColumns(), 0);
    harness.check(ta.getScrollbarVisibility(), TextArea.SCROLLBARS_VERTICAL_ONLY);

    // try bad scroll bar visibility
    ta = new TextArea("ABC", 3, -7, 999);
    harness.check(ta.getText(), "ABC");
    harness.check(ta.isEditable());
    harness.check(ta.getRows(), 3);
    harness.check(ta.getColumns(), 0);
    harness.check(ta.getScrollbarVisibility(), TextArea.SCROLLBARS_BOTH);
  }

}
