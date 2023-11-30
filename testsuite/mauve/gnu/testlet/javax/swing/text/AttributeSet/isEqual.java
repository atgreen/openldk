// Tags: JDK1.2

// Copyright (C) 2005 Red Hat.

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
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
// Boston, MA 02110-1301 USA.

package gnu.testlet.javax.swing.text.AttributeSet;

import javax.swing.text.AttributeSet;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleContext;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class isEqual extends StyleContext implements Testlet
{
  /**
   * Starts the test run.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    boolean caught = false;
    SimpleAttributeSet set1 = new SimpleAttributeSet();
    StyleContext.SmallAttributeSet set2 = createSmallAttributeSet(set1);

    try
      {
        set1.isEqual(null);
      }
    catch (NullPointerException npe)
      {
        caught = true;
      }

    harness.check(caught);
    caught = false;

    try
      {
        set2.isEqual(null);
      }
    catch (NullPointerException npe)
      {
        caught = true;
      }
    harness.check (caught);

    harness.check (set2.isEqual(set1));  
    harness.check (set1.isEqual(set2));
  }
}
