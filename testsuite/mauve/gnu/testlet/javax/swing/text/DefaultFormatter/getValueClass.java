/* getValueClass.java -- Tests the getValueClass() method of DefaultFormatter
   Copyright (C) 2006 Roman Kennke (kennke@aicas.com)
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

package gnu.testlet.javax.swing.text.DefaultFormatter;

import javax.swing.text.DefaultFormatter;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests if the getValueClass() method works correctly.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class getValueClass implements Testlet
{

  public void test(TestHarness harness)
  {
    testDefaultValue(harness);
  }

  /**
   * Tests the default value for this property, which should be null.
   * 
   * @param h the test harness to use
   */
  private void testDefaultValue(TestHarness h)
  {
    h.checkPoint("defaultValue");
    DefaultFormatter f = new DefaultFormatter();
    h.check(f.getValueClass(), null);
  }
}
