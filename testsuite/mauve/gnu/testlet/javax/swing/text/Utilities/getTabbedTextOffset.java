/* getTabbedTextOffset.java
   Copyright (C) 2006  Robert Schuster <robertschuster@fsfe.org>
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

// Tags: 1.2

package gnu.testlet.javax.swing.text.Utilities;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Font;
import java.awt.FontMetrics;

import javax.swing.text.Segment;
import javax.swing.text.Utilities;

public class getTabbedTextOffset implements Testlet
{
    

  static FontMetrics fm = new TestFontMetrics();
  
  // Since every character in the TestFontMetrics has the same width it does
  // not matter which text we use. We only need lots of characters.
  static Segment s = createSegment("iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii"
                            + "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii"
                            + "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii");

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
     // The x value that has to be reached before getTabbedTextOffset stops.
     int endX = 50;

     harness.checkPoint("without rounding");
     for (int i=0; i <= endX; i++)
       harness.check(calculate(i, false), expectWithoutRounding(i));

     harness.checkPoint("with rounding");
     for (int i=0; i <= endX; i++)
       harness.check(calculate(i, true), expectWithRounding(i));

     harness.checkPoint("with rounding (implicit)");
     for (int i=0; i <= endX; i++)
       harness.check(calculate(i), expectWithRounding(i));

  }

  static int calculate(int index)
  {
    // This just checks that the variant of the method without the rounding
    // parameter calls the other one with round=true.
    return Utilities.getTabbedTextOffset(s, fm, 0, index, null, 0);
  }


  static int calculate(int index, boolean round)
  {
    // The test does not care about the start offset or an x value != 0
    // as they should only shift the results.
    return Utilities.getTabbedTextOffset(s, fm, 0, index, null, 0, round);
  }

  /** Returns the expected value when rounding is not active. */
  static int expectWithoutRounding(int index)
  {
    return index / 10;
  }

  /** Returns the expected value when rounding is active. */
  static int expectWithRounding(int index)
  {
    return (index + 5)/10;
  }

  static Segment createSegment(String foo)
  {
    return new Segment(foo.toCharArray(), 0, foo.length());
  }

  // A very simple FontMetrics implementation for use with the test.
  static class TestFontMetrics extends FontMetrics
  {

    public TestFontMetrics()
    {
      super(new Font(null, 0, 10));
    }

    public int getAscent()
    {
      return 10;
    }

    public int getLeading()
    {
      return 10;
    }

    public int getMaxAdvance()
    {
      return 10;
    }

    public int charWidth(char ch)
    {
      return 10;
    }

    public int charsWidth(char[] a, int offs, int len)
    {
      return len*10;
    }

  }

}
