/* constructors.java -- Some checks for the constructors in the GapContent
                        class.
   Copyright (C) 2006  David Gilbert <david.gilbert@object-refinery.com>
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
// Uses: MyGapContent

package gnu.testlet.javax.swing.text.GapContent;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.text.BadLocationException;
import javax.swing.text.GapContent;

public class constructors implements Testlet
{
    
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    testConstructor1(harness);
    testConstructor2(harness);
  }
  
  public void testConstructor1(TestHarness harness) 
  {
    harness.checkPoint("()");
    MyGapContent gc = new MyGapContent();
    harness.check(gc.length(), 1);
    
    boolean pass = false;
    try
    {
      pass = gc.getString(0, 1).equals("\n");
    }
    catch (BadLocationException e)
    {
      harness.fail(e.toString());
    }
    harness.check(pass);
    harness.check(gc.getArrayLength(), 10);
  }
  
  public void testConstructor2(TestHarness harness) 
  {
    harness.checkPoint("(int)");
    MyGapContent gc = new MyGapContent(10);
    harness.check(gc.length(), 1);
    boolean pass = false;
    try
    {
      pass = gc.getString(0, 1).equals("\n");
    }
    catch (BadLocationException e)
    {
      harness.fail(e.toString());
    }
    harness.check(pass);
    harness.check(gc.getArrayLength(), 10);
    
    // try unusual initial sizes
    int length = -1;
    int arrayLength = -1;
    MyGapContent gc2 = null;

    try
      {
        gc2 = new MyGapContent(0);
        length = gc2.length();
        arrayLength = gc2.getArrayLength();
      }
    catch (Exception e)
      {
        // Can be ignored.
      }
    harness.check(length, 1);
    harness.check(arrayLength, 2);

    length = -1;
    arrayLength = -1;
    gc2 = null;
    try
      {
        gc2 = new MyGapContent(0);
        length = gc2.length();
        arrayLength = gc2.getArrayLength();
      }
    catch (Exception e)
      {
        // Can be ignored. 
      }
    harness.check(length, 1);
    harness.check(arrayLength, 2);

    
  }

}

