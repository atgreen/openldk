/* constructors.java -- some checks for the constructors in the SizeSequence
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

// Tags: JDK1.2

package gnu.testlet.javax.swing.SizeSequence;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Arrays;

import javax.swing.SizeSequence;

public class constructors implements Testlet
{
    
  public void test(TestHarness harness)
  {
    testConstructor1(harness);
    testConstructor2(harness);
    testConstructor3(harness);
    testConstructor4(harness);
  }
  
  private void testConstructor1(TestHarness harness)
  {
    harness.checkPoint("()");
    SizeSequence s = new SizeSequence();  
    harness.check(Arrays.equals(s.getSizes(), new int[0]));
  }
  
  private void testConstructor2(TestHarness harness)
  {
    harness.checkPoint("(int)");
    
    SizeSequence s = new SizeSequence(3);
    harness.check(Arrays.equals(s.getSizes(), new int[3]));
    
    // try negative
    boolean pass = false;
    try
      {
        s = new SizeSequence(-1);
      }
    catch (NegativeArraySizeException e)
      {
        pass = true;  
      }
    harness.check(pass);
  }
  
  private void testConstructor3(TestHarness harness)
  {
    harness.checkPoint("(int, int)");
    SizeSequence s = new SizeSequence(3, 5);
    harness.check(Arrays.equals(s.getSizes(), new int[] {5, 5, 5}));

    // try negative
    boolean pass = false;
    try
      {
        s = new SizeSequence(-1, 5);
      }
    catch (NegativeArraySizeException e)
      {
        pass = true;  
      }
    harness.check(pass);
  }

  private void testConstructor4(TestHarness harness)
  {
    harness.checkPoint("(int[])");
    int[] sizes = new int[] {1, 2, 3};
    SizeSequence s = new SizeSequence(sizes);
    harness.check(Arrays.equals(s.getSizes(), new int[] {1, 2, 3}));
    sizes[0] = 99;
    harness.check(Arrays.equals(s.getSizes(), new int[] {1, 2, 3}));
    
    // try null
    boolean pass = false;
    try
    {
      s = new SizeSequence(null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  }

}
