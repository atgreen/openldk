// Tags: JDK1.4

// Copyright (C) 2004 David Gilbert <david.gilbert@object-refinery.com>

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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.awt.BasicStroke;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.BasicStroke;
import java.util.Arrays;

/**
 * Some checks for the constructors in the {@link BasicStroke} class.
 */
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
    testConstructor3(harness);
    testConstructor4(harness);
    testConstructor5(harness);
  }
  
  private void testConstructor1(TestHarness harness)
  {
    harness.checkPoint("BasicStroke()");
    BasicStroke s = new BasicStroke();
    harness.check(s.getLineWidth(), 1.0f);
    harness.check(s.getEndCap(), BasicStroke.CAP_SQUARE);
    harness.check(s.getLineJoin(), BasicStroke.JOIN_MITER);
    harness.check(s.getMiterLimit(), 10.0f);
  }

  private void testConstructor2(TestHarness harness)
  {
    harness.checkPoint("BasicStroke(float)");
    BasicStroke s = new BasicStroke(3.4f);
    harness.check(s.getLineWidth(), 3.4f);
    harness.check(s.getEndCap(), BasicStroke.CAP_SQUARE);
    harness.check(s.getLineJoin(), BasicStroke.JOIN_MITER);
    harness.check(s.getMiterLimit(), 10.0f);
    
    // negative stroke should throw IllegalArgumentException
    try 
    {
      s = new BasicStroke(-1.0f);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }
    
    // but zero is OK
    try
    {
      s = new BasicStroke(0.0f);
      harness.check(true);
    }
    catch (Exception e)
    {
      harness.check(false);
    }
  }

  private void testConstructor3(TestHarness harness)
  {
    harness.checkPoint("BasicStroke(float, int, int)");
    BasicStroke s = new BasicStroke(4.5f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);
    harness.check(s.getLineWidth(), 4.5f);
    harness.check(s.getEndCap(), BasicStroke.CAP_ROUND);
    harness.check(s.getLineJoin(), BasicStroke.JOIN_ROUND);
    harness.check(s.getMiterLimit(), 10.0f);
    
    // negative stroke should throw IllegalArgumentException
    try 
    {
      s = new BasicStroke(-1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }

    // bad cap type should throw IllegalArgumentException
    try 
    {
      s = new BasicStroke(-1.0f, 11, BasicStroke.JOIN_ROUND);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }

    // bad join type should throw IllegalArgumentException
    try 
    {
      s = new BasicStroke(-1.0f, BasicStroke.CAP_ROUND, 22);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }

  }

  private void testConstructor4(TestHarness harness)
  {
    harness.checkPoint("BasicStroke(float, int, int, float)");
    BasicStroke s = new BasicStroke(4.5f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND, 15.0f);
    harness.check(s.getLineWidth(), 4.5f);
    harness.check(s.getEndCap(), BasicStroke.CAP_ROUND);
    harness.check(s.getLineJoin(), BasicStroke.JOIN_ROUND);
    harness.check(s.getMiterLimit(), 15.0f);
    
    // negative stroke should throw IllegalArgumentException
    try 
    {
      s = new BasicStroke(-1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND, 9.0f);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }

    // bad cap type should throw IllegalArgumentException
    try 
    {
      s = new BasicStroke(-1.0f, 11, BasicStroke.JOIN_ROUND, 9.0f);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }

    // bad join type should throw IllegalArgumentException
    try 
    {
      s = new BasicStroke(-1.0f, BasicStroke.CAP_ROUND, 22, 9.0f);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }

    // miter limit < 1 and join type is miter should throw IllegalArgumentException
    try 
    {
      s = new BasicStroke(-1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_MITER, 0.5f);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }

  }

  private void testConstructor5(TestHarness harness)
  {
    harness.checkPoint("BasicStroke(float, int, int, float, float[], float)");
    BasicStroke s = new BasicStroke(4.5f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND, 15.0f, new float[] {1.0f, 2.0f}, 1.5f);
    harness.check(s.getLineWidth(), 4.5f);
    harness.check(s.getEndCap(), BasicStroke.CAP_ROUND);
    harness.check(s.getLineJoin(), BasicStroke.JOIN_ROUND);
    harness.check(s.getMiterLimit(), 15.0f);
    harness.check(Arrays.equals(s.getDashArray(), new float[] {1.0f, 2.0f}));
    harness.check(s.getDashPhase(), 1.5f);
    
    // negative stroke should throw IllegalArgumentException
    try 
    {
      s = new BasicStroke(-1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND, 9.0f, new float[] {1.0f, 2.0f}, 1.5f);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }

    // bad cap type should throw IllegalArgumentException
    try 
    {
      s = new BasicStroke(-1.0f, 11, BasicStroke.JOIN_ROUND, 9.0f, new float[] {1.0f, 2.0f}, 1.5f);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }

    // bad join type should throw IllegalArgumentException
    try 
    {
      s = new BasicStroke(-1.0f, BasicStroke.CAP_ROUND, 22, 9.0f, new float[] {1.0f, 2.0f}, 1.5f);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }

    // miter limit < 1 and join type is miter should throw IllegalArgumentException
    try 
    {
      s = new BasicStroke(-1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_MITER, 0.5f, new float[] {1.0f, 2.0f}, 1.5f);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }
    
    // dash values all zero should throw IllegalArgumentException
    try 
    {
      s = new BasicStroke(-1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_MITER, 0.5f, new float[] {0.0f, 0.0f}, 1.5f);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }
    
    // zero length dash array should throw IllegalArgumentException
    try 
    {
      s = new BasicStroke(-1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_MITER, 0.5f, new float[] {}, 1.5f);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }
    
    // null dash array should throw NullPointerException?
    try 
    {
      s = new BasicStroke(-1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_MITER, 0.5f, null, 1.5f);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }

    // negative dash phase
    try 
    {
      s = new BasicStroke(-1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_MITER, 0.5f, new float[] {1.0f, 2.0f}, -1.5f);
      harness.check(false);
    }
    catch (IllegalArgumentException e)
    {
      harness.check(true);
    }

  }
  
}
