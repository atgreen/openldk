// Tags: JDK1.2

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.awt.AlphaComposite;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.AlphaComposite;

/**
 * Some checks for the getInstance() method in the {@link AlphaComposite} class.  
 */
public class getInstance implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness) 
  {
    test1(harness);
    test2(harness);
  }

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test1(TestHarness harness)  
  {
    harness.checkPoint("(int)");
    
    // CLEAR
    AlphaComposite a = AlphaComposite.getInstance(AlphaComposite.CLEAR);
    harness.check(a.getRule(), AlphaComposite.CLEAR);
    harness.check(a.getAlpha(), 1.0f);
    
    // SRC
    a = AlphaComposite.getInstance(AlphaComposite.SRC);
    harness.check(a.getRule(), AlphaComposite.SRC);
    harness.check(a.getAlpha(), 1.0f);

    // DST
    a = AlphaComposite.getInstance(AlphaComposite.DST);
    harness.check(a.getRule(), AlphaComposite.DST);
    harness.check(a.getAlpha(), 1.0f);

    // SRC_OVER
    a = AlphaComposite.getInstance(AlphaComposite.SRC_OVER);
    harness.check(a.getRule(), AlphaComposite.SRC_OVER);
    harness.check(a.getAlpha(), 1.0f);

    // DST_OVER
    a = AlphaComposite.getInstance(AlphaComposite.DST_OVER);
    harness.check(a.getRule(), AlphaComposite.DST_OVER);
    harness.check(a.getAlpha(), 1.0f);

    // SRC_IN
    a = AlphaComposite.getInstance(AlphaComposite.SRC_IN);
    harness.check(a.getRule(), AlphaComposite.SRC_IN);
    harness.check(a.getAlpha(), 1.0f);

    // DST_IN
    a = AlphaComposite.getInstance(AlphaComposite.DST_IN);
    harness.check(a.getRule(), AlphaComposite.DST_IN);
    harness.check(a.getAlpha(), 1.0f);

    // SRC_OUT
    a = AlphaComposite.getInstance(AlphaComposite.SRC_OUT);
    harness.check(a.getRule(), AlphaComposite.SRC_OUT);
    harness.check(a.getAlpha(), 1.0f);

    // DST_OUT
    a = AlphaComposite.getInstance(AlphaComposite.DST_OUT);
    harness.check(a.getRule(), AlphaComposite.DST_OUT);
    harness.check(a.getAlpha(), 1.0f);
  
    // try a bad constant
    boolean pass = false;
    try
    {
      a = AlphaComposite.getInstance(99);   
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
    
    // try another bad constant
    pass = false;
    try
    {
      a = AlphaComposite.getInstance(-1);   
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
    
  }

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test2(TestHarness harness)  
  {
    harness.checkPoint("(int, float)");
 
    AlphaComposite a = AlphaComposite.getInstance(AlphaComposite.SRC, 0.123f);
    harness.check(a.getRule(), AlphaComposite.SRC);
    harness.check(a.getAlpha(), 0.123f);
    
    // try negative alpha
    boolean pass = false;
    try
    {
      /* a = */ AlphaComposite.getInstance(AlphaComposite.SRC, -0.01f);    
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);

    // try alpha > 1.0
    pass = false;
    try
    {
      /* a = */ AlphaComposite.getInstance(AlphaComposite.SRC, 1.01f);    
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
  
  }
}
