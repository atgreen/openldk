// Tags: JDK1.2
// Uses: MyMetalScrollBarUI

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
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
// Boston, MA 02110-1301 USA.

package gnu.testlet.javax.swing.plaf.metal.MetalScrollBarUI;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Rectangle;

import javax.swing.JScrollBar;
import javax.swing.UIManager;
import javax.swing.plaf.metal.MetalLookAndFeel;
import javax.swing.plaf.metal.MetalScrollBarUI;

/**
 * Some checks for the layoutContainer() method in the 
 * {@link MetalScrollBarUI} class.  
 */
public class layoutContainer implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)  
  {
    // use a known look and feel
    try
    {
      UIManager.setLookAndFeel(new MetalLookAndFeel());
    }
    catch (Exception e)
    {
      e.printStackTrace();
    }
    testHorizontal(harness);
    testHorizontalFreeStanding(harness);
    testVertical(harness);
    testVerticalFreeStanding(harness);
  }

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void testHorizontal(TestHarness harness)  
  {
    harness.checkPoint("testHorizontal()");
    MyMetalScrollBarUI ui = new MyMetalScrollBarUI();
    JScrollBar scrollBar = new JScrollBar(JScrollBar.HORIZONTAL);
    scrollBar.putClientProperty("JScrollBar.isFreeStanding", Boolean.FALSE);
    scrollBar.setUI(ui);
    scrollBar.setBounds(0, 0, 100, 20);
    ui.layoutContainer(scrollBar);
    harness.check(ui.getTrackBounds(), new Rectangle(15, 0, 70, 20));    
    harness.check(ui.getThumbBounds(), new Rectangle(15, 0, 15, 20));
    
    scrollBar.setBounds(40, 50, 250, 37);
    ui.layoutContainer(scrollBar);
    harness.check(ui.getTrackBounds(), new Rectangle(15, 0, 220, 37));    
    harness.check(ui.getThumbBounds(), new Rectangle(15, 0, 22, 37));  
  }

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void testHorizontalFreeStanding(TestHarness harness)  
  {
    harness.checkPoint("testHorizontalFreeStanding()");
    MyMetalScrollBarUI ui = new MyMetalScrollBarUI();
    JScrollBar scrollBar = new JScrollBar(JScrollBar.HORIZONTAL);
    scrollBar.putClientProperty("JScrollBar.isFreeStanding", Boolean.TRUE);
    scrollBar.setUI(ui);
    scrollBar.setBounds(0, 0, 100, 20);
    ui.layoutContainer(scrollBar);
    harness.check(ui.getTrackBounds(), new Rectangle(15, 0, 69, 20));    
    harness.check(ui.getThumbBounds(), new Rectangle(15, 0, 17, 20));
    
    scrollBar.setBounds(40, 50, 250, 37);
    ui.layoutContainer(scrollBar);
    harness.check(ui.getTrackBounds(), new Rectangle(15, 0, 219, 37));    
    harness.check(ui.getThumbBounds(), new Rectangle(15, 0, 21, 37));  
  }

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void testVertical(TestHarness harness)  
  {
    harness.checkPoint("testVertical()");
    MyMetalScrollBarUI ui = new MyMetalScrollBarUI();
    JScrollBar scrollBar = new JScrollBar(JScrollBar.VERTICAL);
    scrollBar.putClientProperty("JScrollBar.isFreeStanding", Boolean.FALSE);
    scrollBar.setUI(ui);
    scrollBar.setBounds(0, 0, 20, 100);
    ui.layoutContainer(scrollBar);
    harness.check(ui.getTrackBounds(), new Rectangle(0, 15, 20, 70));    
    harness.check(ui.getThumbBounds(), new Rectangle(0, 15, 20, 15));
    
    scrollBar.setBounds(40, 50, 37, 250);
    ui.layoutContainer(scrollBar);
    harness.check(ui.getTrackBounds(), new Rectangle(0, 15, 37, 220));    
    harness.check(ui.getThumbBounds(), new Rectangle(0, 15, 37, 22));  
  }

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void testVerticalFreeStanding(TestHarness harness)  
  {
    harness.checkPoint("testVerticalFreeStanding()");
    MyMetalScrollBarUI ui = new MyMetalScrollBarUI();
    JScrollBar scrollBar = new JScrollBar(JScrollBar.VERTICAL);
    scrollBar.putClientProperty("JScrollBar.isFreeStanding", Boolean.TRUE);
    scrollBar.setUI(ui);
    scrollBar.setBounds(0, 0, 20, 100);
    ui.layoutContainer(scrollBar);
    harness.check(ui.getTrackBounds(), new Rectangle(0, 15, 20, 69));    
    harness.check(ui.getThumbBounds(), new Rectangle(0, 15, 20, 17));
    
    scrollBar.setBounds(40, 50, 37, 250);
    ui.layoutContainer(scrollBar);
    harness.check(ui.getTrackBounds(), new Rectangle(0, 15, 37, 219));    
    harness.check(ui.getThumbBounds(), new Rectangle(0, 15, 37, 21));  
  }

}
