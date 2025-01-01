// Tags: JDK1.2

// Copyright (C) 2005, 2006 David Gilbert <david.gilbert@object-refinery.com>

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

package gnu.testlet.javax.swing.border.TitledBorder;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Color;
import java.awt.Font;

import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import javax.swing.plaf.metal.DefaultMetalTheme;
import javax.swing.plaf.metal.MetalLookAndFeel;

/**
 * Some checks for the constructors of the {@link TitledBorder} 
 * class.
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
    // test with DefaultMetalTheme
    try
      {
        MetalLookAndFeel.setCurrentTheme(new DefaultMetalTheme());
        UIManager.setLookAndFeel(new MetalLookAndFeel());
      }
    catch (UnsupportedLookAndFeelException e)
      {
        e.printStackTrace();  
      }
    test1(harness);
    test2(harness);
    test3(harness);
    test4(harness);
    test5(harness);
    test6(harness);
  }
  
  public void test1(TestHarness harness)       
  {
    harness.checkPoint("(Border)");
    Border b = new EmptyBorder(1, 2, 3, 4);
    TitledBorder tb = new TitledBorder(b);
    harness.check(tb.getBorder(), b);
    harness.check(tb.getTitle(), "");
    Color c = UIManager.getLookAndFeelDefaults().getColor(
            "TitledBorder.titleColor");
    harness.check(tb.getTitleColor(), c);
    Font f = UIManager.getLookAndFeelDefaults().getFont("TitledBorder.font");
    harness.check(tb.getTitleFont(), f);
    harness.check(tb.getTitlePosition(), TitledBorder.DEFAULT_POSITION);
    harness.check(tb.getTitleJustification(), TitledBorder.LEADING);
    
    tb = new TitledBorder((Border) null);
    Border bb = UIManager.getLookAndFeelDefaults().getBorder(
            "TitledBorder.border");
    harness.check(tb.getBorder(), bb);
  }

  public void test2(TestHarness harness)       
  {
    harness.checkPoint("(Border, String)");
    Border b = new EmptyBorder(1, 2, 3, 4);
    TitledBorder tb = new TitledBorder(b, "XYZ");
    harness.check(tb.getBorder(), b);
    harness.check(tb.getTitle(), "XYZ");
    Color c = UIManager.getLookAndFeelDefaults().getColor(
            "TitledBorder.titleColor");
    harness.check(tb.getTitleColor(), c);
    Font f = UIManager.getLookAndFeelDefaults().getFont("TitledBorder.font");
    harness.check(tb.getTitleFont(), f);
    harness.check(tb.getTitlePosition(), TitledBorder.DEFAULT_POSITION);
    harness.check(tb.getTitleJustification(), TitledBorder.LEADING);
    
    tb = new TitledBorder((Border) null, "XYZ");
    Border bb = UIManager.getLookAndFeelDefaults().getBorder(
            "TitledBorder.border");
    harness.check(tb.getBorder(), bb);
    
    tb = new TitledBorder(new EmptyBorder(1, 2, 3, 4), null);
    harness.check(tb.getTitle(), null);
  }

  public void test3(TestHarness harness)       
  {
    harness.checkPoint("(Border, String, int, int)");
    Border b = new EmptyBorder(1, 2, 3, 4);
    TitledBorder tb = new TitledBorder(b, "XYZ", TitledBorder.LEFT, 
            TitledBorder.BELOW_BOTTOM);
    harness.check(tb.getBorder(), b);
    harness.check(tb.getTitle(), "XYZ");
    Color c = UIManager.getLookAndFeelDefaults().getColor(
            "TitledBorder.titleColor");
    harness.check(tb.getTitleColor(), c);
    Font f = UIManager.getLookAndFeelDefaults().getFont("TitledBorder.font");
    harness.check(tb.getTitleFont(), f);
    harness.check(tb.getTitlePosition(), TitledBorder.BELOW_BOTTOM);
    harness.check(tb.getTitleJustification(), TitledBorder.LEFT);
    
    tb = new TitledBorder((Border) null, "XYZ", TitledBorder.LEFT, 
            TitledBorder.BELOW_BOTTOM);
    Border bb = UIManager.getLookAndFeelDefaults().getBorder(
            "TitledBorder.border");
    harness.check(tb.getBorder(), bb);
    
    tb = new TitledBorder(new EmptyBorder(1, 2, 3, 4), null, TitledBorder.LEFT,
            TitledBorder.BELOW_BOTTOM);
    harness.check(tb.getTitle(), null);
  }

  public void test4(TestHarness harness)       
  {
    harness.checkPoint("(Border, String, int, int, Font)");
    Border b = new EmptyBorder(1, 2, 3, 4);
    Font f = new Font("Dialog", Font.BOLD, 16);
    TitledBorder tb = new TitledBorder(b, "XYZ", TitledBorder.LEFT, 
            TitledBorder.BELOW_BOTTOM, f);
    harness.check(tb.getBorder(), b);
    harness.check(tb.getTitle(), "XYZ");
    Color c = UIManager.getLookAndFeelDefaults().getColor(
            "TitledBorder.titleColor");
    harness.check(tb.getTitleColor(), c);
    harness.check(tb.getTitleFont(), f);
    harness.check(tb.getTitlePosition(), TitledBorder.BELOW_BOTTOM);
    harness.check(tb.getTitleJustification(), TitledBorder.LEFT);
    
    tb = new TitledBorder((Border) null, "XYZ", TitledBorder.LEFT, 
            TitledBorder.BELOW_BOTTOM, f);
    Border bb = UIManager.getLookAndFeelDefaults().getBorder(
            "TitledBorder.border");
    harness.check(tb.getBorder(), bb);
    
    tb = new TitledBorder(new EmptyBorder(1, 2, 3, 4), null, TitledBorder.LEFT,
            TitledBorder.BELOW_BOTTOM, f);
    harness.check(tb.getTitle(), null);
  }

  public void test5(TestHarness harness)       
  {
    harness.checkPoint("(Border, String, int, int, Font, Color)");
    Border b = new EmptyBorder(1, 2, 3, 4);
    Font f = new Font("Dialog", Font.BOLD, 16);
    TitledBorder tb = new TitledBorder(b, "XYZ", TitledBorder.LEFT, 
            TitledBorder.BELOW_BOTTOM, f, Color.red);
    harness.check(tb.getBorder(), b);
    harness.check(tb.getTitle(), "XYZ");
    harness.check(tb.getTitleColor(), Color.red);
    harness.check(tb.getTitleFont(), f);
    harness.check(tb.getTitlePosition(), TitledBorder.BELOW_BOTTOM);
    harness.check(tb.getTitleJustification(), TitledBorder.LEFT);
    
    tb = new TitledBorder((Border) null, "XYZ", TitledBorder.LEFT, 
            TitledBorder.BELOW_BOTTOM, f, Color.red);
    Border bb = UIManager.getLookAndFeelDefaults().getBorder(
            "TitledBorder.border");
    harness.check(tb.getBorder(), bb);
    
    tb = new TitledBorder(new EmptyBorder(1, 2, 3, 4), null, TitledBorder.LEFT,
            TitledBorder.BELOW_BOTTOM, f, Color.red);
    harness.check(tb.getTitle(), null);
  }

  public void test6(TestHarness harness)       
  {
    harness.checkPoint("(String)");
    TitledBorder tb = new TitledBorder("XYZ");
    harness.check(tb.getTitle(), "XYZ");
    Border b = UIManager.getLookAndFeelDefaults().getBorder(
            "TitledBorder.border");
    harness.check(tb.getBorder(), b);
    Color c = UIManager.getLookAndFeelDefaults().getColor(
            "TitledBorder.titleColor");
    harness.check(tb.getTitleColor(), c);
    Font f = UIManager.getLookAndFeelDefaults().getFont("TitledBorder.font");
    harness.check(tb.getTitleFont(), f);
    harness.check(tb.getTitlePosition(), TitledBorder.DEFAULT_POSITION);
    harness.check(tb.getTitleJustification(), TitledBorder.LEADING);
    
    tb = new TitledBorder((String) null);
    harness.check(tb.getTitle(), null);
  }
}
