// Tags: JDK1.2

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

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

import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Insets;

import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;

/**
 * Some checks for the getBorderInsets() methods of the {@link TitledBorder} 
 * class.
 */
public class getBorderInsets implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)       
  {
    test1(harness);
  }
  
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test1(TestHarness harness)       
  {
    harness.checkPoint("(Component)");
    JPanel p = new JPanel();
    TitledBorder b = new TitledBorder(new EmptyBorder(1, 2, 3, 4));
    p.setBorder(b);
    p.setFont(b.getTitleFont());
    Insets insets = b.getBorderInsets(p);
    harness.check(insets, new Insets(5, 6, 7, 8));
    
    FontMetrics fm = p.getFontMetrics(p.getFont());
    int fontHeight = fm.getAscent() + fm.getDescent();
    b.setTitle("XYZ");
    insets = b.getBorderInsets(p);
    harness.check(insets, new Insets(5 + fontHeight, 6, 7, 8));
    
    b.setTitleFont(new Font("Dialog", Font.PLAIN, 24));
    p.setFont(b.getTitleFont());
    fm = p.getFontMetrics(p.getFont());
    fontHeight = fm.getAscent() + fm.getDescent();
    insets = b.getBorderInsets(p);
    harness.check(insets, new Insets(5 + fontHeight, 6, 7, 8));

    b.setTitlePosition(TitledBorder.ABOVE_TOP);
    insets = b.getBorderInsets(p);
    harness.check(insets, new Insets(5 + fontHeight + 2, 6, 7, 8));
  
    b.setTitlePosition(TitledBorder.TOP);
    insets = b.getBorderInsets(p);
    harness.check(insets, new Insets(5 + fontHeight, 6, 7, 8));

    b.setTitlePosition(TitledBorder.BELOW_TOP);
    insets = b.getBorderInsets(p);
    harness.check(insets, new Insets(5 + fontHeight + 2, 6, 7, 8));
  
    b.setTitlePosition(TitledBorder.ABOVE_BOTTOM);
    insets = b.getBorderInsets(p);
    harness.check(insets, new Insets(5, 6, 7 + fontHeight + 2, 8));
  
    b.setTitlePosition(TitledBorder.BOTTOM);
    insets = b.getBorderInsets(p);
    harness.check(insets, new Insets(5, 6, 7 + fontHeight, 8));

    b.setTitlePosition(TitledBorder.BELOW_BOTTOM);
    insets = b.getBorderInsets(p);
    harness.check(insets, new Insets(5, 6, 7 + fm.getHeight(), 8));
  }

}
