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
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.javax.swing.SwingUtilities;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Rectangle;

import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.plaf.basic.BasicIconFactory;

/**
 * Some checks for the layoutCompoundLabel() method in the SwingUtilities
 * class.
 */
public class layoutCompoundLabel implements Testlet, SwingConstants
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
  
  public void test1(TestHarness harness)
  {
    harness.checkPoint("(JComponent)");
    test1TextOnly(harness);
    test1IconOnly(harness);
    test1TextAndIconCL(harness);
    test1TextAndIconCR(harness);
  }
  
  /**
   * Tests various combinations with text but no icon.
   * 
   * @param harness  the test harness.
   */
  public void test1TextOnly(TestHarness harness) 
  {
    JLabel label = new JLabel("X");
    Font font = new Font("Dialog", Font.PLAIN, 12);
    FontMetrics fm = label.getFontMetrics(font);
    String text = "X";
    String displayText = "";
    int ww = fm.stringWidth(text);
    int hh = fm.getHeight();
    Icon icon = null;
    Rectangle viewR = new Rectangle(10, 20, 100, 25);
    Rectangle iconR = new Rectangle();
    Rectangle textR = new Rectangle();
    
    harness.checkPoint("TL-text");
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, TOP, LEFT, CENTER, LEFT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(iconR, new Rectangle(10 + ww, 20 + hh / 2, 0, 0));
    harness.check(textR, new Rectangle(10, 20, ww, hh));
    
    harness.checkPoint("TC-text");
    iconR = new Rectangle();
    textR = new Rectangle();
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, TOP, CENTER, CENTER, LEFT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(iconR, new Rectangle(60 + (ww + 1) / 2, 20 + hh / 2, 0, 0));
    harness.check(textR, new Rectangle(60 - ww / 2, 20, ww, hh));
    
    harness.checkPoint("TR-text");
    iconR = new Rectangle();
    textR = new Rectangle();
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, TOP, RIGHT, CENTER, LEFT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(iconR, new Rectangle(110, 20 + hh / 2, 0, 0));
    harness.check(textR, new Rectangle(110 - ww, 20, ww, hh));
    
    harness.checkPoint("CL-text");
    iconR = new Rectangle();
    textR = new Rectangle();
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, CENTER, LEFT, CENTER, LEFT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(iconR, new Rectangle(10 + ww, 32, 0, 0));
    harness.check(textR, new Rectangle(10, 32 - hh / 2, ww, hh));
    
    harness.checkPoint("CC-text");
    iconR = new Rectangle();
    textR = new Rectangle();
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, CENTER, CENTER, CENTER, LEFT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(iconR, new Rectangle(60 + (ww + 1) / 2, 32, 0, 0));
    harness.check(textR, new Rectangle(60 - ww / 2, 32 - hh / 2, ww, hh));
    
    harness.checkPoint("CR-text");
    iconR = new Rectangle();
    textR = new Rectangle();
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, CENTER, RIGHT, CENTER, LEFT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(iconR, new Rectangle(110, 32, 0, 0));
    harness.check(textR, new Rectangle(110 - ww, 32 - hh / 2, ww, hh));
    
    harness.checkPoint("BL-text");
    iconR = new Rectangle();
    textR = new Rectangle();
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, BOTTOM, LEFT, CENTER, LEFT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(iconR, new Rectangle(10 + ww, 37, 0, 0));
    harness.check(textR, new Rectangle(10, 45 - hh, ww, hh));
    
    harness.checkPoint("BC-text");
    iconR = new Rectangle();
    textR = new Rectangle();
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, BOTTOM, CENTER, CENTER, LEFT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(iconR, new Rectangle(60 + (ww + 1) / 2, 37, 0, 0));
    harness.check(textR, new Rectangle(60 - ww / 2, 45 - hh, ww, hh));
    
    harness.checkPoint("BR-text");
    iconR = new Rectangle();
    textR = new Rectangle();
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, BOTTOM, RIGHT, CENTER, LEFT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(iconR, new Rectangle(110, 37, 0, 0));
    harness.check(textR, new Rectangle(110 - ww, 45 - hh, ww, hh));

  }
  
  public void test1IconOnly(TestHarness harness) 
  {
    JLabel label = new JLabel("X");
    Font font = new Font("Dialog", Font.PLAIN, 12);
    FontMetrics fm = label.getFontMetrics(font);
    String text = null;
    String displayText = "";
    Icon icon = BasicIconFactory.getMenuArrowIcon();
    int ww = icon.getIconWidth();
    int hh = icon.getIconHeight();
    Rectangle viewR = new Rectangle(10, 20, 100, 25);
    Rectangle iconR = new Rectangle();
    Rectangle textR = new Rectangle();
    
    harness.checkPoint("TL-icon");
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, TOP, LEFT, CENTER, LEFT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(textR, new Rectangle(10, 24, 0, 0));
    harness.check(iconR, new Rectangle(10, 20, ww, hh));
    
    harness.checkPoint("TC-icon");
    iconR = new Rectangle();
    textR = new Rectangle();
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, TOP, CENTER, CENTER, LEFT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(textR, new Rectangle(60 - ww / 2, 24, 0, 0));
    harness.check(iconR, new Rectangle(60 - ww / 2, 20, ww, hh));
    
    harness.checkPoint("TR-icon");
    iconR = new Rectangle();
    textR = new Rectangle();
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, TOP, RIGHT, CENTER, LEFT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(textR, new Rectangle(110 - ww, 24, 0, 0));
    harness.check(iconR, new Rectangle(110 - ww, 20, ww, hh));
    
    harness.checkPoint("CL-icon");
    iconR = new Rectangle();
    textR = new Rectangle();
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, CENTER, LEFT, CENTER, LEFT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(textR, new Rectangle(10, 32, 0, 0));
    harness.check(iconR, new Rectangle(10, 32 - hh / 2, ww, hh));
    
    harness.checkPoint("CC-icon");
    iconR = new Rectangle();
    textR = new Rectangle();
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, CENTER, CENTER, CENTER, LEFT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(textR, new Rectangle(60 - ww / 2, 32, 0, 0));
    harness.check(iconR, new Rectangle(60 - ww / 2, 32 - hh / 2, ww, hh));
    
    harness.checkPoint("CR-icon");
    iconR = new Rectangle();
    textR = new Rectangle();
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, CENTER, RIGHT, CENTER, LEFT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(textR, new Rectangle(110 - ww, 32, 0, 0));
    harness.check(iconR, new Rectangle(110 - ww, 32 - hh / 2, ww, hh));
    
    harness.checkPoint("BL-icon");
    iconR = new Rectangle();
    textR = new Rectangle();
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, BOTTOM, LEFT, CENTER, LEFT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(textR, new Rectangle(10, 41, 0, 0));
    harness.check(iconR, new Rectangle(10, 45 - hh, ww, hh));
    
    harness.checkPoint("BC-icon");
    iconR = new Rectangle();
    textR = new Rectangle();
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, BOTTOM, CENTER, CENTER, LEFT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(textR, new Rectangle(60 - ww / 2, 41, 0, 0));
    harness.check(iconR, new Rectangle(60 - ww / 2, 45 - hh, ww, hh));
    
    harness.checkPoint("BR-icon");
    iconR = new Rectangle();
    textR = new Rectangle();
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, BOTTOM, RIGHT, CENTER, LEFT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(textR, new Rectangle(110 - ww, 41, 0, 0));
    harness.check(iconR, new Rectangle(110 - ww, 45 - hh, ww, hh));
  }
  
  public void test1TextAndIconCL(TestHarness harness) 
  {
    JLabel label = new JLabel("X");
    Font font = new Font("Dialog", Font.PLAIN, 12);
    FontMetrics fm = label.getFontMetrics(font);
    String text = "X";
    String displayText = "";
    int ww = fm.stringWidth(text);
    int hh = fm.getHeight();
    Icon icon = BasicIconFactory.getMenuArrowIcon();
    int iconW = icon.getIconWidth();
    int iconH = icon.getIconHeight();
    Rectangle viewR = new Rectangle(10, 20, 100, 25);
    Rectangle iconR = new Rectangle();
    Rectangle textR = new Rectangle();
    
    harness.checkPoint("TL-CL");
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, TOP, LEFT, CENTER, LEFT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(iconR, new Rectangle(10 + ww + 3, 20 + ((hh - icon.getIconHeight()) / 2), iconW, iconH));
    harness.check(textR, new Rectangle(10, 20, ww, hh));
    
    harness.checkPoint("TC-CL");
    iconR = new Rectangle();
    textR = new Rectangle();
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, TOP, CENTER, CENTER, LEFT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(iconR, new Rectangle(60 + ww / 2, 20 + ((hh - icon.getIconHeight()) / 2), iconW, iconH));
    harness.check(textR, new Rectangle(60 - (ww + 1) / 2 - 3, 20, ww, hh));
    
    harness.checkPoint("TR-CL");
    iconR = new Rectangle();
    textR = new Rectangle();
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, TOP, RIGHT, CENTER, LEFT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(iconR, new Rectangle(110 - iconW, 20 + ((hh - icon.getIconHeight()) / 2), iconW, iconH));
    harness.check(textR, new Rectangle(110 - ww - iconW - 3, 20, ww, hh));
    
    harness.checkPoint("CL-CL");
    iconR = new Rectangle();
    textR = new Rectangle();
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, CENTER, LEFT, CENTER, LEFT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(iconR, new Rectangle(10 + ww + 3, 32 - iconH / 2, iconW, iconH));
    harness.check(textR, new Rectangle(10, 32 - hh / 2, ww, hh));
    
    harness.checkPoint("CC-CL");
    iconR = new Rectangle();
    textR = new Rectangle();
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, CENTER, CENTER, CENTER, LEFT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(iconR, new Rectangle(60 + ww / 2, 32 - iconH / 2, iconW, iconH));
    harness.check(textR, new Rectangle(60 - (ww + 1) / 2 - 3, 32 - hh / 2, ww, hh));
    
    harness.checkPoint("CR-CL");
    iconR = new Rectangle();
    textR = new Rectangle();
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, CENTER, RIGHT, CENTER, LEFT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(iconR, new Rectangle(110 - iconW, 32 - iconH / 2, iconW, iconH));
    harness.check(textR, new Rectangle(110 - ww - iconW - 3, 32 - hh / 2, ww, hh));
    
    harness.checkPoint("BL-CL");
    iconR = new Rectangle();
    textR = new Rectangle();
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, BOTTOM, LEFT, CENTER, LEFT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(iconR, new Rectangle(10 + ww + 3, 45 - hh + (hh - iconH) / 2, iconW, iconH));
    harness.check(textR, new Rectangle(10, 45 - hh, ww, hh));
    
    harness.checkPoint("BC-CL");
    iconR = new Rectangle();
    textR = new Rectangle();
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, BOTTOM, CENTER, CENTER, LEFT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(iconR, new Rectangle(60 + ww / 2, 45 - hh + (hh - iconH) / 2, iconW, iconH));
    harness.check(textR, new Rectangle(60 - (ww + 1) / 2 - 3, 45 - hh, ww, hh));
    
    harness.checkPoint("BR-CL");
    iconR = new Rectangle();
    textR = new Rectangle();
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, BOTTOM, RIGHT, CENTER, LEFT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(iconR, new Rectangle(110 - iconW, 45 - hh + (hh - iconH) / 2, iconW, iconH));
    harness.check(textR, new Rectangle(110 - ww - iconW - 3, 45 - hh, ww, hh));
  }
 
  public void test1TextAndIconCR(TestHarness harness) 
  {
    JLabel label = new JLabel("X");
    Font font = new Font("Dialog", Font.PLAIN, 12);
    FontMetrics fm = label.getFontMetrics(font);
    String text = "X";
    String displayText = "";
    int ww = fm.stringWidth(text);
    int hh = fm.getHeight();
    Icon icon = BasicIconFactory.getMenuArrowIcon();
    int iconW = icon.getIconWidth();
    int iconH = icon.getIconHeight();
    Rectangle viewR = new Rectangle(10, 20, 100, 25);
    Rectangle iconR = new Rectangle();
    Rectangle textR = new Rectangle();
    
    harness.checkPoint("TL-CR");
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, TOP, LEFT, CENTER, RIGHT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(iconR, new Rectangle(10, 20 + ((hh - icon.getIconHeight()) / 2), iconW, iconH));
    harness.check(textR, new Rectangle(10 + iconW + 3, 20, ww, hh));
    
    harness.checkPoint("TC-CR");
    iconR = new Rectangle();
    textR = new Rectangle();
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, TOP, CENTER, CENTER, RIGHT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(iconR, new Rectangle(60 - (ww + 1) / 2 - 3, 20 + ((hh - icon.getIconHeight()) / 2), iconW, iconH));
    harness.check(textR, new Rectangle(60 + (iconW + 3 + ww + 1) / 2 - ww, 20, ww, hh));
    
    harness.checkPoint("TR-CR");
    iconR = new Rectangle();
    textR = new Rectangle();
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, TOP, RIGHT, CENTER, RIGHT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(iconR, new Rectangle(110 - ww - iconW - 3, 20 + ((hh - icon.getIconHeight()) / 2), iconW, iconH));
    harness.check(textR, new Rectangle(110 - ww, 20, ww, hh));
    
    harness.checkPoint("CL-CR");
    iconR = new Rectangle();
    textR = new Rectangle();
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, CENTER, LEFT, CENTER, RIGHT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(iconR, new Rectangle(10, 32 - iconH / 2, iconW, iconH));
    harness.check(textR, new Rectangle(10 + iconW + 3, 32 - hh / 2, ww, hh));
    
    harness.checkPoint("CC-CR");
    iconR = new Rectangle();
    textR = new Rectangle();
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, CENTER, CENTER, CENTER, RIGHT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(iconR, new Rectangle(60 - (ww + 1) / 2 - 3, 32 - iconH / 2, iconW, iconH));
    harness.check(textR, new Rectangle(60 + (iconW + 3 + ww + 1) / 2 - ww, 32 - hh / 2, ww, hh));
    
    harness.checkPoint("CR-CR");
    iconR = new Rectangle();
    textR = new Rectangle();
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, CENTER, RIGHT, CENTER, RIGHT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(iconR, new Rectangle(110 - ww - iconW - 3, 32 - iconH / 2, iconW, iconH));
    harness.check(textR, new Rectangle(110 - ww, 32 - hh / 2, ww, hh));
    
    harness.checkPoint("BL-CR");
    iconR = new Rectangle();
    textR = new Rectangle();
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, BOTTOM, LEFT, CENTER, RIGHT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(iconR, new Rectangle(10, 45 - hh + (hh - iconH) / 2, iconW, iconH));
    harness.check(textR, new Rectangle(10 + iconW + 3, 45 - hh, ww, hh));
    
    harness.checkPoint("BC-CR");
    iconR = new Rectangle();
    textR = new Rectangle();
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, BOTTOM, CENTER, CENTER, RIGHT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(iconR, new Rectangle(60 - (ww + 1) / 2 - 3, 45 - hh + (hh - iconH) / 2, iconW, iconH));
    harness.check(textR, new Rectangle(60 + (iconW + 3 + ww + 1) / 2 - ww, 45 - hh, ww, hh));
    
    harness.checkPoint("BR-CR");
    iconR = new Rectangle();
    textR = new Rectangle();
    displayText = SwingUtilities.layoutCompoundLabel(fm, text, icon, BOTTOM, RIGHT, CENTER, RIGHT, viewR, iconR, textR, 3);
    harness.check(viewR, new Rectangle(10, 20, 100, 25));
    harness.check(iconR, new Rectangle(110 - ww - iconW - 3, 45 - hh + (hh - iconH) / 2, iconW, iconH));
    harness.check(textR, new Rectangle(110 - ww, 45 - hh, ww, hh));
  }

}
