/* getIndexAtPointBounds.java -- Tests AccessibleJLabel.getIndexAtPoint()
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

// Tags: JDK1.3

package gnu.testlet.javax.swing.JLabel.AccessibleJLabel;

import java.awt.FontMetrics;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;

import javax.accessibility.AccessibleText;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.SwingUtilities;
import javax.swing.plaf.basic.BasicHTML;
import javax.swing.text.Position;
import javax.swing.text.View;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests the functionality of AccessibleJLabel.getCharacterBounds().
 */
public class getIndexAtPoint implements Testlet
{

  public void test(TestHarness harness)
  {
    testNoHTML(harness);
    testHTML(harness);
  }

  /**
   * For non-HTML labels the method returns -1. Maybe this test is a little
   * strict. Implementations could very well return something more reasonable
   * here. However, the reference implementation does not.
   */
  private void testNoHTML(TestHarness h)
  {
    h.checkPoint("testNoHTML");
    JLabel l = new JLabel("Hello World");
    AccessibleText at = (AccessibleText) l.getAccessibleContext();
    int i = at.getIndexAtPoint(new Point(5, 5));
    h.check(i, -1);
  }

  private void testHTML(TestHarness h)
  {
    h.checkPoint("testHTML");
    JFrame f = new JFrame();
    JLabel l = new JLabel("<html>Hello World</html>");
    f.getContentPane().add(l);
    f.pack();
    f.setVisible(true);
    l.setSize(100, 30);
    AccessibleText at = (AccessibleText) l.getAccessibleContext();
    // The HTML renderer is stored as client property.
    View v = (View) l.getClientProperty(BasicHTML.propertyKey);
    Rectangle r = getTextRectangle(l);
    int expected = -1;
    expected = v.viewToModel(5, 5, r, new Position.Bias[0]);
    int i = at.getIndexAtPoint(new Point(5, 5));
    h.check(i, expected);
    f.dispose();
  }

  private Rectangle getTextRectangle(JLabel l)
  {
    Rectangle textR = new Rectangle();
    Rectangle iconR = new Rectangle();
    Insets i = l.getInsets();
    int w = l.getWidth();
    int h = l.getHeight();
    Rectangle viewR = new Rectangle(i.left, i.top, w - i.left - i.right,
                                    h - i.top - i.bottom);
    FontMetrics fm = l.getFontMetrics(l.getFont());
    SwingUtilities.layoutCompoundLabel(l, fm, l.getText(), l.getIcon(),
                                       l.getVerticalAlignment(),
                                       l.getHorizontalAlignment(),
                                       l.getVerticalTextPosition(),
                                       l.getHorizontalTextPosition(),
                                       viewR, iconR, textR,
                                       l.getIconTextGap());
    return textR;
  }
}
