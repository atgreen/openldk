//Tags: JDK1.2

//Copyright (C) 2005 Red Hat

//This file is part of Mauve.

//Mauve is free software; you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation; either version 2, or (at your option)
//any later version.

//Mauve is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.

//You should have received a copy of the GNU General Public License
//along with Mauve; see the file COPYING.  If not, write to
//the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
//Boston, MA 02110-1301 USA.

package gnu.testlet.javax.swing.text.BoxView;

import java.awt.Dimension;

import javax.swing.JTextArea;
import javax.swing.plaf.TextUI;
import javax.swing.text.View;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class spans implements Testlet 
{
  public static String markup =
    "Questions are <font size=\"+1\" color=\"blue\">a burden</font> to others,\n" +
    "answers <font size=\"+2\" color=\"red\">a prison</font> for oneself.";
  public static JTextArea ta3 = new JTextArea(markup);

  public void test(TestHarness harness)
  {
    ta3.setLineWrap(true);
    TextUI ui = ta3.getUI();
    View rootView = ui.getRootView(ta3);
    View view = rootView.getView(0);

    Dimension min = ta3.getMinimumSize();
    harness.check (min.width, view.getMinimumSpan(View.X_AXIS));
    harness.check (min.height, view.getMinimumSpan(View.Y_AXIS));
    Dimension pref = ta3.getPreferredSize();
    harness.check (pref.width, view.getPreferredSpan(View.X_AXIS));
    harness.check (pref.height, view.getPreferredSpan(View.Y_AXIS));

    // Asking for minimum size before asking for the preferred
    // size results in minimum and pref being different
    harness.check (!min.equals(pref));

    // Asking for the minimum size after asking for the preferred
    // size uses the cached preferred size
    Dimension min3 = ta3.getMinimumSize();
    harness.check (!min3.equals(min));
    harness.check (min3, pref);

    // Check that room has been allocated for the rows
    // and that the width doesn't change based on the text.
    int width = pref.width;
    harness.check (width > 0);
    ta3.setText("");
    harness.check (width, ta3.getPreferredSize().width);
    ta3.setText("\n\n\n\n\n\n\n");
    harness.check (width, ta3.getPreferredSize().width);
    
    
  }
}
