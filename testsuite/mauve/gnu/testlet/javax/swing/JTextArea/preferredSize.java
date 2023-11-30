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

package gnu.testlet.javax.swing.JTextArea;

import javax.swing.JTextArea;
import javax.swing.text.View;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class preferredSize implements Testlet 
{
  public static String markup =
    "Questions are <font size=\"+1\" color=\"blue\">a burden</font> to others,\n" +
    "answers <font size=\"+2\" color=\"red\">a prison</font> for oneself.";
  public static JTextArea ta2 = new JTextArea(markup);

  public void test(TestHarness harness)
  {
    View view = ta2.getUI().getRootView(ta2);
    try
      {
        harness.check (view.getPreferredSpan(View.HORIZONTAL) > 
                       view.getPreferredSpan(View.VERTICAL));
        ta2.setText("");
        harness.check (ta2.getPreferredSize().width == 0);
        harness.check (view.getPreferredSpan(View.HORIZONTAL) == 0);
        ta2.setText("\n\n\n\n\n\n\n\n\n");
        harness.check (ta2.getPreferredSize().width == 0);
        harness.check (view.getPreferredSpan(View.HORIZONTAL) == 0);

        ta2.setLineWrap(true);
        ta2.setWrapStyleWord(true);

        harness.check (ta2.getPreferredSize().width == 0);
        harness.check (view.getPreferredSpan(View.HORIZONTAL) == 0);
        ta2.setText("");
        harness.check (ta2.getPreferredSize().width == 0);
        harness.check (view.getPreferredSpan(View.HORIZONTAL) == 0);
        ta2.setText("\n\n\n\n\n\n\n\n\n");
        harness.check (ta2.getPreferredSize().width == 0);
        harness.check (view.getPreferredSpan(View.HORIZONTAL) == 0);
      }
    catch (Exception e)
      {
        // There shouldn't be an exception thrown.  At the time of writing of 
        // this test case, GNU Classpath 0.19 + CVS does fall into this block.
        harness.debug(e);
      }
  }
}
