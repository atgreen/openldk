//Tags: JDK1.2

//Copyright (C) 2006 Mark J. Wielaard  (mark@klomp.org)

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

package gnu.testlet.javax.swing.JEditorPane;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import javax.swing.JEditorPane;

public class setText implements Testlet
{
  public void test(TestHarness harness)
  {
    JEditorPane pane = new JEditorPane();
    harness.check(pane.getText(), "");
    pane.setText(pane.getText());
    harness.check(pane.getText(), "");
    pane.setText("");
    harness.check(pane.getText(), "");
    pane.setText(null);
    harness.check(pane.getText(), "");
    pane.setText("GNU");
    harness.check(pane.getText(), "GNU");
    pane.setText(pane.getText());
    harness.check(pane.getText(), "GNU");
    pane.setText("");
    harness.check(pane.getText(), "");
    pane.setText(null);
    harness.check(pane.getText(), "");
  }
}
