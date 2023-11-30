/* preferredLayoutSize.java -- Checks for the preferredLayoutSize() method in 
                               the JSpinner.DefaultEditor class.
   Copyright (C) 2006 David Gilbert <david.gilbert@object-refinery.com>
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

// Tags: JDK1.4

package gnu.testlet.javax.swing.JSpinner.DefaultEditor;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Dimension;
import java.awt.Insets;

import javax.swing.JSpinner;

public class preferredLayoutSize implements Testlet 
{

  public void test(TestHarness harness) 
  {
    harness.checkPoint("(Container)");
    JSpinner s = new JSpinner();
    JSpinner.NumberEditor editor = (JSpinner.NumberEditor) s.getEditor();
    Dimension tfSize = editor.getTextField().getPreferredSize();
    Insets insets = editor.getInsets();
    Dimension result = editor.preferredLayoutSize(editor);
    harness.check(result.width, tfSize.width + insets.left + insets.right);
    harness.check(result.height, tfSize.height + insets.top + insets.bottom);
  }
}
