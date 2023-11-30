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
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
// Boston, MA 02110-1301 USA.


package gnu.testlet.javax.swing.plaf.basic.BasicFileChooserUI;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.JFileChooser;
import javax.swing.plaf.basic.BasicFileChooserUI;

/**
 * Some checks for the getApproveButtonMnemonic() method in the 
 * {@link BasicFileChooserUI} class.
 */
public class getApproveButtonMnemonic implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness) 
  {
    JFileChooser fc = new JFileChooser();
    fc.setDialogType(JFileChooser.OPEN_DIALOG);
    BasicFileChooserUI ui = new BasicFileChooserUI(fc);
    ui.installUI(fc);
    harness.check(ui.getApproveButtonMnemonic(fc), 79);
    fc.setDialogType(JFileChooser.SAVE_DIALOG);
    harness.check(ui.getApproveButtonMnemonic(fc), 83);
    fc.setApproveButtonMnemonic(99);    
    harness.check(ui.getApproveButtonMnemonic(fc), 99);
  }
  
}
