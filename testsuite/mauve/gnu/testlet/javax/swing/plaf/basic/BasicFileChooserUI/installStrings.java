// Tags: JDK1.2 
// Uses: MyBasicFileChooserUI

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

import java.util.Locale;

import javax.swing.JFileChooser;
import javax.swing.plaf.basic.BasicFileChooserUI;

/**
 * Some checks for the installStrings() method in the 
 * {@link BasicFileChooserUI} class.
 */
public class installStrings implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness) 
  {
    Locale.setDefault(Locale.UK);
    JFileChooser fc1 = new JFileChooser();
    MyBasicFileChooserUI ui = new MyBasicFileChooserUI(fc1);
    harness.check(ui.getCancelButtonText(), null);
    harness.check(ui.getCancelButtonToolTipText(), null);
    harness.check(ui.getCancelButtonMnemonic(), 0);
    harness.check(ui.getDirectoryOpenButtonText(), null);
    harness.check(ui.getDirectoryOpenButtonToolTipText(), null);
    harness.check(ui.getDirectoryOpenButtonMnemonic(), 0);
    harness.check(ui.getHelpButtonText(), null);
    harness.check(ui.getHelpButtonToolTipText(), null);
    harness.check(ui.getHelpButtonMnemonic(), 0);
    harness.check(ui.getOpenButtonText(), null);
    harness.check(ui.getOpenButtonToolTipText(), null);
    harness.check(ui.getOpenButtonMnemonic(), 0);
    harness.check(ui.getSaveButtonText(), null);
    harness.check(ui.getSaveButtonToolTipText(), null);
    harness.check(ui.getSaveButtonMnemonic(), 0);
    harness.check(ui.getUpdateButtonText(), null);
    harness.check(ui.getUpdateButtonToolTipText(), null);
    harness.check(ui.getUpdateButtonMnemonic(), 0);
    
    ui.installStrings(fc1);
    harness.check(ui.getCancelButtonText(), "Cancel");
    harness.check(ui.getCancelButtonToolTipText(), "Abort file chooser dialog");
    harness.check(ui.getCancelButtonMnemonic(), 67);
    harness.check(ui.getDirectoryOpenButtonText(), "Open");
    harness.check(ui.getDirectoryOpenButtonToolTipText(), "Open selected directory");
    harness.check(ui.getDirectoryOpenButtonMnemonic(), 79);
    harness.check(ui.getHelpButtonText(), "Help");
    harness.check(ui.getHelpButtonToolTipText(), "FileChooser help");
    harness.check(ui.getHelpButtonMnemonic(), 72);
    harness.check(ui.getOpenButtonText(), "Open");
    harness.check(ui.getOpenButtonToolTipText(), "Open selected file");
    harness.check(ui.getOpenButtonMnemonic(), 79);
    harness.check(ui.getSaveButtonText(), "Save");
    harness.check(ui.getSaveButtonToolTipText(), "Save selected file");
    harness.check(ui.getSaveButtonMnemonic(), 83);
    harness.check(ui.getUpdateButtonText(), "Update");
    harness.check(ui.getUpdateButtonToolTipText(), "Update directory listing");
    harness.check(ui.getUpdateButtonMnemonic(), 85);

    // it appears that the default strings in the Basic look and feel are
    // not localised...
    Locale.setDefault(Locale.FRANCE);
    JFileChooser fc2 = new JFileChooser();
    MyBasicFileChooserUI ui2 = new MyBasicFileChooserUI(fc2);
    ui2.installStrings(fc2);
    harness.check(ui2.getCancelButtonText(), "Cancel");
    harness.check(ui2.getCancelButtonToolTipText(), "Abort file chooser dialog");
    harness.check(ui2.getDirectoryOpenButtonText(), "Open");
    harness.check(ui2.getDirectoryOpenButtonToolTipText(), "Open selected directory");
    harness.check(ui2.getDirectoryOpenButtonMnemonic(), 79);
    harness.check(ui2.getHelpButtonText(), "Help");
    harness.check(ui2.getHelpButtonToolTipText(), "FileChooser help");
    harness.check(ui2.getHelpButtonMnemonic(), 72);
    harness.check(ui2.getOpenButtonText(), "Open");
    harness.check(ui2.getOpenButtonToolTipText(), "Open selected file");
    harness.check(ui2.getOpenButtonMnemonic(), 79);
    harness.check(ui2.getSaveButtonText(), "Save");
    harness.check(ui2.getSaveButtonToolTipText(), "Save selected file");
    harness.check(ui2.getSaveButtonMnemonic(), 83);
    harness.check(ui2.getUpdateButtonText(), "Update");
    harness.check(ui2.getUpdateButtonToolTipText(), "Update directory listing");
    harness.check(ui2.getUpdateButtonMnemonic(), 85);
  }
  
}
