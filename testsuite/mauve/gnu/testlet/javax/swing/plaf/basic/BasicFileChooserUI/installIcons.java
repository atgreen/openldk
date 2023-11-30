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

import javax.swing.JFileChooser;
import javax.swing.plaf.basic.BasicFileChooserUI;
import javax.swing.plaf.metal.MetalIconFactory;

/**
 * Some checks for the installIcons() method in the 
 * {@link BasicFileChooserUI} class.
 */
public class installIcons implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness) 
  {
    JFileChooser fc = new JFileChooser();
    MyBasicFileChooserUI ui = new MyBasicFileChooserUI(fc);
    harness.check(ui.getComputerIcon(), null);
    harness.check(ui.getDetailsViewIcon(), null);
    harness.check(ui.getDirectoryIcon(), null);
    harness.check(ui.getFileIcon(), null);
    harness.check(ui.getFloppyDriveIcon(), null);
    harness.check(ui.getHardDriveIcon(), null);
    harness.check(ui.getHomeFolderIcon(), null);
    harness.check(ui.getListViewIcon(), null);
    harness.check(ui.getNewFolderIcon(), null);
    harness.check(ui.getUpFolderIcon(), null);
    
    ui.installIcons(fc);
    harness.check(ui.getComputerIcon(), MetalIconFactory.getTreeComputerIcon());
    harness.check(ui.getDetailsViewIcon(), MetalIconFactory.getFileChooserDetailViewIcon());
    harness.check(ui.getDirectoryIcon() instanceof MetalIconFactory.TreeFolderIcon);
    harness.check(ui.getDirectoryIcon().getIconWidth(), 16);    
    harness.check(ui.getDirectoryIcon().getIconHeight(), 18);    
    harness.check(ui.getFileIcon() instanceof MetalIconFactory.TreeLeafIcon);
    harness.check(ui.getFileIcon().getIconWidth(), 16);    
    harness.check(ui.getFileIcon().getIconHeight(), 20);    
    harness.check(ui.getFloppyDriveIcon(), MetalIconFactory.getTreeFloppyDriveIcon());
    harness.check(ui.getHardDriveIcon(), MetalIconFactory.getTreeHardDriveIcon());
    harness.check(ui.getHomeFolderIcon(), MetalIconFactory.getFileChooserHomeFolderIcon());
    harness.check(ui.getListViewIcon(), MetalIconFactory.getFileChooserListViewIcon());
    harness.check(ui.getNewFolderIcon(), MetalIconFactory.getFileChooserNewFolderIcon());
    harness.check(ui.getUpFolderIcon(), MetalIconFactory.getFileChooserUpFolderIcon());
  }
  
}
