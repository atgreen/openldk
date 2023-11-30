// Tags: not-a-test

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

import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.plaf.basic.BasicFileChooserUI;

/**
 * Provides access to protected fields and methods.
 */
public class MyBasicFileChooserUI extends BasicFileChooserUI 
{

  public MyBasicFileChooserUI(JFileChooser fc) 
  {
    super(fc);
  }
  public JButton getApproveButton(JFileChooser fc)
  {
    return super.getApproveButton(fc);
  }
  public int getCancelButtonMnemonic()
  {
    return this.cancelButtonMnemonic;
  }
  public String getCancelButtonText()
  {
    return this.cancelButtonText;
  }
  public String getCancelButtonToolTipText()
  {
    return this.cancelButtonToolTipText;
  }
  public int getDirectoryOpenButtonMnemonic()
  {
    return this.directoryOpenButtonMnemonic;
  }
  public String getDirectoryOpenButtonText()
  {
    return this.directoryOpenButtonText;
  }
  public String getDirectoryOpenButtonToolTipText()
  {
    return this.directoryOpenButtonToolTipText;
  }
  public int getHelpButtonMnemonic()
  {
    return this.helpButtonMnemonic;
  }
  public String getHelpButtonText()
  {
    return this.helpButtonText;
  }
  public String getHelpButtonToolTipText()
  {
    return this.helpButtonToolTipText;
  }
  public int getOpenButtonMnemonic()
  {
    return this.openButtonMnemonic;
  }
  public String getOpenButtonText()
  {
    return this.openButtonText;
  }
  public String getOpenButtonToolTipText()
  {
    return this.openButtonToolTipText;
  }
  public int getSaveButtonMnemonic()
  {
    return this.saveButtonMnemonic;
  }
  public String getSaveButtonText()
  {
    return this.saveButtonText;
  }
  public String getSaveButtonToolTipText()
  {
    return this.saveButtonToolTipText;
  }
  public int getUpdateButtonMnemonic()
  {
    return this.updateButtonMnemonic;
  }
  public String getUpdateButtonText()
  {
    return this.updateButtonText;
  }
  public String getUpdateButtonToolTipText()
  {
    return this.updateButtonToolTipText;
  }
  public void installStrings(JFileChooser fc)
  {
    super.installStrings(fc);
  }
  public void uninstallStrings(JFileChooser fc)
  {
    super.uninstallStrings(fc);
  }
  public void installIcons(JFileChooser fc) 
  {
    super.installIcons(fc);
  }
  public void uninstallIcons(JFileChooser fc)
  {
    super.uninstallIcons(fc);
  }
  public Icon getComputerIcon()
  {
    return this.computerIcon;
  }
  public Icon getDetailsViewIcon()
  {
    return this.detailsViewIcon;
  }
  public Icon getDirectoryIcon()
  {
    return this.directoryIcon;
  }
  public Icon getFileIcon()
  {
    return this.fileIcon;
  }
  public Icon getFloppyDriveIcon()
  {
    return this.floppyDriveIcon;
  }
  public Icon getHardDriveIcon()
  {
    return this.hardDriveIcon;
  }
  public Icon getHomeFolderIcon()
  {
    return this.homeFolderIcon;
  }
  public Icon getListViewIcon()
  {
    return this.listViewIcon;
  }
  public Icon getNewFolderIcon()
  {
    return this.newFolderIcon;
  }
  public Icon getUpFolderIcon()
  {
    return this.upFolderIcon;
  }
}

