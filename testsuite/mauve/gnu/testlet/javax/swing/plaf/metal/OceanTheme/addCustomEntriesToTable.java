/* addCustomEntriesToTable.java -- some checks for the 
       addCustomEntriesToTable() method in the OceanTheme class.
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

// Tags: JDK1.5

package gnu.testlet.javax.swing.plaf.metal.OceanTheme;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Color;
import java.awt.Insets;
import java.util.Arrays;

import javax.swing.UIDefaults;
import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.BorderUIResource.LineBorderUIResource;
import javax.swing.plaf.metal.OceanTheme;

public class addCustomEntriesToTable implements Testlet
{
  public void test(TestHarness harness)
  {
    UIDefaults defaults = new UIDefaults();
    OceanTheme theme = new OceanTheme();
    theme.addCustomEntriesToTable(defaults);
    
    harness.check(defaults.get("Button.disabledToolBarBorderBackground"), 
            new ColorUIResource(204, 204, 204));
    harness.check(defaults.get("Button.gradient"), Arrays.asList(new Object[]
        {new Float(0.3), new Float(0.0), new ColorUIResource(221, 232, 243),
         new ColorUIResource(Color.WHITE), new ColorUIResource(184, 207, 229)})); 
    harness.check(defaults.get("Button.rollover"), Boolean.TRUE); 
    harness.check(defaults.get("Button.rolloverIconType"), "ocean"); 
    harness.check(defaults.get("Button.toolBarBorderBackground"), 
        new ColorUIResource(153, 153, 153)); 
    harness.check(defaults.get("CheckBox.gradient"), Arrays.asList(new Object[]
        {new Float(0.3), new Float(0.0), new ColorUIResource(221, 232, 243),
         new ColorUIResource(Color.WHITE), new ColorUIResource(184, 207, 229)})); 
    harness.check(defaults.get("CheckBox.rollover"), Boolean.TRUE); // -> true
    harness.check(defaults.get("CheckBoxMenuItem.gradient"), Arrays.asList(
        new Object[] {new Float(0.3), new Float(0.0), 
        new ColorUIResource(221, 232, 243), new ColorUIResource(Color.WHITE), 
        new ColorUIResource(184, 207, 229)})); 
    //harness.check(defaults.get("FileChooser.homeFolderIcon"), null); // -> javax.swing.plaf.IconUIResource@5210f6d3
    //harness.check(defaults.get("FileChooser.newFolderIcon"), null); // -> javax.swing.plaf.IconUIResource@99b5393
    //harness.check(defaults.get("FileChooser.upFolderIcon"), null); // -> javax.swing.plaf.IconUIResource@732b3d53
    //harness.check(defaults.get("FileView.computerIcon"), null); // -> javax.swing.plaf.IconUIResource@73d6776d
    //harness.check(defaults.get("FileView.directoryIcon"), null); // -> javax.swing.plaf.IconUIResource@65faba46
    //harness.check(defaults.get("FileView.fileIcon"), null); // -> javax.swing.plaf.IconUIResource@77d80e6d
    //harness.check(defaults.get("FileView.floppyDriveIcon"), null); // -> javax.swing.plaf.IconUIResource@7e5a9de6
    //harness.check(defaults.get("FileView.hardDriveIcon"), null); // -> javax.swing.plaf.IconUIResource@6686fe26
    harness.check(defaults.get("InternalFrame.activeTitleGradient"), 
        Arrays.asList(new Object[] {new Float(0.3), new Float(0.0), 
        new ColorUIResource(221, 232, 243), new ColorUIResource(Color.WHITE), 
        new ColorUIResource(184, 207, 229)})); 
    //harness.check(defaults.get("InternalFrame.closeIcon"), null); // -> javax.swing.plaf.metal.OceanTheme$IFIcon@2fdb7df8
    //harness.check(defaults.get("InternalFrame.icon"), null); // -> javax.swing.plaf.IconUIResource@69f78ef1
    //harness.check(defaults.get("InternalFrame.iconifyIcon"), null); // -> javax.swing.plaf.metal.OceanTheme$IFIcon@7bc9a690
    //harness.check(defaults.get("InternalFrame.maximizeIcon"), null); // -> javax.swing.plaf.metal.OceanTheme$IFIcon@5f7a8a02
    //harness.check(defaults.get("InternalFrame.minimizeIcon"), null); // -> javax.swing.plaf.metal.OceanTheme$IFIcon@74b2002f
    //harness.check(defaults.get("InternalFrame.paletteCloseIcon"), null); // -> javax.swing.plaf.metal.OceanTheme$IFIcon@432a0f6c
    harness.check(defaults.get("Label.disabledForeground"), 
        new ColorUIResource(153, 153, 153));
    LineBorderUIResource border = (LineBorderUIResource) defaults.get(
        "List.focusCellHighlightBorder");
    harness.check(border.getThickness(), 1);
    harness.check(border.getLineColor(), new ColorUIResource(99, 130, 191));
    harness.check(defaults.get("MenuBar.borderColor"), 
        new ColorUIResource(204, 204, 204)); 
    harness.check(defaults.get("MenuBar.gradient"), Arrays.asList(new Object[]
        {new Float(1.0), new Float(0.0), new ColorUIResource(Color.WHITE),
         new ColorUIResource(218, 218, 218), 
         new ColorUIResource(218, 218, 218)}));
    harness.check(defaults.get("MenuBarUI"), 
        "javax.swing.plaf.metal.MetalMenuBarUI"); 
    harness.check(defaults.get("Menu.opaque"), Boolean.FALSE); 
    //harness.check(defaults.get("OptionPane.errorIcon"), null); // -> javax.swing.plaf.IconUIResource@151a64ed
    //harness.check(defaults.get("OptionPane.informationIcon"), null); // -> javax.swing.plaf.IconUIResource@53ad085
    //harness.check(defaults.get("OptionPane.questionIcon"), null); // -> javax.swing.plaf.IconUIResource@4ba33d48
    //harness.check(defaults.get("OptionPane.warningIcon"), null); // -> javax.swing.plaf.IconUIResource@392d263f
    harness.check(defaults.get("RadioButton.gradient"), Arrays.asList(
        new Object[] {new Float(0.3), new Float(0.0), 
        new ColorUIResource(221, 232, 243), new ColorUIResource(Color.WHITE), 
        new ColorUIResource(184, 207, 229)})); 
    harness.check(defaults.get("RadioButton.rollover"), Boolean.TRUE); 
    harness.check(defaults.get("RadioButtonMenuItem.gradient"), Arrays.asList(
        new Object[] {new Float(0.3), new Float(0.0), 
        new ColorUIResource(221, 232, 243), new ColorUIResource(Color.WHITE), 
        new ColorUIResource(184, 207, 229)})); 
    harness.check(defaults.get("ScrollBar.gradient"), Arrays.asList(
        new Object[] {new Float(0.3), new Float(0.0), 
        new ColorUIResource(221, 232, 243), new ColorUIResource(Color.WHITE), 
        new ColorUIResource(184, 207, 229)})); 
    harness.check(defaults.get("Slider.altTrackColor"), 
        new ColorUIResource(210, 226, 239)); 
    harness.check(defaults.get("Slider.focusGradient"), Arrays.asList(
        new Object[] {new Float(0.3), new Float(0.2), 
        new ColorUIResource(200, 221, 242), new ColorUIResource(Color.WHITE), 
        new ColorUIResource(184, 207, 229)})); 
    harness.check(defaults.get("Slider.gradient"), Arrays.asList(new Object[]
        {new Float(0.3), new Float(0.2), new ColorUIResource(200, 221, 242),
         new ColorUIResource(Color.WHITE), 
         new ColorUIResource(184, 207, 229)}));
    harness.check(defaults.get("SplitPane.dividerFocusColor"), 
        new ColorUIResource(200, 221, 242)); 
    harness.check(defaults.get("SplitPane.oneTouchButtonsOpaque"), 
        Boolean.FALSE); 
    harness.check(defaults.get("TabbedPane.borderHightlightColor"), 
        new ColorUIResource(99, 130, 191));
    harness.check(defaults.get("TabbedPane.contentAreaColor"), 
        new ColorUIResource(200, 221, 242)); 
    harness.check(defaults.get("TabbedPane.contentBorderInsets"), 
        new Insets(4, 2, 3, 3)); 
    harness.check(defaults.get("TabbedPane.tabAreaBackground"), 
        new ColorUIResource(218, 218, 218)); 
    harness.check(defaults.get("TabbedPane.tabAreaInsets"), 
        new Insets(2, 2, 0, 6)); 
    harness.check(defaults.get("TabbedPane.selected"), 
        new ColorUIResource(200, 221, 242)); 
    harness.check(defaults.get("TabbedPane.unselectedBackground"), 
        new ColorUIResource(238, 238, 238)); 
    harness.check(defaults.get("Table.focusCellHighlightBorder") 
        instanceof LineBorderUIResource); 
    harness.check(defaults.get("Table.gridColor"), 
        new ColorUIResource(122, 138, 153)); 
    harness.check(defaults.get("ToggleButton.gradient"), 
        Arrays.asList(new Object[] {new Float(0.3), new Float(0.0), 
        new ColorUIResource(221, 232, 243), new ColorUIResource(Color.WHITE), 
        new ColorUIResource(184, 207, 229)}));
    harness.check(defaults.get("ToolBar.borderColor"), 
        new ColorUIResource(204, 204, 204)); 
    harness.check(defaults.get("ToolBar.isRollover"), Boolean.TRUE); 
    //harness.check(defaults.get("Tree.expandedIcon"), null); // -> javax.swing.plaf.IconUIResource@2911a3a4
    //harness.check(defaults.get("Tree.leafIcon"), null); // -> javax.swing.plaf.IconUIResource@5ca352a5
    //harness.check(defaults.get("Tree.closedIcon"), null); // -> javax.swing.plaf.IconUIResource@584fce71
    //harness.check(defaults.get("Tree.collapsedIcon"), null); // -> javax.swing.plaf.metal.OceanTheme$COIcon@56406199
    //harness.check(defaults.get("Tree.openIcon"), null); // -> javax.swing.plaf.IconUIResource@57bcc0bc
    harness.check(defaults.get("Tree.selectionBorderColor"), 
        new ColorUIResource(99, 130, 191)); 
  }
}
