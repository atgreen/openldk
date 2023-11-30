// Tags: JDK1.3
// Uses: MyMetalLookAndFeel

// Copyright (C) 2005, 2006, David Gilbert <david.gilbert@object-refinery.com>

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

package gnu.testlet.javax.swing.plaf.metal.MetalLookAndFeel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Insets;
import java.util.Arrays;
import java.util.List;

import javax.swing.Icon;
import javax.swing.InputMap;
import javax.swing.KeyStroke;
import javax.swing.ListCellRenderer;
import javax.swing.UIDefaults;
import javax.swing.border.Border;
import javax.swing.border.LineBorder;
import javax.swing.plaf.BorderUIResource;
import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.DimensionUIResource;
import javax.swing.plaf.FontUIResource;
import javax.swing.plaf.InputMapUIResource;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.IconUIResource;
import javax.swing.plaf.BorderUIResource.LineBorderUIResource;
import javax.swing.plaf.basic.BasicBorders;
import javax.swing.plaf.metal.DefaultMetalTheme;
import javax.swing.plaf.metal.MetalBorders;
import javax.swing.plaf.metal.MetalIconFactory;
import javax.swing.plaf.metal.MetalLookAndFeel;

/**
 * Some checks for the initComponentDefaults() method in the 
 * {@link MetalLookAndFeel} class.  
 */
public class getDefaults implements Testlet
{
  /**
   * This extends the default theme, so that each theme color gets a
   * unique value. This allows to check the color values of the UIDefaults
   * against the theme colors.
   */
  static class TestTheme extends DefaultMetalTheme
  {
    public ColorUIResource getAcceleratorForeground()
    {
      return new ColorUIResource(0, 0, 1);
    }
    public ColorUIResource getAcceleratorSelectedForeground()
    {
      return new ColorUIResource(0, 0, 2);
    }
    public ColorUIResource getBlack()
    {
      return new ColorUIResource(0, 0, 3);
    }
    public ColorUIResource getControl()
    {
      return new ColorUIResource(0, 0, 4);
    }
    public ColorUIResource getControlDarkShadow()
    {
      return new ColorUIResource(0, 0, 5);
    }
    public ColorUIResource getControlDisabled()
    {
      return new ColorUIResource(0, 0, 6);
    }
    public ColorUIResource getControlHighlight()
    {
      return new ColorUIResource(0, 0, 7);
    }
    public ColorUIResource getControlInfo()
    {
      return new ColorUIResource(0, 0, 8);
    }
    public ColorUIResource getControlShadow()
    {
      return new ColorUIResource(0, 0, 9);
    }
    public ColorUIResource getControlTextColor()
    {
      return new ColorUIResource(0, 0, 10);
    }
    public ColorUIResource getDesktopColor()
    {
      return new ColorUIResource(0, 0, 11);
    }
    public ColorUIResource getFocusColor()
    {
      return new ColorUIResource(0, 0, 12);
    }
    public ColorUIResource getHighlightedTextColor()
    {
      return new ColorUIResource(0, 0, 13);
    }
    public ColorUIResource getInactiveSystemTextColor()
    {
      return new ColorUIResource(0, 0, 14);
    }
    public ColorUIResource getMenuBackground()
    {
      return new ColorUIResource(0, 0, 15);
    }
    public ColorUIResource getMenuDisabledForeground()
    {
      return new ColorUIResource(0, 0, 16);
    }
    public ColorUIResource getMenuForeground()
    {
      return new ColorUIResource(0, 0, 17);
    }
    public ColorUIResource getMenuSelectedBackground()
    {
      return new ColorUIResource(0, 0, 18);
    }
    public ColorUIResource getMenuSelectedForeground()
    {
      return new ColorUIResource(0, 0, 19);
    }
    public ColorUIResource getPrimaryControl()
    {
      return new ColorUIResource(0, 0, 20);
    }
    public ColorUIResource getPrimaryControlDarkShadow()
    {
      return new ColorUIResource(0, 0, 21);
    }
    public ColorUIResource getPrimaryControlHighlight()
    {
      return new ColorUIResource(0, 0, 22);
    }
    public ColorUIResource getPrimaryControlInfo()
    {
      return new ColorUIResource(0, 0, 23);
    }
    public ColorUIResource getPrimaryControlShadow()
    {
      return new ColorUIResource(0, 0, 24);
    }
    public ColorUIResource getSeparatorBackground()
    {
      return new ColorUIResource(0, 0, 25);
    }
    public ColorUIResource getSeparatorForeground()
    {
      return new ColorUIResource(0, 0, 26);
    }
    public ColorUIResource getSystemTextColor()
    {
      return new ColorUIResource(0, 0, 27);
    }
    public ColorUIResource getTextHighlightColor()
    {
      return new ColorUIResource(0, 0, 28);
    }
    public ColorUIResource getUserTextColor()
    {
      return new ColorUIResource(0, 0, 29);
    }
    public ColorUIResource getWindowBackground()
    {
      return new ColorUIResource(0, 0, 30);
    }
    public ColorUIResource getWindowTitleBackground()
    {
      return new ColorUIResource(0, 0, 31);
    }
    public ColorUIResource getWindowTitleForeground()
    {
      return new ColorUIResource(0, 0, 32);
    }
    public ColorUIResource getWindowTitleInactiveBackground()
    {
      return new ColorUIResource(0, 0, 33);
    }
    public ColorUIResource getWindowTitleInactiveForeground()
    {
      return new ColorUIResource(0, 0, 34);
    }
    public ColorUIResource getInactiveControlTextColor()
    {
      return new ColorUIResource(0, 0, 35);
    }
    public FontUIResource getControlTextFont() 
    {
      return new FontUIResource("Dialog", Font.PLAIN, 13);
    }
    public FontUIResource getMenuTextFont() 
    {
      return new FontUIResource("Dialog", Font.PLAIN, 15);
    }
  }

  /**
   * Runs the test using the specified harness.  
   * 
   * @param harness  the test harness (<code>null</code> not allowed).
   */
  public void test(TestHarness harness) 
  {
    MyMetalLookAndFeel.setCurrentTheme(new TestTheme());
    MyMetalLookAndFeel laf = new MyMetalLookAndFeel();

    // The following does not work, at least not with JDK1.5. Maybe
    // don't use the 'defaults' parameter anymore...
    // UIDefaults defaults = new UIDefaults();
    // laf.initComponentDefaults(defaults);
    UIDefaults defaults = laf.getDefaults();

    // TODO: in the following code, there are many 'instanceof' checks - these
    // are typically very weak tests.  Maybe they can be strengthened...
    // The color tests do not test for the real color values. This is
    // not possible. It merely tests which of the MetalTheme colors
    // is use here. See the TestTheme class above.
    harness.checkPoint("AuditoryCues");
    harness.check(defaults.get("AuditoryCues.allAuditoryCues") != null);
    harness.check(defaults.get("AuditoryCues.cueList") != null);
    harness.check(defaults.get("AuditoryCues.defaultCueList") != null);
    harness.check(defaults.get("AuditoryCues.noAuditoryCues") != null);
    
    harness.checkPoint("Button");
    harness.check(defaults.get("Button.background"), new ColorUIResource(0, 0, 4));
    harness.check(defaults.get("Button.border") instanceof BorderUIResource.CompoundBorderUIResource);
    harness.check(defaults.get("Button.darkShadow"), new ColorUIResource(0, 0, 5));
    harness.check(defaults.get("Button.disabledText"), new ColorUIResource(0, 0, 35));
    harness.check(defaults.get("Button.disabledToolBarBorderBackground"), null);
    harness.check(defaults.get("Button.focus"), new ColorUIResource(0, 0, 12));
    harness.check(defaults.get("Button.focusInputMap") instanceof InputMapUIResource);
    harness.check(defaults.get("Button.font"), new FontUIResource("Dialog", Font.PLAIN, 13));
    harness.check(defaults.get("Button.foreground"), new ColorUIResource(0, 0, 10));
    harness.check(defaults.get("Button.highlight"), new ColorUIResource(0, 0, 7));
    harness.check(defaults.get("Button.light"), new ColorUIResource(0, 0, 7));
    Insets buttonMargin = defaults.getInsets("Button.margin");
    harness.check(buttonMargin instanceof UIResource);
    harness.check(buttonMargin, new InsetsUIResource(2, 14, 2, 14));
    harness.check(defaults.get("Button.select"), new ColorUIResource(0, 0, 9));
    harness.check(defaults.get("Button.shadow"), new ColorUIResource(0, 0, 9));
    harness.check(defaults.get("Button.textIconGap"), new Integer(4));
    harness.check(defaults.get("Button.textShiftOffset"), new Integer(0));
    harness.check(defaults.get("Button.toolBarBorderBackground"), null);

    harness.checkPoint("CheckBox");
    harness.check(defaults.get("CheckBox.background"), new ColorUIResource(0, 0, 4));
    harness.check(defaults.get("CheckBox.border") instanceof BorderUIResource.CompoundBorderUIResource);
    harness.check(defaults.get("CheckBox.disabledText"), new ColorUIResource(0, 0, 35));
    harness.check(defaults.get("CheckBox.focus"), new ColorUIResource(0, 0, 12));
    harness.check(defaults.get("CheckBox.focusInputMap") instanceof InputMapUIResource);
    harness.check(defaults.get("CheckBox.font"), new FontUIResource("Dialog", Font.PLAIN, 13));
    harness.check(defaults.get("CheckBox.foreground"), new ColorUIResource(0, 0, 10));
    harness.check(defaults.get("CheckBox.icon") instanceof Icon);
    harness.check(defaults.get("CheckBox.margin"), new InsetsUIResource(2, 2, 2, 2));
    harness.check(defaults.get("Checkbox.select"), new ColorUIResource(0, 0, 9));
    harness.check(defaults.get("CheckBox.textIconGap"), new Integer(4));
    harness.check(defaults.get("CheckBox.textShiftOffset"), new Integer(0));

    harness.checkPoint("CheckBoxMenuItem");
    harness.check(defaults.get("CheckBoxMenuItem.acceleratorFont"), new Font("Dialog", Font.PLAIN, 10));
    harness.check(defaults.get("CheckBoxMenuItem.acceleratorForeground"), new ColorUIResource(0, 0, 1));
    harness.check(defaults.get("CheckBoxMenuItem.acceleratorSelectionForeground"), new ColorUIResource(0, 0, 2));
    harness.check(defaults.get("CheckBoxMenuItem.arrowIcon") instanceof Icon);
    harness.check(defaults.get("CheckBoxMenuItem.background"), new ColorUIResource(0, 0, 15));
//    harness.check(defaults.get("CheckBoxMenuItem.border") instanceof MetalBorders.MenuItemBorder); 
    harness.check(defaults.get("CheckBoxMenuItem.borderPainted"), Boolean.TRUE);
    harness.check(defaults.get("CheckBoxMenuItem.checkIcon") instanceof Icon);
    harness.check(defaults.get("CheckBoxMenuItem.commandSound"), "sounds/MenuItemCommand.wav");
    harness.check(defaults.get("CheckBoxMenuItem.disabledForeground"), new ColorUIResource(0, 0, 16));
    harness.check(defaults.get("CheckBoxMenuItem.font"), new FontUIResource("Dialog", Font.PLAIN, 15));
    harness.check(defaults.get("CheckBoxMenuItem.foreground"), new ColorUIResource(0, 0, 17));
    harness.check(defaults.get("CheckBoxMenuItem.margin"), new InsetsUIResource(2, 2, 2, 2));
    harness.check(defaults.get("CheckBoxMenuItem.selectionBackground"), new ColorUIResource(0, 0, 18));
    harness.check(defaults.get("CheckBoxMenuItem.selectionForeground"), new ColorUIResource(0, 0, 19));
    harness.check(defaults.get("CheckBoxMenuItem.select"), null);

    harness.checkPoint("ColorChooser");
    harness.check(defaults.get("ColorChooser.background"), new ColorUIResource(0, 0, 4));
    harness.check(defaults.get("ColorChooser.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("ColorChooser.foreground"), new ColorUIResource(0, 0, 10));
    harness.check(defaults.getInt("ColorChooser.rgbBlueMnemonic"), 0);
    harness.check(defaults.getInt("ColorChooser.rgbGreenMnemonic"), 0);
    harness.check(defaults.getInt("ColorChooser.rgbRedMnemonic"), 0);
    harness.check(defaults.get("ColorChooser.swatchesDefaultRecentColor"), new ColorUIResource(0, 0, 4));
    harness.check(defaults.get("ColorChooser.swatchesRecentSwatchSize"), new Dimension(10, 10));
    harness.check(defaults.get("ColorChooser.swatchesSwatchSize"), new Dimension(10, 10));

    harness.checkPoint("ComboBox");
    harness.check(defaults.get("ComboBox.ancestorInputMap") instanceof InputMapUIResource);
    harness.check(defaults.get("ComboBox.background"), new ColorUIResource(0, 0, 4));
    harness.check(defaults.get("ComboBox.buttonBackground"), new ColorUIResource(0, 0, 4));
    harness.check(defaults.get("ComboBox.buttonDarkShadow"), new ColorUIResource(0, 0, 5));
    harness.check(defaults.get("ComboBox.buttonHighlight"), new ColorUIResource(0, 0, 7));
    harness.check(defaults.get("ComboBox.buttonShadow"), new ColorUIResource(0, 0, 9));
    harness.check(defaults.get("ComboBox.disabledBackground"), new ColorUIResource(0, 0, 4));
    harness.check(defaults.get("ComboBox.disabledForeground"), new ColorUIResource(0, 0, 14));
    harness.check(defaults.get("ComboBox.font"), new FontUIResource("Dialog", Font.PLAIN, 13));
    harness.check(defaults.get("ComboBox.foreground"), new ColorUIResource(0, 0, 10));
    harness.check(defaults.get("ComboBox.selectionBackground"), new ColorUIResource(0, 0, 24));
    harness.check(defaults.get("ComboBox.selectionForeground"), new ColorUIResource(0, 0, 10));
    
    harness.checkPoint("Desktop");
    harness.check(defaults.get("Desktop.ancestorInputMap") instanceof InputMapUIResource);
    harness.check(defaults.get("Desktop.background"), new ColorUIResource(0, 0, 11));
    
    harness.checkPoint("DesktopIcon");
    harness.check(defaults.get("DesktopIcon.background"), new ColorUIResource(0, 0, 4));
    harness.check(defaults.get("DesktopIcon.border") instanceof BorderUIResource.CompoundBorderUIResource); 
    harness.check(defaults.get("DesktopIcon.font"), new FontUIResource("Dialog", Font.PLAIN, 13));
    harness.check(defaults.get("DesktopIcon.foreground"), new ColorUIResource(0, 0, 10));
    harness.check(defaults.getInt("DesktopIcon.width"), 160);
    
    harness.checkPoint("EditorPane");
    harness.check(defaults.get("EditorPane.background"), new ColorUIResource(0, 0, 30));
    harness.check(defaults.get("EditorPane.border") instanceof BasicBorders.MarginBorder); 
    harness.check(defaults.getInt("EditorPane.caretBlinkRate"), 500);
    harness.check(defaults.get("EditorPane.caretForeground"), new ColorUIResource(0, 0, 29));
    harness.check(defaults.get("EditorPane.focusInputMap") instanceof InputMapUIResource);
    harness.check(defaults.get("EditorPane.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("EditorPane.foreground"), new ColorUIResource(0, 0, 29));
    harness.check(defaults.get("EditorPane.inactiveForeground"), new ColorUIResource(0, 0, 14));
    harness.check(defaults.get("EditorPane.margin"), new InsetsUIResource(3, 3, 3, 3));
    harness.check(defaults.get("EditorPane.selectionBackground"), new ColorUIResource(0, 0, 28));
    harness.check(defaults.get("EditorPane.selectionForeground"), new ColorUIResource(0, 0, 13));
    
    harness.checkPoint("FileChooser");
    harness.check(defaults.get("FileChooser.ancestorInputMap") instanceof InputMapUIResource);
    harness.check(defaults.getInt("FileChooser.cancelButtonMnemonic"), 0);
    harness.check(defaults.get("FileChooser.detailsViewIcon") instanceof Icon);
    harness.check(defaults.getInt("FileChooser.directoryOpenButtonMnemonic"), 0);
    harness.check(defaults.getInt("FileChooser.fileNameLabelMnemonic"), 78);
    harness.check(defaults.getInt("FileChooser.filesOfTypeLabelMnemonic"), 84);
    harness.check(defaults.getInt("FileChooser.helpButtonMnemonic"), 0);
    harness.check(defaults.get("FileChooser.homeFolderIcon") instanceof Icon);
    harness.check(defaults.get("FileChooser.listViewIcon") instanceof Icon);
    harness.check(defaults.getInt("FileChooser.lookInLabelMnemonic"), 73);
    harness.check(defaults.get("FileChooser.newFolderIcon") instanceof Icon);
    harness.check(defaults.getInt("FileChooser.openButtonMnemonic"), 0);
    harness.check(defaults.getInt("FileChooser.saveButtonMnemonic"), 0);
    harness.check(defaults.getInt("FileChooser.updateButtonMnemonic"), 0);
    harness.check(defaults.get("FileChooser.upFolderIcon") instanceof Icon);
    
    harness.checkPoint("FileView");
    harness.check(defaults.get("FileView.computerIcon"), MetalIconFactory.getTreeComputerIcon());
    harness.check(defaults.get("FileView.directoryIcon") instanceof MetalIconFactory.TreeFolderIcon);
    harness.check(defaults.get("FileView.fileIcon") instanceof MetalIconFactory.TreeLeafIcon);
    harness.check(defaults.get("FileView.floppyDriveIcon"), MetalIconFactory.getTreeFloppyDriveIcon());
    harness.check(defaults.get("FileView.hardDriveIcon"), MetalIconFactory.getTreeHardDriveIcon());
    
    harness.checkPoint("FormattedTextField");
    harness.check(defaults.get("FormattedTextField.background"), new ColorUIResource(0, 0, 30));  
    harness.check(defaults.get("FormattedTextField.border") instanceof BorderUIResource.CompoundBorderUIResource);  
    harness.check(defaults.getInt("FormattedTextField.caretBlinkRate"), 500);
    harness.check(defaults.get("FormattedTextField.caretForeground"), new ColorUIResource(0, 0, 29));  
    harness.check(defaults.get("FormattedTextField.focusInputMap") instanceof InputMapUIResource);
    harness.check(defaults.get("FormattedTextField.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("FormattedTextField.foreground"), new ColorUIResource(0, 0, 29));  
    harness.check(defaults.get("FormattedTextField.inactiveBackground"), new ColorUIResource(0, 0, 4));  
    harness.check(defaults.get("FormattedTextField.inactiveForeground"), new ColorUIResource(0, 0, 14));  
    harness.check(defaults.get("FormattedTextField.margin"), new InsetsUIResource(0, 0, 0, 0));
    harness.check(defaults.get("FormattedTextField.selectionBackground"), new ColorUIResource(0, 0, 28));  
    harness.check(defaults.get("FormattedTextField.selectionForeground"), new ColorUIResource(0, 0, 13));  
    
    harness.checkPoint("InternalFrame");
//    harness.check(defaults.get("InternalFrame.border") instanceof MetalBorders.InternalFrameBorder); 
    harness.check(defaults.get("InternalFrame.activeTitleBackground"), new ColorUIResource(0, 0, 31));
    harness.check(defaults.get("InternalFrame.activeTitleForeground"), new ColorUIResource(0, 0, 32));
    harness.check(defaults.get("InternalFrame.borderColor"), new ColorUIResource(0, 0, 4));
    harness.check(defaults.get("InternalFrame.borderDarkShadow"), new ColorUIResource(0, 0, 5));
    harness.check(defaults.get("InternalFrame.borderHighlight"), new ColorUIResource(0, 0, 7));
    harness.check(defaults.get("InternalFrame.borderLight"), new ColorUIResource(0, 0, 7));
    harness.check(defaults.get("InternalFrame.borderShadow"), new ColorUIResource(0, 0, 9));
    harness.check(defaults.get("InternalFrame.closeIcon") instanceof Icon);
    harness.check(defaults.get("InternalFrame.closeSound"), "sounds/FrameClose.wav");
    harness.check(defaults.get("InternalFrame.icon") instanceof Icon);
    harness.check(defaults.get("InternalFrame.iconifyIcon") instanceof Icon);
    harness.check(defaults.get("InternalFrame.inactiveTitleBackground"), new ColorUIResource(0, 0, 33));
    harness.check(defaults.get("InternalFrame.inactiveTitleForeground"), new ColorUIResource(0, 0, 34));
    harness.check(defaults.get("InternalFrame.maximizeIcon") instanceof Icon);
    harness.check(defaults.get("InternalFrame.maximizeSound"), "sounds/FrameMaximize.wav");
    harness.check(defaults.get("InternalFrame.minimizeIcon") instanceof Icon);
    harness.check(defaults.get("InternalFrame.minimizeSound"), "sounds/FrameMinimize.wav");
//    harness.check(defaults.get("InternalFrame.optionDialogBorder") instanceof MetalBorders.OptionDialogBorder);
//    harness.check(defaults.get("InternalFrame.paletteBorder") instanceof MetalBorders.PaletteBorder);
    harness.check(defaults.get("InternalFrame.paletteCloseIcon") instanceof Icon);
    harness.check(defaults.getInt("InternalFrame.paletteTitleHeight"), 11);
    harness.check(defaults.get("InternalFrame.restoreDownSound"), "sounds/FrameRestoreDown.wav");
    harness.check(defaults.get("InternalFrame.restoreUpSound"), "sounds/FrameRestoreUp.wav");
    harness.check(defaults.get("InternalFrame.titleFont"), new FontUIResource("Dialog", Font.BOLD, 12));
    
    harness.checkPoint("Label");
    harness.check(defaults.get("Label.background"), new ColorUIResource(0, 0, 4));
    harness.check(defaults.get("Label.disabledForeground"), new ColorUIResource(0, 0, 14));
    harness.check(defaults.get("Label.disabledShadow"), new ColorUIResource(0, 0, 9));
    harness.check(defaults.get("Label.font"), new FontUIResource("Dialog", Font.PLAIN, 13));
    harness.check(defaults.get("Label.foreground"), new ColorUIResource(0, 0, 27));
    
    harness.checkPoint("List");
    harness.check(defaults.get("List.background"), new ColorUIResource(0, 0, 30));
    harness.check(defaults.get("List.cellRenderer") instanceof ListCellRenderer);
    LineBorder lb = (LineBorder) defaults.getBorder("List.focusCellHighlightBorder");
    harness.check(lb instanceof BorderUIResource.LineBorderUIResource);
    harness.check(lb.getThickness(), 1);
    harness.check(lb.getLineColor(), MetalLookAndFeel.getFocusColor());
    harness.check(defaults.get("List.focusInputMap") instanceof InputMapUIResource);
    harness.check(defaults.get("List.focusInputMap.RightToLeft") instanceof InputMapUIResource);
    harness.check(defaults.get("List.font"), new FontUIResource("Dialog", Font.PLAIN, 13));
    harness.check(defaults.get("List.foreground"), new ColorUIResource(0, 0, 29));
    harness.check(defaults.get("List.selectionBackground"), new ColorUIResource(0, 0, 28));
    harness.check(defaults.get("List.selectionForeground"), new ColorUIResource(0, 0, 13));
    
    harness.checkPoint("Menu");
    harness.check(defaults.get("Menu.acceleratorFont"), new FontUIResource("Dialog", Font.PLAIN, 10));
    harness.check(defaults.get("Menu.acceleratorForeground"), new ColorUIResource(0, 0, 1));
    harness.check(defaults.get("Menu.acceleratorSelectionForeground"), new ColorUIResource(0, 0, 2));
    harness.check(defaults.get("Menu.arrowIcon") instanceof Icon); 
//    harness.check(defaults.get("Menu.border") instanceof MetalBorders.MenuItemBorder);
    harness.check(defaults.get("Menu.background"), new ColorUIResource(0, 0, 15));
    harness.check(defaults.get("Menu.borderPainted"), Boolean.TRUE);
    harness.check(defaults.get("Menu.checkIcon"), null);
    harness.check(defaults.get("Menu.crossMenuMnemonic"), Boolean.TRUE);
    harness.check(defaults.get("Menu.disabledForeground"), new ColorUIResource(0, 0, 16));
    harness.check(defaults.get("Menu.font"), new FontUIResource("Dialog", Font.PLAIN, 15));
    harness.check(defaults.get("Menu.foreground"), new ColorUIResource(0, 0, 17));
    harness.check(defaults.get("Menu.margin"), new InsetsUIResource(2, 2, 2, 2));
    harness.check(defaults.getInt("Menu.menuPopupOffsetX"), 0);
    harness.check(defaults.getInt("Menu.menuPopupOffsetY"), 0);
    harness.check(defaults.get("Menu.selectionBackground"), new ColorUIResource(0, 0, 18));
    harness.check(defaults.get("Menu.selectionForeground"), new ColorUIResource(0, 0, 19));
    int[] value = (int[]) defaults.get("Menu.shortcutKeys");
    harness.check(value != null ? value.length : 0, 1);
    harness.check(value != null ? value[0] : 0, 8);
    harness.check(defaults.getInt("Menu.submenuPopupOffsetX"), -4);
    harness.check(defaults.getInt("Menu.submenuPopupOffsetY"), -3);
    
    harness.checkPoint("MenuBar");
//    harness.check(defaults.get("MenuBar.border") instanceof MetalBorders.MenuBarBorder);
    harness.check(defaults.get("MenuBar.background"), new ColorUIResource(0, 0, 15));
    harness.check(defaults.get("MenuBar.borderColor"), null);
    harness.check(defaults.get("MenuBar.font"), new FontUIResource("Dialog", Font.PLAIN, 15));
    harness.check(defaults.get("MenuBar.foreground"), new ColorUIResource(0, 0, 17));
    harness.check(defaults.get("MenuBar.highlight"), new ColorUIResource(0, 0, 7));
    harness.check(defaults.get("MenuBar.shadow"), new ColorUIResource(0, 0, 9));
    Object[] bindings = (Object[]) defaults.get("MenuBar.windowBindings");
    harness.check(bindings.length, 2);
    harness.check(bindings[0], "F10");
    harness.check(bindings[1], "takeFocus");
    
    harness.checkPoint("MenuItem");
    harness.check(defaults.get("MenuItem.acceleratorDelimiter"), "-");
    harness.check(defaults.get("MenuItem.acceleratorFont"), new FontUIResource("Dialog", Font.PLAIN, 10));
    harness.check(defaults.get("MenuItem.acceleratorForeground"), new ColorUIResource(0, 0, 1));
    harness.check(defaults.get("MenuItem.acceleratorSelectionForeground"), new ColorUIResource(0, 0, 2));
    harness.check(defaults.get("MenuItem.arrowIcon") instanceof Icon);
//    harness.check(defaults.get("MenuItem.border") instanceof MetalBorders.MenuItemBorder);
    harness.check(defaults.get("MenuItem.background"), new ColorUIResource(0, 0, 15));
    harness.check(defaults.get("MenuItem.borderPainted"), Boolean.TRUE);
    harness.check(defaults.get("MenuItem.checkIcon"), null);
    harness.check(defaults.get("MenuItem.commandSound"), "sounds/MenuItemCommand.wav");
    harness.check(defaults.get("MenuItem.disabledForeground"), new ColorUIResource(0, 0, 16));
    harness.check(defaults.get("MenuItem.font"), new FontUIResource("Dialog", Font.PLAIN, 15));
    harness.check(defaults.get("MenuItem.foreground"), new ColorUIResource(0, 0, 17));
    harness.check(defaults.get("MenuItem.margin"), new InsetsUIResource(2, 2, 2, 2));
    harness.check(defaults.get("MenuItem.selectionBackground"), new ColorUIResource(0, 0, 18));
    harness.check(defaults.get("MenuItem.selectionForeground"), new ColorUIResource(0, 0, 19));
    
    harness.checkPoint("OptionPane");
    harness.check(defaults.get("OptionPane.background"), new ColorUIResource(0, 0, 4));
    harness.check(defaults.get("OptionPane.border") instanceof BorderUIResource.EmptyBorderUIResource);
    harness.check(defaults.get("OptionPane.buttonAreaBorder") instanceof BorderUIResource.EmptyBorderUIResource);
    harness.check(defaults.getInt("OptionPane.buttonClickThreshhold"), 500);
    harness.check(defaults.get("OptionPane.errorDialog.border.background"), new ColorUIResource(153, 51, 51));
    harness.check(defaults.get("OptionPane.errorDialog.titlePane.background"), new ColorUIResource(255, 153, 153));
    harness.check(defaults.get("OptionPane.errorDialog.titlePane.foreground"), new ColorUIResource(51, 0, 0));
    harness.check(defaults.get("OptionPane.errorDialog.titlePane.shadow"), new ColorUIResource(204, 102, 102));
    harness.check(defaults.get("OptionPane.errorIcon") instanceof IconUIResource);
    harness.check(defaults.get("OptionPane.errorSound"), "sounds/OptionPaneError.wav");
    harness.check(defaults.get("OptionPane.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("OptionPane.foreground"), new ColorUIResource(0, 0, 10));
    harness.check(defaults.get("OptionPane.informationIcon") instanceof IconUIResource);
    harness.check(defaults.get("OptionPane.informationSound"), "sounds/OptionPaneInformation.wav");
    harness.check(defaults.get("OptionPane.messageAreaBorder") instanceof BorderUIResource.EmptyBorderUIResource);
    harness.check(defaults.get("OptionPane.messageForeground"), new ColorUIResource(0, 0, 10));
    harness.check(defaults.get("OptionPane.minimumSize"), new DimensionUIResource(262, 90));
    harness.check(defaults.get("OptionPane.questionDialog.border.background"), new ColorUIResource(51, 102, 51));
    harness.check(defaults.get("OptionPane.questionDialog.titlePane.background"), new ColorUIResource(153, 204, 153));
    harness.check(defaults.get("OptionPane.questionDialog.titlePane.foreground"), new ColorUIResource(0, 51, 0));
    harness.check(defaults.get("OptionPane.questionDialog.titlePane.shadow"), new ColorUIResource(102, 153, 102));
    harness.check(defaults.get("OptionPane.questionIcon") instanceof IconUIResource);
    harness.check(defaults.get("OptionPane.questionSound"), "sounds/OptionPaneQuestion.wav");
    harness.check(defaults.get("OptionPane.warningDialog.border.background"), new ColorUIResource(153, 102, 51));
    harness.check(defaults.get("OptionPane.warningDialog.titlePane.background"), new ColorUIResource(255, 204, 153));
    harness.check(defaults.get("OptionPane.warningDialog.titlePane.foreground"), new ColorUIResource(102, 51, 0));
    harness.check(defaults.get("OptionPane.warningDialog.titlePane.shadow"), new ColorUIResource(204, 153, 102));
    harness.check(defaults.get("OptionPane.warningIcon") instanceof IconUIResource);
    harness.check(defaults.get("OptionPane.warningSound"), "sounds/OptionPaneWarning.wav");
    bindings = (Object[]) defaults.get("OptionPane.windowBindings");
    harness.check(bindings.length, 2);
    harness.check(bindings[0], "ESCAPE");
    harness.check(bindings[1], "close");
    
    harness.checkPoint("Panel");
    harness.check(defaults.get("Panel.background"), new ColorUIResource(0, 0, 4));
    harness.check(defaults.get("Panel.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("Panel.foreground"), new ColorUIResource(0, 0, 29));
    
    harness.checkPoint("PasswordField");
    harness.check(defaults.get("PasswordField.background"), new ColorUIResource(0, 0, 30));
    harness.check(defaults.get("PasswordField.border"), MetalBorders.getTextBorder());
    harness.check(defaults.getInt("PasswordField.caretBlinkRate"), 500);
    harness.check(defaults.get("PasswordField.caretForeground"), new ColorUIResource(0, 0, 29));
    harness.check(defaults.get("PasswordField.focusInputMap") instanceof InputMapUIResource);
    harness.check(defaults.get("PasswordField.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("PasswordField.foreground"), new ColorUIResource(0, 0, 29));
    harness.check(defaults.get("PasswordField.inactiveBackground"), new ColorUIResource(0, 0, 4));
    harness.check(defaults.get("PasswordField.inactiveForeground"), new ColorUIResource(0, 0, 14));
    harness.check(defaults.get("PasswordField.margin"), new InsetsUIResource(0, 0, 0, 0));
    harness.check(defaults.get("PasswordField.selectionBackground"), new ColorUIResource(0, 0, 28));
    harness.check(defaults.get("PasswordField.selectionForeground"), new ColorUIResource(0, 0, 13));
    
    harness.checkPoint("PopupMenu");
    harness.check(defaults.get("PopupMenu.background"), new ColorUIResource(0, 0, 15));
    harness.check(defaults.get("PopupMenu.border") instanceof MetalBorders.PopupMenuBorder);
    harness.check(defaults.get("PopupMenu.font"), new FontUIResource("Dialog", Font.PLAIN, 15));
    harness.check(defaults.get("PopupMenu.foreground"), new ColorUIResource(0, 0, 17));
    harness.check(defaults.get("PopupMenu.popupSound"), "sounds/PopupMenuPopup.wav");
    harness.check(defaults.get("PopupMenu.selectedWindowInputMapBindings") instanceof Object[]);
    harness.check(defaults.get("PopupMenu.selectedWindowInputMapBindings.RightToLeft") instanceof Object[]);
    
    harness.checkPoint("ProgressBar");
    harness.check(defaults.get("ProgressBar.background"), new ColorUIResource(0, 0, 4));
    LineBorderUIResource b = (LineBorderUIResource) defaults.get("ProgressBar.border");
    harness.check(b.getThickness(), 1);
    harness.check(b.getLineColor(), new Color(0, 0, 5));
    harness.check(defaults.getInt("ProgressBar.cellLength"), 1);
    harness.check(defaults.getInt("ProgressBar.cellSpacing"), 0);
    harness.check(defaults.getInt("ProgressBar.cycleTime"), 3000);
    harness.check(defaults.get("ProgressBar.font"), new FontUIResource("Dialog", Font.PLAIN, 13));
    harness.check(defaults.get("ProgressBar.foreground"), new ColorUIResource(0, 0, 24));
    harness.check(defaults.getInt("ProgressBar.repaintInterval"), 50);
    harness.check(defaults.get("ProgressBar.selectionBackground"), new ColorUIResource(0, 0, 21));
    harness.check(defaults.get("ProgressBar.selectionForeground"), new ColorUIResource(0, 0, 4));
    
    harness.checkPoint("RadioButton");
    harness.check(defaults.get("RadioButton.background"), new ColorUIResource(0, 0, 4));
    harness.check(defaults.get("RadioButton.border") instanceof BorderUIResource.CompoundBorderUIResource);
    harness.check(defaults.get("RadioButton.darkShadow"), new ColorUIResource(0, 0, 5));
    harness.check(defaults.get("RadioButton.disabledText"), new ColorUIResource(0, 0, 35));
    harness.check(defaults.get("RadioButton.focus"), new ColorUIResource(0, 0, 12));
    harness.check(defaults.get("RadioButton.focusInputMap") instanceof InputMapUIResource);
    harness.check(defaults.get("RadioButton.font"), new FontUIResource("Dialog", Font.PLAIN, 13));
    harness.check(defaults.get("RadioButton.foreground"), new ColorUIResource(0, 0, 10));
    harness.check(defaults.get("RadioButton.highlight"), new ColorUIResource(0, 0, 7));
    harness.check(defaults.get("RadioButton.icon") instanceof Icon);
    harness.check(defaults.get("RadioButton.light"), new ColorUIResource(0, 0, 7));
    harness.check(defaults.get("RadioButton.margin"), new InsetsUIResource(2, 2, 2, 2));
    harness.check(defaults.get("RadioButton.select"), new ColorUIResource(0, 0, 9));
    harness.check(defaults.get("RadioButton.shadow"), new ColorUIResource(0, 0, 9));
    harness.check(defaults.getInt("RadioButton.textIconGap"), 4);
    harness.check(defaults.getInt("RadioButton.textShiftOffset"), 0);
    
    harness.checkPoint("RadioButtonMenuItem");
    harness.check(defaults.get("RadioButtonMenuItem.acceleratorFont"), new Font("Dialog", Font.PLAIN, 10));
    harness.check(defaults.get("RadioButtonMenuItem.acceleratorForeground"), new ColorUIResource(0, 0, 1));
    harness.check(defaults.get("RadioButtonMenuItem.acceleratorSelectionForeground"), new ColorUIResource(0, 0, 2));
    harness.check(defaults.get("RadioButtonMenuItem.arrowIcon") instanceof Icon);
//    harness.check(defaults.get("RadioButtonMenuItem.border") instanceof MetalBorders.MenuItemBorder);
    harness.check(defaults.get("RadioButtonMenuItem.background"), new ColorUIResource(0, 0, 15));
    harness.check(defaults.get("RadioButtonMenuItem.borderPainted"), Boolean.TRUE);
    harness.check(defaults.get("RadioButtonMenuItem.checkIcon") instanceof Icon);
    harness.check(defaults.get("RadioButtonMenuItem.commandSound"), "sounds/MenuItemCommand.wav");
    harness.check(defaults.get("RadioButtonMenuItem.disabledForeground"), new ColorUIResource(0, 0, 16));
    harness.check(defaults.get("RadioButtonMenuItem.font"), new FontUIResource("Dialog", Font.PLAIN, 15));
    harness.check(defaults.get("RadioButtonMenuItem.foreground"), new ColorUIResource(0, 0, 17));
    harness.check(defaults.get("RadioButtonMenuItem.margin"), new InsetsUIResource(2, 2, 2, 2));
    harness.check(defaults.get("RadioButtonMenuItem.selectionBackground"), new ColorUIResource(0, 0, 18));
    harness.check(defaults.get("RadioButtonMenuItem.selectionForeground"), new ColorUIResource(0, 0, 19));
    
    harness.checkPoint("RootPane");
    harness.check(defaults.get("RootPane.colorChooserDialogBorder") instanceof Border);
    harness.check(defaults.get("RootPane.defaultButtonWindowKeyBindings") instanceof Object[]);
    harness.check(defaults.get("RootPane.errorDialogBorder") instanceof Border);
    harness.check(defaults.get("RootPane.fileChooserDialogBorder") instanceof Border);
    harness.check(defaults.get("RootPane.frameBorder") instanceof Border);
    harness.check(defaults.get("RootPane.informationDialogBorder") instanceof Border);
    harness.check(defaults.get("RootPane.plainDialogBorder") instanceof Border);
    harness.check(defaults.get("RootPane.questionDialogBorder") instanceof Border);
    harness.check(defaults.get("RootPane.warningDialogBorder") instanceof Border);
    
    harness.checkPoint("ScrollBar");
    harness.check(defaults.get("ScrollBar.allowsAbsolutePositioning"), Boolean.TRUE);
    harness.check(defaults.get("ScrollBar.background"), new ColorUIResource(0, 0, 4));
    harness.check(defaults.get("ScrollBar.darkShadow"), new ColorUIResource(0, 0, 5));
    harness.check(defaults.get("ScrollBar.focusInputMap"), null);
    harness.check(defaults.get("ScrollBar.focusInputMap.RightToLeft"), null);
    harness.check(defaults.get("ScrollBar.foreground"), new ColorUIResource(0, 0, 4));
    harness.check(defaults.get("ScrollBar.highlight"), new ColorUIResource(0, 0, 7));
    harness.check(defaults.get("ScrollBar.maximumThumbSize"), new DimensionUIResource(4096, 4096));
    harness.check(defaults.get("ScrollBar.minimumThumbSize"), new DimensionUIResource(8, 8));
    harness.check(defaults.get("ScrollBar.shadow"), new ColorUIResource(0, 0, 9));
    harness.check(defaults.get("ScrollBar.thumb"), new ColorUIResource(0, 0, 24));
    harness.check(defaults.get("ScrollBar.thumbDarkShadow"), new ColorUIResource(0, 0, 5));
    harness.check(defaults.get("ScrollBar.thumbHighlight"), new ColorUIResource(0, 0, 20));
    harness.check(defaults.get("ScrollBar.thumbShadow"), new ColorUIResource(0, 0, 21));
    harness.check(defaults.get("ScrollBar.track"), new ColorUIResource(0, 0, 4));
    harness.check(defaults.get("ScrollBar.trackHighlight"), new ColorUIResource(0, 0, 5));
    harness.check(defaults.getInt("ScrollBar.width"), 17);
    
    harness.checkPoint("ScrollPane");
    harness.check(defaults.get("ScrollPane.ancestorInputMap") instanceof InputMapUIResource);
    harness.check(defaults.get("ScrollPane.ancestorInputMap.RightToLeft") instanceof InputMapUIResource);
    harness.check(defaults.get("ScrollPane.background"), new ColorUIResource(0, 0, 4));
    harness.check(defaults.get("ScrollPane.border") instanceof MetalBorders.ScrollPaneBorder);
    harness.check(defaults.get("ScrollPane.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("ScrollPane.foreground"), new ColorUIResource(0, 0, 10));
    
    harness.checkPoint("Separator");
    harness.check(defaults.get("Separator.background"), new ColorUIResource(0, 0, 25));
    harness.check(defaults.get("Separator.foreground"), new ColorUIResource(0, 0, 26));
    harness.check(defaults.get("Separator.highlight"), new ColorUIResource(0, 0, 7));
    harness.check(defaults.get("Separator.shadow"), new ColorUIResource(0, 0, 9));
    
    harness.checkPoint("Slider");
    harness.check(defaults.get("Slider.altTrackColor"), null);
    harness.check(defaults.get("Slider.background"), new ColorUIResource(0, 0, 4));
    harness.check(defaults.get("Slider.focus"), new ColorUIResource(0, 0, 12));
    InputMap focusInputMap = (InputMap) defaults.get("Slider.focusInputMap");
    KeyStroke[] keys = focusInputMap.keys();
//    for (int i = 0; i < keys.length; i++) {
//        System.out.println(keys[i] + " --> " + focusInputMap.get(keys[i]));
//    }
    List keyList = Arrays.asList(keys);
    harness.check(keyList.contains(KeyStroke.getKeyStroke("LEFT")));
    harness.check(keyList.contains(KeyStroke.getKeyStroke("RIGHT")));
    harness.check(keyList.contains(KeyStroke.getKeyStroke("UP")));
    harness.check(keyList.contains(KeyStroke.getKeyStroke("DOWN")));  
    harness.check(keyList.contains(KeyStroke.getKeyStroke("KP_LEFT")));
    harness.check(keyList.contains(KeyStroke.getKeyStroke("KP_RIGHT")));
    harness.check(keyList.contains(KeyStroke.getKeyStroke("KP_UP")));  
    harness.check(keyList.contains(KeyStroke.getKeyStroke("KP_DOWN")));  
    harness.check(keyList.contains(KeyStroke.getKeyStroke("HOME")));
    harness.check(keyList.contains(KeyStroke.getKeyStroke("END")));
    harness.check(keyList.contains(KeyStroke.getKeyStroke("PAGE_UP")));
    harness.check(keyList.contains(KeyStroke.getKeyStroke("PAGE_DOWN")));
    harness.check(keyList.contains(KeyStroke.getKeyStroke("ctrl PAGE_DOWN"))); 
    harness.check(keyList.contains(KeyStroke.getKeyStroke("ctrl PAGE_UP")));  
    harness.check(focusInputMap.get(KeyStroke.getKeyStroke("LEFT")), "negativeUnitIncrement");
    harness.check(focusInputMap.get(KeyStroke.getKeyStroke("RIGHT")), "positiveUnitIncrement");
    harness.check(focusInputMap.get(KeyStroke.getKeyStroke("UP")), "positiveUnitIncrement");
    harness.check(focusInputMap.get(KeyStroke.getKeyStroke("DOWN")), "negativeUnitIncrement");
    harness.check(focusInputMap.get(KeyStroke.getKeyStroke("KP_LEFT")), "negativeUnitIncrement");
    harness.check(focusInputMap.get(KeyStroke.getKeyStroke("KP_RIGHT")), "positiveUnitIncrement");
    harness.check(focusInputMap.get(KeyStroke.getKeyStroke("KP_UP")), "positiveUnitIncrement");
    harness.check(focusInputMap.get(KeyStroke.getKeyStroke("KP_DOWN")), "negativeUnitIncrement");
    harness.check(focusInputMap.get(KeyStroke.getKeyStroke("HOME")), "minScroll");
    harness.check(focusInputMap.get(KeyStroke.getKeyStroke("END")), "maxScroll");
    harness.check(focusInputMap.get(KeyStroke.getKeyStroke("PAGE_UP")), "positiveBlockIncrement");
    harness.check(focusInputMap.get(KeyStroke.getKeyStroke("PAGE_DOWN")), "negativeBlockIncrement");
    harness.check(focusInputMap.get(KeyStroke.getKeyStroke("ctrl PAGE_DOWN")), "negativeBlockIncrement");
    harness.check(focusInputMap.get(KeyStroke.getKeyStroke("ctrl PAGE_UP")), "positiveBlockIncrement");
    
    InputMap rightToLeftMap = (InputMap) defaults.get("Slider.focusInputMap.RightToLeft");
    keys = rightToLeftMap != null ? rightToLeftMap.keys() : new KeyStroke[] {};
    keyList = Arrays.asList(keys);
//    for (int i = 0; i < keys.length; i++) {
//        System.out.println(keys[i] + " --> " + focusInputMap.get(keys[i]));
//    }
    harness.check(keyList.contains(KeyStroke.getKeyStroke("RIGHT")));
    harness.check(keyList.contains(KeyStroke.getKeyStroke("KP_RIGHT")));
    harness.check(keyList.contains(KeyStroke.getKeyStroke("LEFT")));
    harness.check(keyList.contains(KeyStroke.getKeyStroke("KP_LEFT")));
    if (rightToLeftMap == null) 
    {
      rightToLeftMap = new InputMap();  // to prevent NullPointerException    
    }
    harness.check(rightToLeftMap.get(KeyStroke.getKeyStroke("RIGHT")), "negativeUnitIncrement");
    harness.check(rightToLeftMap.get(KeyStroke.getKeyStroke("KP_RIGHT")), "negativeUnitIncrement");
    harness.check(rightToLeftMap.get(KeyStroke.getKeyStroke("LEFT")), "positiveUnitIncrement");
    harness.check(rightToLeftMap.get(KeyStroke.getKeyStroke("KP_LEFT")), "positiveUnitIncrement");
    
    harness.check(defaults.get("Slider.focusInsets"), new InsetsUIResource(0, 0, 0, 0));
    harness.check(defaults.get("Slider.foreground"), new ColorUIResource(0, 0, 24));
    harness.check(defaults.get("Slider.highlight"), new ColorUIResource(0, 0, 7));
    harness.check(defaults.get("Slider.horizontalThumbIcon") != null); 
    harness.check(defaults.getInt("Slider.majorTickLength"), 6);
    harness.check(defaults.get("Slider.shadow"), new ColorUIResource(0, 0, 9));
    harness.check(defaults.getInt("Slider.trackWidth"), 7);
    harness.check(defaults.get("Slider.verticalThumbIcon") != null);
    
    harness.checkPoint("Spinner");
    harness.check(defaults.get("Spinner.ancestorInputMap") instanceof InputMapUIResource);
    harness.check(defaults.get("Spinner.arrowButtonBorder") instanceof Border);
    harness.check(defaults.get("Spinner.arrowButtonInsets"), new InsetsUIResource(0, 0, 0, 0));
    harness.check(defaults.get("Spinner.arrowButtonSize"), new Dimension(16, 5));
    harness.check(defaults.get("Spinner.border") instanceof Border);
    harness.check(defaults.get("Spinner.background"), new ColorUIResource(0, 0, 4));
    harness.check(defaults.get("Spinner.editorBorderPainted"), Boolean.FALSE);
    harness.check(defaults.get("Spinner.font"), new FontUIResource("Dialog", Font.PLAIN, 13));
    harness.check(defaults.get("Spinner.foreground"), new ColorUIResource(0, 0, 4));
    
    harness.checkPoint("SplitPane");
    harness.check(defaults.get("SplitPane.ancestorInputMap") instanceof InputMapUIResource);
    harness.check(defaults.get("SplitPane.background"), new ColorUIResource(0, 0, 4));
    harness.check(defaults.get("SplitPane.border") instanceof Border);
    harness.check(defaults.get("SplitPane.darkShadow"), new ColorUIResource(0, 0, 5));
    harness.check(defaults.get("SplitPane.dividerFocusColor"), new ColorUIResource(0, 0, 20));
    harness.check(defaults.getInt("SplitPane.dividerSize"), 10);
    harness.check(defaults.get("SplitPane.highlight"), new ColorUIResource(0, 0, 7));
    harness.check(defaults.get("SplitPane.shadow"), new ColorUIResource(0, 0, 9));

    harness.checkPoint("SplitPaneDivider");
    harness.check(defaults.get("SplitPaneDivider.draggingColor"), new ColorUIResource(64, 64, 64));
    harness.check(defaults.get("SplitPaneDivider.border") instanceof Border);
    
    harness.checkPoint("TabbedPane");
    harness.check(defaults.get("TabbedPane.ancestorInputMap") instanceof InputMapUIResource);
    harness.check(defaults.get("TabbedPane.background"), new ColorUIResource(0, 0, 9));
    harness.check(defaults.get("TabbedPane.borderHighlightColor"), null);
    harness.check(defaults.get("TabbedPane.contentAreaColor"), null);
    harness.check(defaults.get("TabbedPane.contentBorderInsets"), new InsetsUIResource(2, 2, 3, 3));
    harness.check(defaults.get("TabbedPane.darkShadow"), new ColorUIResource(0, 0, 5));
    harness.check(defaults.get("TabbedPane.focus"), new ColorUIResource(0, 0, 21));
    harness.check(defaults.get("TabbedPane.focusInputMap") instanceof InputMapUIResource);
    harness.check(defaults.get("TabbedPane.font"), new FontUIResource("Dialog", Font.PLAIN, 13));
    harness.check(defaults.get("TabbedPane.foreground"), new ColorUIResource(0, 0, 10));
    harness.check(defaults.get("TabbedPane.highlight"), new ColorUIResource(0, 0, 7));
    harness.check(defaults.get("TabbedPane.light"), new ColorUIResource(0, 0, 4));
    harness.check(defaults.get("TabbedPane.selected"), new ColorUIResource(0, 0, 4));
    harness.check(defaults.get("TabbedPane.selectedTabPadInsets"), new InsetsUIResource(2, 2, 2, 1));
    harness.check(defaults.get("TabbedPane.selectHighlight"), new ColorUIResource(0, 0, 7));
    harness.check(defaults.get("TabbedPane.shadow"), new ColorUIResource(0, 0, 9));
    harness.check(defaults.get("TabbedPane.tabAreaBackground"), new ColorUIResource(0, 0, 4));
    harness.check(defaults.get("TabbedPane.tabAreaInsets"), new InsetsUIResource(4, 2, 0, 6));
    harness.check(defaults.get("TabbedPane.tabInsets"), new InsetsUIResource(0, 9, 1, 9));
    harness.check(defaults.getInt("TabbedPane.tabRunOverlay"), 2);
    harness.check(defaults.getInt("TabbedPane.textIconGap"), 4);
    harness.check(defaults.get("TabbedPane.unselectedBackground"), null);
    
    harness.checkPoint("Table");
    harness.check(defaults.get("Table.ancestorInputMap") instanceof InputMapUIResource);
    harness.check(defaults.get("Table.ancestorInputMap.RightToLeft") instanceof InputMapUIResource);
    harness.check(defaults.get("Table.background"), new ColorUIResource(0, 0, 30));
    harness.check(defaults.get("Table.focusCellBackground"), new ColorUIResource(0, 0, 30));
    harness.check(defaults.get("Table.focusCellForeground"), new ColorUIResource(0, 0, 10));
    harness.check(defaults.get("Table.focusCellHighlightBorder") instanceof BorderUIResource.LineBorderUIResource);
    harness.check(defaults.get("Table.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("Table.foreground"), new ColorUIResource(0, 0, 10));
    harness.check(defaults.get("Table.gridColor"), new ColorUIResource(0, 0, 9));
    harness.check(defaults.get("Table.scrollPaneBorder") instanceof MetalBorders.ScrollPaneBorder);
    harness.check(defaults.get("Table.focusCellBackground"), new ColorUIResource(0, 0, 30));
    
    harness.checkPoint("TableHeader");
//    harness.check(defaults.get("TableHeader.cellBorder") instanceof MetalBorders.TableHeaderBorder);
    harness.check(defaults.get("TableHeader.background"), new ColorUIResource(0, 0, 4));
    harness.check(defaults.get("TableHeader.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("TableHeader.foreground"), new ColorUIResource(0, 0, 10));
    
    harness.checkPoint("TextArea");
    harness.check(defaults.get("TextArea.background"), new ColorUIResource(0, 0, 30));
    harness.check(defaults.get("TextArea.border") instanceof BasicBorders.MarginBorder);
    harness.check(defaults.getInt("TextArea.caretBlinkRate"), 500);
    harness.check(defaults.get("TextArea.caretForeground"), new ColorUIResource(0, 0, 29));
    harness.check(defaults.get("TextArea.focusInputMap") instanceof InputMapUIResource);
    harness.check(defaults.get("TextArea.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("TextArea.foreground"), new ColorUIResource(0, 0, 29));
    harness.check(defaults.get("TextArea.inactiveForeground"), new ColorUIResource(0, 0, 14));
    harness.check(defaults.get("TextArea.margin"), new InsetsUIResource(0, 0, 0, 0));
    harness.check(defaults.get("TextArea.selectionBackground"), new ColorUIResource(0, 0, 28));
    harness.check(defaults.get("TextArea.selectionForeground"), new ColorUIResource(0, 0, 13));
    
    harness.checkPoint("TextField");
    harness.check(defaults.get("TextField.background"), new ColorUIResource(0, 0, 30));
    harness.check(defaults.get("TextField.border") instanceof BorderUIResource.CompoundBorderUIResource);
    harness.check(defaults.getInt("TextField.caretBlinkRate"), 500);
    harness.check(defaults.get("TextField.caretForeground"), new ColorUIResource(0, 0, 29));
    harness.check(defaults.get("TextField.darkShadow"), new ColorUIResource(0, 0, 5));
    harness.check(defaults.get("TextField.focusInputMap") instanceof InputMapUIResource);
    harness.check(defaults.get("TextField.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("TextField.foreground"), new ColorUIResource(0, 0, 29));
    harness.check(defaults.get("TextField.highlight"), new ColorUIResource(0, 0, 7));
    harness.check(defaults.get("TextField.inactiveBackground"), new ColorUIResource(0, 0, 4));
    harness.check(defaults.get("TextField.inactiveForeground"), new ColorUIResource(0, 0, 14));
    harness.check(defaults.get("TextField.light"), new ColorUIResource(0, 0, 7));
    harness.check(defaults.get("TextField.margin"), new InsetsUIResource(0, 0, 0, 0));
    harness.check(defaults.get("TextField.selectionBackground"), new ColorUIResource(0, 0, 28));
    harness.check(defaults.get("TextField.selectionForeground"), new ColorUIResource(0, 0, 13));
    harness.check(defaults.get("TextField.shadow"), new ColorUIResource(0, 0, 9));
    
    harness.checkPoint("TextPane");
    harness.check(defaults.get("TextPane.background"), new ColorUIResource(0, 0, 30));
    harness.check(defaults.get("TextPane.border") instanceof BasicBorders.MarginBorder);
    harness.check(defaults.getInt("TextPane.caretBlinkRate"), 500);
    harness.check(defaults.get("TextPane.caretForeground"), new ColorUIResource(0, 0, 29));
    harness.check(defaults.get("TextPane.focusInputMap") instanceof InputMapUIResource);
    harness.check(defaults.get("TextPane.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("TextPane.foreground"), new ColorUIResource(0, 0, 29));
    harness.check(defaults.get("TextPane.inactiveForeground"), new ColorUIResource(0, 0, 14));
    harness.check(defaults.get("TextPane.margin"), new InsetsUIResource(3, 3, 3, 3));
    harness.check(defaults.get("TextPane.selectionBackground"), new ColorUIResource(0, 0, 28));
    harness.check(defaults.get("TextPane.selectionForeground"), new ColorUIResource(0, 0, 13));
    
    harness.checkPoint("TitledBorder");
    harness.check(defaults.get("TitledBorder.border") instanceof LineBorderUIResource);
    harness.check(defaults.get("TitledBorder.font"), new FontUIResource("Dialog", Font.PLAIN, 13));
    harness.check(defaults.get("TitledBorder.titleColor"), new ColorUIResource(0, 0, 27));
    
    harness.checkPoint("ToggleButton");
    harness.check(defaults.get("ToggleButton.background"), new ColorUIResource(0, 0, 4));
    harness.check(defaults.get("ToggleButton.border") instanceof BorderUIResource.CompoundBorderUIResource);
    harness.check(defaults.get("ToggleButton.darkShadow"), new ColorUIResource(0, 0, 5));
    harness.check(defaults.get("ToggleButton.disabledText"), new ColorUIResource(0, 0, 35));
    harness.check(defaults.get("ToggleButton.focus"), new ColorUIResource(0, 0, 12));
    harness.check(defaults.get("ToggleButton.focusInputMap") instanceof InputMapUIResource);
    harness.check(defaults.get("ToggleButton.font"), new FontUIResource("Dialog", Font.PLAIN, 13));
    harness.check(defaults.get("ToggleButton.foreground"), new ColorUIResource(0, 0, 10));
    harness.check(defaults.get("ToggleButton.highlight"), new ColorUIResource(0, 0, 7));
    harness.check(defaults.get("ToggleButton.light"), new ColorUIResource(0, 0, 7));
    harness.check(defaults.get("ToggleButton.margin"), new InsetsUIResource(2, 14, 2, 14));
    harness.check(defaults.get("ToggleButton.select"), new ColorUIResource(0, 0, 9));
    harness.check(defaults.get("ToggleButton.shadow"), new ColorUIResource(0, 0, 9));
    harness.check(defaults.getInt("ToggleButton.textIconGap"), 4);
    harness.check(defaults.getInt("ToggleButton.textShiftOffset"), 0);
    
    harness.checkPoint("ToolBar");
    harness.check(defaults.get("ToolBar.ancestorInputMap") instanceof InputMapUIResource);
    harness.check(defaults.get("ToolBar.background"), new ColorUIResource(0, 0, 15));
//    harness.check(defaults.get("ToolBar.border") instanceof MetalBorders.ToolBarBorder);
    harness.check(defaults.get("ToolBar.borderColor"), null);
    harness.check(defaults.get("ToolBar.darkShadow"), new ColorUIResource(0, 0, 5));
    harness.check(defaults.get("ToolBar.dockingBackground"), new ColorUIResource(0, 0, 15));
    harness.check(defaults.get("ToolBar.dockingForeground"), new ColorUIResource(0, 0, 21));
    harness.check(defaults.get("ToolBar.floatingBackground"), new ColorUIResource(0, 0, 15));
    harness.check(defaults.get("ToolBar.floatingForeground"), new ColorUIResource(0, 0, 20));
    harness.check(defaults.get("ToolBar.font"), new FontUIResource("Dialog", Font.PLAIN, 15));
    harness.check(defaults.get("ToolBar.foreground"), new ColorUIResource(0, 0, 17));
    harness.check(defaults.get("ToolBar.highlight"), new ColorUIResource(0, 0, 7));
    harness.check(defaults.get("ToolBar.light"), new ColorUIResource(0, 0, 7));
    harness.check(defaults.get("ToolBar.separatorSize"), new DimensionUIResource(10, 10));
    harness.check(defaults.get("ToolBar.shadow"), new ColorUIResource(0, 0, 9));
    
    harness.checkPoint("ToolTip");
    harness.check(defaults.get("ToolTip.background"), new ColorUIResource(0, 0, 20));
    harness.check(defaults.get("ToolTip.backgroundInactive"), new ColorUIResource(0, 0, 4));
    LineBorderUIResource b2 = (LineBorderUIResource) defaults.get("ToolTip.border");
    harness.check(b2.getThickness(), 1);
    harness.check(b2.getLineColor(), new Color(0, 0, 21));
    b2 = (LineBorderUIResource) defaults.get("ToolTip.borderInactive");
    harness.check(b2 != null ? b2.getThickness() : 0, 1);
    harness.check(b2 != null ? b2.getLineColor() : null, new Color(0, 0, 5));
    harness.check(defaults.get("ToolTip.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("ToolTip.foreground"), new ColorUIResource(0, 0, 23));
    harness.check(defaults.get("ToolTip.foregroundInactive"), new ColorUIResource(0, 0, 5));
    harness.check(defaults.get("ToolTip.hideAccelerator"), Boolean.FALSE);
    
    harness.checkPoint("Tree");
    harness.check(defaults.get("Tree.background"), new ColorUIResource(0, 0, 30));
    harness.check(defaults.get("Tree.ancestorInputMap") instanceof InputMapUIResource);
    harness.check(defaults.get("Tree.changeSelectionWithFocus"), Boolean.TRUE);
    harness.check(defaults.get("Tree.closedIcon") instanceof MetalIconFactory.TreeFolderIcon);
    harness.check(defaults.get("Tree.collapsedIcon") instanceof MetalIconFactory.TreeControlIcon);
    harness.check(defaults.get("Tree.drawsFocusBorderAroundIcon"), Boolean.FALSE);
    harness.check(defaults.get("Tree.editorBorder") instanceof BorderUIResource.LineBorderUIResource);
    harness.check(defaults.get("Tree.expandedIcon") instanceof MetalIconFactory.TreeControlIcon);
    harness.check(defaults.get("Tree.focusInputMap") instanceof InputMapUIResource);
    harness.check(defaults.get("Tree.focusInputMap.RightToLeft") instanceof InputMapUIResource);
    harness.check(defaults.get("Tree.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("Tree.foreground"), new ColorUIResource(0, 0, 29));
    harness.check(defaults.get("Tree.hash"), new ColorUIResource(0, 0, 20));
    harness.check(defaults.get("Tree.leafIcon") instanceof MetalIconFactory.TreeLeafIcon);
    harness.check(defaults.getInt("Tree.leftChildIndent"), 7);
    harness.check(defaults.get("Tree.line"), new ColorUIResource(0, 0, 20));
    harness.check(defaults.get("Tree.openIcon") instanceof MetalIconFactory.TreeFolderIcon);
    harness.check(defaults.getInt("Tree.rightChildIndent"), 13);
    harness.check(defaults.getInt("Tree.rowHeight"), 0);
    harness.check(defaults.get("Tree.scrollsOnExpand"), Boolean.TRUE);
    harness.check(defaults.get("Tree.selectionBackground"), new ColorUIResource(0, 0, 28));
    harness.check(defaults.get("Tree.selectionBorderColor"), new ColorUIResource(0, 0, 12));
    harness.check(defaults.get("Tree.selectionForeground"), new ColorUIResource(0, 0, 13));
    harness.check(defaults.get("Tree.textBackground"), new ColorUIResource(0, 0, 30));
    harness.check(defaults.get("Tree.textForeground"), new ColorUIResource(0, 0, 29));
    
    harness.checkPoint("Viewport");
    harness.check(defaults.get("Viewport.background"), new ColorUIResource(0, 0, 4));
    harness.check(defaults.get("Viewport.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("Viewport.foreground"), new ColorUIResource(0, 0, 29));
  }
}
