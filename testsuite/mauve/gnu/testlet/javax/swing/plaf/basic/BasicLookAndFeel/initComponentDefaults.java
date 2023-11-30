// Tags: JDK1.3
// Uses: MyBasicLookAndFeel

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
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.javax.swing.plaf.basic.BasicLookAndFeel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Dimension;
import java.awt.Font;
import java.util.Arrays;
import java.util.List;

import javax.swing.Icon;
import javax.swing.InputMap;
import javax.swing.KeyStroke;
import javax.swing.ListCellRenderer;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.swing.plaf.BorderUIResource;
import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.DimensionUIResource;
import javax.swing.plaf.FontUIResource;
import javax.swing.plaf.InputMapUIResource;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.plaf.BorderUIResource.CompoundBorderUIResource;
import javax.swing.plaf.basic.BasicBorders;
import javax.swing.plaf.basic.BasicBorders.ButtonBorder;
import javax.swing.plaf.basic.BasicBorders.MarginBorder;

/**
 * Some checks for the initComponentDefaults() method in the 
 * {@link BasicLookAndFeel} class.  
 */
public class initComponentDefaults implements Testlet
{

  /**
   * Runs the test using the specified harness.  
   * 
   * @param harness  the test harness (<code>null</code> not allowed).
   */
  public void test(TestHarness harness) 
  {
    // TODO: there are a lot of 'instanceof' checks in here.  Those are weak
    // tests, try to strengthen them.

    MyBasicLookAndFeel laf = new MyBasicLookAndFeel();
    UIDefaults defaults = new UIDefaults();
    laf.initComponentDefaults(defaults);
        
    harness.checkPoint("AuditoryCues");
    harness.check(defaults.get("AuditoryCues.allAuditoryCues") != null);
    harness.check(defaults.get("AuditoryCues.cueList") != null);
    harness.check(defaults.get("AuditoryCues.noAuditoryCues") != null);
    
    harness.checkPoint("Button");
    CompoundBorderUIResource b1 = (CompoundBorderUIResource) defaults.get("Button.border");
    harness.check(b1.getInsideBorder() instanceof MarginBorder);
    harness.check(b1.getOutsideBorder() instanceof ButtonBorder);
    harness.check(defaults.get("Button.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("Button.margin"), new InsetsUIResource(2, 14, 2, 14));
    harness.check(defaults.get("Button.textIconGap"), new Integer(4));
    harness.check(defaults.get("Button.textShiftOffset"), new Integer(0));
    harness.check(defaults.get("Button.focusInputMap") instanceof InputMapUIResource);
    Object b = UIManager.get("Button.focusInputMap");
    InputMapUIResource bim = (InputMapUIResource) b;
    KeyStroke[] kb = bim.keys();
    harness.check(kb.length == 2);
    harness.check(bim.get(KeyStroke.getKeyStroke("SPACE")), "pressed");
    harness.check(bim.get(KeyStroke.getKeyStroke("released SPACE")), "released");
    
    harness.checkPoint("CheckBox");
    CompoundBorderUIResource b2 = (CompoundBorderUIResource) defaults.get("CheckBox.border");
    harness.check(b2.getInsideBorder() instanceof MarginBorder);
    harness.check(b2.getOutsideBorder() instanceof ButtonBorder);
    harness.check(defaults.get("CheckBox.focusInputMap") instanceof InputMapUIResource);
    Object c = UIManager.get("CheckBox.focusInputMap");
    InputMapUIResource cim = (InputMapUIResource) c;
    KeyStroke[] kc = cim.keys();
    harness.check(kc.length == 2);
    harness.check(cim.get(KeyStroke.getKeyStroke("SPACE")), "pressed");
    harness.check(cim.get(KeyStroke.getKeyStroke("released SPACE")), "released");
    harness.check(defaults.get("CheckBox.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("CheckBox.icon") instanceof Icon);
    harness.check(defaults.get("CheckBox.margin"), new InsetsUIResource(2, 2, 2, 2));
    harness.check(defaults.get("CheckBox.textIconGap"), new Integer(4));
    harness.check(defaults.get("CheckBox.textShiftOffset"), new Integer(0));
    
    harness.checkPoint("CheckBoxMenuItem");
    harness.check(defaults.get("CheckBoxMenuItem.acceleratorFont"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("CheckBoxMenuItem.arrowIcon") instanceof Icon);
    harness.check(defaults.get("CheckBoxMenuItem.border") instanceof BasicBorders.MarginBorder);
    harness.check(defaults.get("CheckBoxMenuItem.borderPainted"), Boolean.FALSE);
    harness.check(defaults.get("CheckBoxMenuItem.checkIcon") instanceof Icon);
    harness.check(defaults.get("CheckBoxMenuItem.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("CheckBoxMenuItem.margin"), new InsetsUIResource(2, 2, 2, 2));
    
    harness.checkPoint("ColorChooser");
    harness.check(defaults.get("ColorChooser.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("ColorChooser.rgbBlueMnemonic"), "66");
    harness.check(defaults.get("ColorChooser.rgbGreenMnemonic"), "78");
    harness.check(defaults.get("ColorChooser.rgbRedMnemonic"), "68");
    harness.check(defaults.get("ColorChooser.swatchesRecentSwatchSize"), new Dimension(10, 10));
    harness.check(defaults.get("ColorChooser.swatchesSwatchSize"), new Dimension(10, 10));
    
    harness.checkPoint("ComboBox");
    harness.check(defaults.get("ComboBox.ancestorInputMap") instanceof InputMapUIResource);
    harness.check(defaults.get("ComboBox.font"), new FontUIResource("SansSerif", Font.PLAIN, 12));
    
    harness.checkPoint("Desktop");
    harness.check(defaults.get("Desktop.ancestorInputMap") instanceof InputMapUIResource);
    
    harness.checkPoint("DesktopIcon");
    harness.check(defaults.get("DesktopIcon.border") instanceof BorderUIResource.CompoundBorderUIResource);
        
    harness.checkPoint("EditorPane");
    harness.check(defaults.get("EditorPane.background"), new ColorUIResource(255, 255, 255));
    harness.check(defaults.get("EditorPane.border") instanceof BasicBorders.MarginBorder);
    harness.check(defaults.get("EditorPane.caretBlinkRate"), new Integer(500));
    harness.check(defaults.get("EditorPane.font"), new FontUIResource("Serif", Font.PLAIN, 12));
    harness.check(defaults.get("EditorPane.margin"), new InsetsUIResource(3, 3, 3, 3));
    Object e = UIManager.get("EditorPane.focusInputMap");
    InputMapUIResource eim = (InputMapUIResource) e;
    KeyStroke[] ke = eim.keys();
    harness.check(ke.length == 55);
    harness.check(eim.get(KeyStroke.getKeyStroke("shift UP")), "selection-up");
    harness.check(eim.get(KeyStroke.getKeyStroke("ctrl RIGHT")), "caret-next-word");
    harness.check(eim.get(KeyStroke.getKeyStroke("shift ctrl LEFT")), "selection-previous-word");
    harness.check(eim.get(KeyStroke.getKeyStroke("shift KP_UP")), "selection-up");
    harness.check(eim.get(KeyStroke.getKeyStroke("DOWN")), "caret-down");
    harness.check(eim.get(KeyStroke.getKeyStroke("shift ctrl T")), "previous-link-action");
    harness.check(eim.get(KeyStroke.getKeyStroke("ctrl LEFT")), "caret-previous-word");
    harness.check(eim.get(KeyStroke.getKeyStroke("CUT")), "cut-to-clipboard");
    harness.check(eim.get(KeyStroke.getKeyStroke("END")), "caret-end-line");
    harness.check(eim.get(KeyStroke.getKeyStroke("shift PAGE_UP")), "selection-page-up");
    harness.check(eim.get(KeyStroke.getKeyStroke("KP_UP")), "caret-up");
    harness.check(eim.get(KeyStroke.getKeyStroke("DELETE")), "delete-next");
    harness.check(eim.get(KeyStroke.getKeyStroke("ctrl HOME")), "caret-begin");
    harness.check(eim.get(KeyStroke.getKeyStroke("shift LEFT")), "selection-backward");
    harness.check(eim.get(KeyStroke.getKeyStroke("ctrl END")), "caret-end");
    harness.check(eim.get(KeyStroke.getKeyStroke("BACK_SPACE")), "delete-previous");
    harness.check(eim.get(KeyStroke.getKeyStroke("shift ctrl RIGHT")), "selection-next-word");
    harness.check(eim.get(KeyStroke.getKeyStroke("LEFT")), "caret-backward");
    harness.check(eim.get(KeyStroke.getKeyStroke("KP_LEFT")), "caret-backward");
    harness.check(eim.get(KeyStroke.getKeyStroke("shift KP_RIGHT")), "selection-forward");
    harness.check(eim.get(KeyStroke.getKeyStroke("ctrl SPACE")), "activate-link-action");
    harness.check(eim.get(KeyStroke.getKeyStroke("ctrl H")), "delete-previous");
    harness.check(eim.get(KeyStroke.getKeyStroke("ctrl BACK_SLASH")), "unselect");
    harness.check(eim.get(KeyStroke.getKeyStroke("ENTER")), "insert-break");
    harness.check(eim.get(KeyStroke.getKeyStroke("shift HOME")), "selection-begin-line");
    harness.check(eim.get(KeyStroke.getKeyStroke("RIGHT")), "caret-forward");
    harness.check(eim.get(KeyStroke.getKeyStroke("shift ctrl PAGE_UP")), "selection-page-left");
    harness.check(eim.get(KeyStroke.getKeyStroke("shift DOWN")), "selection-down");
    harness.check(eim.get(KeyStroke.getKeyStroke("PAGE_DOWN")), "page-down");
    harness.check(eim.get(KeyStroke.getKeyStroke("shift KP_LEFT")), "selection-backward");
    harness.check(eim.get(KeyStroke.getKeyStroke("shift ctrl O")), "toggle-componentOrientation");
    harness.check(eim.get(KeyStroke.getKeyStroke("ctrl X")), "cut-to-clipboard");
    harness.check(eim.get(KeyStroke.getKeyStroke("shift ctrl PAGE_DOWN")), "selection-page-right");
    harness.check(eim.get(KeyStroke.getKeyStroke("ctrl C")), "copy-to-clipboard");
    harness.check(eim.get(KeyStroke.getKeyStroke("ctrl KP_RIGHT")), "caret-next-word");
    harness.check(eim.get(KeyStroke.getKeyStroke("shift END")), "selection-end-line");
    harness.check(eim.get(KeyStroke.getKeyStroke("ctrl KP_LEFT")), "caret-previous-word");
    harness.check(eim.get(KeyStroke.getKeyStroke("HOME")), "caret-begin-line");
    harness.check(eim.get(KeyStroke.getKeyStroke("ctrl V")), "paste-from-clipboard");
    harness.check(eim.get(KeyStroke.getKeyStroke("KP_DOWN")), "caret-down");
    harness.check(eim.get(KeyStroke.getKeyStroke("ctrl A")), "select-all");
    harness.check(eim.get(KeyStroke.getKeyStroke("shift RIGHT")), "selection-forward");
    harness.check(eim.get(KeyStroke.getKeyStroke("shift ctrl END")), "selection-end");
    harness.check(eim.get(KeyStroke.getKeyStroke("COPY")), "copy-to-clipboard");
    harness.check(eim.get(KeyStroke.getKeyStroke("shift ctrl KP_LEFT")), "selection-previous-word");
    harness.check(eim.get(KeyStroke.getKeyStroke("ctrl T")), "next-link-action");
    harness.check(eim.get(KeyStroke.getKeyStroke("shift KP_DOWN")), "selection-down");
    harness.check(eim.get(KeyStroke.getKeyStroke("TAB")), "insert-tab");
    harness.check(eim.get(KeyStroke.getKeyStroke("UP")), "caret-up");
    harness.check(eim.get(KeyStroke.getKeyStroke("shift ctrl HOME")), "selection-begin");
    harness.check(eim.get(KeyStroke.getKeyStroke("shift PAGE_DOWN")), "selection-page-down");
    harness.check(eim.get(KeyStroke.getKeyStroke("KP_RIGHT")), "caret-forward");
    harness.check(eim.get(KeyStroke.getKeyStroke("shift ctrl KP_RIGHT")), "selection-next-word");
    harness.check(eim.get(KeyStroke.getKeyStroke("PAGE_UP")), "page-up");
    harness.check(eim.get(KeyStroke.getKeyStroke("PASTE")), "paste-from-clipboard");
    
    harness.checkPoint("FileChooser");
    harness.check(defaults.get("FileChooser.ancestorInputMap") instanceof InputMapUIResource);
    harness.check(defaults.get("FileChooser.cancelButtonMnemonic"), "67");
    harness.check(defaults.get("FileChooser.directoryOpenButtonMnemonic"), "79");
    harness.check(defaults.get("FileChooser.helpButtonMnemonic"), "72");
    harness.check(defaults.get("FileChooser.openButtonMnemonic"), "79");
    harness.check(defaults.get("FileChooser.saveButtonMnemonic"), "83");
    harness.check(defaults.get("FileChooser.updateButtonMnemonic"), "85");

    harness.checkPoint("FileView");

    harness.checkPoint("FormattedTextField");
    harness.check(defaults.get("FormattedTextField.border") instanceof BasicBorders.FieldBorder);
    harness.check(defaults.get("FormattedTextField.caretBlinkRate"), new Integer(500));
    harness.check(defaults.get("FormattedTextField.focusInputMap") instanceof InputMapUIResource);
    harness.check(defaults.get("FormattedTextField.font"), new FontUIResource("SansSerif", Font.PLAIN, 12));
    harness.check(defaults.get("FormattedTextField.margin"), new InsetsUIResource(0, 0, 0, 0));
    Object f = UIManager.get("FormattedTextField.focusInputMap");
    InputMapUIResource fim = (InputMapUIResource) f;
    KeyStroke[] kf = fim.keys();
    harness.check(kf.length == 38);
    harness.check(fim.get(KeyStroke.getKeyStroke("KP_UP")), "increment");
    harness.check(fim.get(KeyStroke.getKeyStroke("END")), "caret-end-line");
    harness.check(fim.get(KeyStroke.getKeyStroke("shift ctrl  O")), "toggle-componentOrientation");
    harness.check(fim.get(KeyStroke.getKeyStroke("shift KP_LEFT")), "selection-backward");
    harness.check(fim.get(KeyStroke.getKeyStroke("shift RIGHT")), "selection-forward");
    harness.check(fim.get(KeyStroke.getKeyStroke("KP_DOWN")), "decrement");
    harness.check(fim.get(KeyStroke.getKeyStroke("HOME")), "caret-begin-line");
    harness.check(fim.get(KeyStroke.getKeyStroke("ctrl V")), "paste-from-clipboard");
    harness.check(fim.get(KeyStroke.getKeyStroke("ctrl H")), "delete-previous");
    harness.check(fim.get(KeyStroke.getKeyStroke("KP_LEFT")), "caret-backward");
    harness.check(fim.get(KeyStroke.getKeyStroke("LEFT")), "caret-backward");
    harness.check(fim.get(KeyStroke.getKeyStroke("ctrl X")), "cut-to-clipboard");
    harness.check(fim.get(KeyStroke.getKeyStroke("KP_RIGHT")), "caret-forward");
    harness.check(fim.get(KeyStroke.getKeyStroke("UP")), "increment");
    harness.check(fim.get(KeyStroke.getKeyStroke("shift ctrl KP_RIGHT")), "selection-next-word");
    harness.check(fim.get(KeyStroke.getKeyStroke("COPY")), "copy-to-clipboard");
    harness.check(fim.get(KeyStroke.getKeyStroke("shift HOME")), "selection-begin-line");
    harness.check(fim.get(KeyStroke.getKeyStroke("ESCAPE")), "reset-field-edit");
    harness.check(fim.get(KeyStroke.getKeyStroke("RIGHT")), "caret-forward");
    harness.check(fim.get(KeyStroke.getKeyStroke("shift ctrl LEFT")), "selection-previous-word");
    harness.check(fim.get(KeyStroke.getKeyStroke("ctrl KP_LEFT")), "caret-previous-word");
    harness.check(fim.get(KeyStroke.getKeyStroke("DOWN")), "decrement");
    harness.check(fim.get(KeyStroke.getKeyStroke("ctrl KP_RIGHT")), "caret-next-word");
    harness.check(fim.get(KeyStroke.getKeyStroke("PASTE")), "paste-from-clipboard");
    harness.check(fim.get(KeyStroke.getKeyStroke("shift ctrl RIGHT")), "selection-next-word");
    harness.check(fim.get(KeyStroke.getKeyStroke("ctrl BACK_SLASH")), "unselect");
    harness.check(fim.get(KeyStroke.getKeyStroke("ctrl A")), "select-all");
    harness.check(fim.get(KeyStroke.getKeyStroke("shift KP_RIGHT")), "selection-forward");
    harness.check(fim.get(KeyStroke.getKeyStroke("CUT")), "cut-to-clipboard");
    harness.check(fim.get(KeyStroke.getKeyStroke("ctrl LEFT")), "caret-previous-word");
    harness.check(fim.get(KeyStroke.getKeyStroke("BACK_SPACE")), "delete-previous");
    harness.check(fim.get(KeyStroke.getKeyStroke("shift ctrl KP_LEFT")), "selection-previous-word");
    harness.check(fim.get(KeyStroke.getKeyStroke("ctrl C")), "copy-to-clipboard");
    harness.check(fim.get(KeyStroke.getKeyStroke("shift END")), "selection-end-line");
    harness.check(fim.get(KeyStroke.getKeyStroke("ctrl RIGHT")), "caret-next-word");
    harness.check(fim.get(KeyStroke.getKeyStroke("DELETE")), "delete-next");
    harness.check(fim.get(KeyStroke.getKeyStroke("ENTER")), "notify-field-accept");
    harness.check(fim.get(KeyStroke.getKeyStroke("shift LEFT")), "selection-backward");
    
    harness.checkPoint("InternalFrame");
    harness.check(defaults.get("InternalFrame.border") instanceof BorderUIResource.CompoundBorderUIResource);
    harness.check(defaults.get("InternalFrame.closeIcon") instanceof Icon);
    harness.check(defaults.get("InternalFrame.iconifyIcon") instanceof Icon);
    harness.check(defaults.get("InternalFrame.maximizeIcon") instanceof Icon);
    harness.check(defaults.get("InternalFrame.minimizeIcon") instanceof Icon);
    harness.check(defaults.get("InternalFrame.titleFont"), new FontUIResource("Dialog", Font.BOLD, 12));
    harness.check(defaults.get("InternalFrame.windowBindings") instanceof Object[]);

    harness.checkPoint("Label");
    harness.check(defaults.get("Label.disabledForeground"), new ColorUIResource(255, 255, 255));
    harness.check(defaults.get("Label.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
 
    harness.checkPoint("List");
    harness.check(defaults.get("List.cellRenderer") instanceof ListCellRenderer);
    harness.check(defaults.get("List.focusCellHighlightBorder") instanceof BorderUIResource.LineBorderUIResource);
    harness.check(defaults.get("List.focusInputMap") instanceof InputMapUIResource);
    Object l = UIManager.get("List.focusInputMap");
    InputMapUIResource lim = (InputMapUIResource) l;
    KeyStroke[] kl = lim.keys();
    harness.check(kl.length == 61);
    harness.check(lim.get(KeyStroke.getKeyStroke("ctrl DOWN")), "selectNextRowChangeLead");
    harness.check(lim.get(KeyStroke.getKeyStroke("shift UP")), "selectPreviousRowExtendSelection");
    harness.check(lim.get(KeyStroke.getKeyStroke("ctrl RIGHT")), "selectNextColumnChangeLead");
    harness.check(lim.get(KeyStroke.getKeyStroke("shift ctrl LEFT")), "selectPreviousColumnExtendSelection");
    harness.check(lim.get(KeyStroke.getKeyStroke("shift KP_UP")), "selectPreviousRowExtendSelection");
    harness.check(lim.get(KeyStroke.getKeyStroke("DOWN")), "selectNextRow");
    harness.check(lim.get(KeyStroke.getKeyStroke("ctrl UP")), "selectPreviousRowChangeLead");
    harness.check(lim.get(KeyStroke.getKeyStroke("ctrl LEFT")), "selectPreviousColumnChangeLead");
    harness.check(lim.get(KeyStroke.getKeyStroke("CUT")), "cut");
    harness.check(lim.get(KeyStroke.getKeyStroke("END")), "selectLastRow");
    harness.check(lim.get(KeyStroke.getKeyStroke("shift PAGE_UP")), "scrollUpExtendSelection");
    harness.check(lim.get(KeyStroke.getKeyStroke("KP_UP")), "selectPreviousRow");
    harness.check(lim.get(KeyStroke.getKeyStroke("shift ctrl UP")), "selectPreviousRowExtendSelection");
    harness.check(lim.get(KeyStroke.getKeyStroke("ctrl HOME")), "selectFirstRowChangeLead");
    harness.check(lim.get(KeyStroke.getKeyStroke("shift LEFT")), "selectPreviousColumnExtendSelection");
    harness.check(lim.get(KeyStroke.getKeyStroke("ctrl END")), "selectLastRowChangeLead");
    harness.check(lim.get(KeyStroke.getKeyStroke("ctrl PAGE_DOWN")), "scrollDownChangeLead");
    harness.check(lim.get(KeyStroke.getKeyStroke("shift ctrl RIGHT")), "selectNextColumnExtendSelection");
    harness.check(lim.get(KeyStroke.getKeyStroke("LEFT")), "selectPreviousColumn");
    harness.check(lim.get(KeyStroke.getKeyStroke("ctrl PAGE_UP")), "scrollUpChangeLead");
    harness.check(lim.get(KeyStroke.getKeyStroke("KP_LEFT")), "selectPreviousColumn");
    harness.check(lim.get(KeyStroke.getKeyStroke("shift KP_RIGHT")), "selectNextColumnExtendSelection");
    harness.check(lim.get(KeyStroke.getKeyStroke("SPACE")), "addToSelection");
    harness.check(lim.get(KeyStroke.getKeyStroke("ctrl SPACE")), "toggleAndAnchor");
    harness.check(lim.get(KeyStroke.getKeyStroke("shift SPACE")), "extendTo");
    harness.check(lim.get(KeyStroke.getKeyStroke("shift ctrl SPACE")), "moveSelectionTo");
    harness.check(lim.get(KeyStroke.getKeyStroke("shift ctrl DOWN")), "selectNextRowExtendSelection");
    harness.check(lim.get(KeyStroke.getKeyStroke("ctrl BACK_SLASH")), "clearSelection");
    harness.check(lim.get(KeyStroke.getKeyStroke("shift HOME")), "selectFirstRowExtendSelection");
    harness.check(lim.get(KeyStroke.getKeyStroke("RIGHT")), "selectNextColumn");
    harness.check(lim.get(KeyStroke.getKeyStroke("shift ctrl PAGE_UP")), "scrollUpExtendSelection");
    harness.check(lim.get(KeyStroke.getKeyStroke("shift DOWN")), "selectNextRowExtendSelection");
    harness.check(lim.get(KeyStroke.getKeyStroke("PAGE_DOWN")), "scrollDown");
    harness.check(lim.get(KeyStroke.getKeyStroke("shift ctrl KP_UP")), "selectPreviousRowExtendSelection");
    harness.check(lim.get(KeyStroke.getKeyStroke("shift KP_LEFT")), "selectPreviousColumnExtendSelection");
    harness.check(lim.get(KeyStroke.getKeyStroke("ctrl X")), "cut");
    harness.check(lim.get(KeyStroke.getKeyStroke("shift ctrl PAGE_DOWN")), "scrollDownExtendSelection");
    harness.check(lim.get(KeyStroke.getKeyStroke("ctrl SLASH")), "selectAll");
    harness.check(lim.get(KeyStroke.getKeyStroke("ctrl C")), "copy");
    harness.check(lim.get(KeyStroke.getKeyStroke("ctrl KP_RIGHT")), "selectNextColumnChangeLead");
    harness.check(lim.get(KeyStroke.getKeyStroke("shift END")), "selectLastRowExtendSelection");
    harness.check(lim.get(KeyStroke.getKeyStroke("shift ctrl KP_DOWN")), "selectNextRowExtendSelection");
    harness.check(lim.get(KeyStroke.getKeyStroke("ctrl KP_LEFT")), "selectPreviousColumnChangeLead");
    harness.check(lim.get(KeyStroke.getKeyStroke("HOME")), "selectFirstRow");
    harness.check(lim.get(KeyStroke.getKeyStroke("ctrl V")), "paste");
    harness.check(lim.get(KeyStroke.getKeyStroke("KP_DOWN")), "selectNextRow");
    harness.check(lim.get(KeyStroke.getKeyStroke("ctrl KP_DOWN")), "selectNextRowChangeLead");
    harness.check(lim.get(KeyStroke.getKeyStroke("shift RIGHT")), "selectNextColumnExtendSelection");
    harness.check(lim.get(KeyStroke.getKeyStroke("ctrl A")), "selectAll");
    harness.check(lim.get(KeyStroke.getKeyStroke("shift ctrl END")), "selectLastRowExtendSelection");
    harness.check(lim.get(KeyStroke.getKeyStroke("COPY")), "copy");
    harness.check(lim.get(KeyStroke.getKeyStroke("ctrl KP_UP")), "selectPreviousRowChangeLead");
    harness.check(lim.get(KeyStroke.getKeyStroke("shift ctrl KP_LEFT")), "selectPreviousColumnExtendSelection");
    harness.check(lim.get(KeyStroke.getKeyStroke("shift KP_DOWN")), "selectNextRowExtendSelection");
    harness.check(lim.get(KeyStroke.getKeyStroke("UP")), "selectPreviousRow");
    harness.check(lim.get(KeyStroke.getKeyStroke("shift ctrl HOME")), "selectFirstRowExtendSelection");
    harness.check(lim.get(KeyStroke.getKeyStroke("shift PAGE_DOWN")), "scrollDownExtendSelection");
    harness.check(lim.get(KeyStroke.getKeyStroke("KP_RIGHT")), "selectNextColumn");
    harness.check(lim.get(KeyStroke.getKeyStroke("shift ctrl KP_RIGHT")), "selectNextColumnExtendSelection");
    harness.check(lim.get(KeyStroke.getKeyStroke("PAGE_UP")), "scrollUp");
    harness.check(lim.get(KeyStroke.getKeyStroke("PASTE")), "paste");
    
    harness.check(defaults.get("List.focusInputMap.RightToLeft") instanceof InputMapUIResource);
    harness.check(defaults.get("List.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    
    harness.checkPoint("Menu");
    harness.check(defaults.get("Menu.acceleratorFont"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("Menu.arrowIcon") instanceof Icon);
    harness.check(defaults.get("Menu.border") instanceof BasicBorders.MarginBorder);
    harness.check(defaults.get("Menu.borderPainted"), Boolean.FALSE);
    harness.check(defaults.get("Menu.checkIcon") instanceof Icon);
    harness.check(defaults.get("Menu.crossMenuMnemonic"), Boolean.TRUE);
    harness.check(defaults.get("Menu.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("Menu.margin"), new InsetsUIResource(2, 2, 2, 2));
    harness.check(defaults.get("Menu.menuPopupOffsetX"), new Integer(0));
    harness.check(defaults.get("Menu.menuPopupOffsetY"), new Integer(0));
    int[] shortcuts = (int[]) defaults.get("Menu.shortcutKeys");
    if (shortcuts == null)
      shortcuts = new int[] { 999 };  // to prevent NullPointerException
    harness.check(shortcuts.length, 1);
    harness.check(shortcuts[0], 8);
    harness.check(defaults.get("Menu.submenuPopupOffsetX"), new Integer(0));
    harness.check(defaults.get("Menu.submenuPopupOffsetY"), new Integer(0));
    
    harness.checkPoint("MenuBar");
    harness.check(defaults.get("MenuBar.border") instanceof BasicBorders.MenuBarBorder);
    harness.check(defaults.get("MenuBar.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("MenuBar.windowBindings") instanceof Object[]);
    
    harness.checkPoint("MenuItem");
    harness.check(defaults.get("MenuItem.acceleratorDelimiter"), "+");
    harness.check(defaults.get("MenuItem.acceleratorFont"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("MenuItem.arrowIcon") instanceof Icon);
    harness.check(defaults.get("MenuItem.border") instanceof BasicBorders.MarginBorder);
    harness.check(defaults.get("MenuItem.borderPainted"), Boolean.FALSE);
    harness.check(defaults.get("MenuItem.checkIcon") instanceof Icon);
    harness.check(defaults.get("MenuItem.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("MenuItem.margin"), new InsetsUIResource(2, 2, 2, 2));
    
    harness.checkPoint("OptionPane");
    harness.check(defaults.get("OptionPane.border") instanceof BorderUIResource.EmptyBorderUIResource);
    harness.check(defaults.get("OptionPane.buttonAreaBorder") instanceof BorderUIResource.EmptyBorderUIResource);
    harness.check(defaults.get("OptionPane.buttonClickThreshhold"), new Integer(500));
    harness.check(defaults.get("OptionPane.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("OptionPane.messageAreaBorder") instanceof BorderUIResource.EmptyBorderUIResource);
    harness.check(defaults.get("OptionPane.minimumSize"), new DimensionUIResource(262, 90));
    harness.check(defaults.get("OptionPane.windowBindings") instanceof Object[]);

    harness.checkPoint("Panel");
    harness.check(defaults.get("Panel.font"), new FontUIResource("Dialog", Font.PLAIN, 12));

    harness.checkPoint("PasswordField");
    harness.check(defaults.get("PasswordField.border") instanceof BasicBorders.FieldBorder);
    harness.check(defaults.get("PasswordField.caretBlinkRate"), new Integer(500));
    harness.check(defaults.get("PasswordField.font"), new FontUIResource("MonoSpaced", Font.PLAIN, 12));
    harness.check(defaults.get("PasswordField.margin"), new InsetsUIResource(0, 0, 0, 0));
    harness.check(UIManager.get("PasswordField.focusInputMap") instanceof InputMapUIResource);
    Object o = UIManager.get("PasswordField.focusInputMap");
    InputMapUIResource im = (InputMapUIResource) o;
    KeyStroke[] k = im.keys();
    harness.check(k.length == 33);
    harness.check(im.get(KeyStroke.getKeyStroke("END")), "caret-end-line");
	harness.check(im.get(KeyStroke.getKeyStroke("shift ctrl O")), "toggle-componentOrientation");
	harness.check(im.get(KeyStroke.getKeyStroke("shift KP_LEFT")), "selection-backward");
	harness.check(im.get(KeyStroke.getKeyStroke("shift RIGHT")), "selection-forward");
	harness.check(im.get(KeyStroke.getKeyStroke("HOME")), "caret-begin-line");
	harness.check(im.get(KeyStroke.getKeyStroke("ctrl V")), "paste-from-clipboard");
	harness.check(im.get(KeyStroke.getKeyStroke("ctrl H")), "delete-previous");
	harness.check(im.get(KeyStroke.getKeyStroke("KP_LEFT")), "caret-backward");
	harness.check(im.get(KeyStroke.getKeyStroke("LEFT")), "caret-backward");
	harness.check(im.get(KeyStroke.getKeyStroke("ctrl X")), "cut-to-clipboard");
	harness.check(im.get(KeyStroke.getKeyStroke("KP_RIGHT")), "caret-forward");
	harness.check(im.get(KeyStroke.getKeyStroke("shift ctrl KP_RIGHT")), "selection-end-line");
	harness.check(im.get(KeyStroke.getKeyStroke("COPY")), "copy-to-clipboard");
	harness.check(im.get(KeyStroke.getKeyStroke("shift HOME")), "selection-begin-line");
	harness.check(im.get(KeyStroke.getKeyStroke("RIGHT")), "caret-forward");
	harness.check(im.get(KeyStroke.getKeyStroke("shift ctrl LEFT")), "selection-begin-line");
	harness.check(im.get(KeyStroke.getKeyStroke("ctrl KP_LEFT")), "caret-begin-line");
	harness.check(im.get(KeyStroke.getKeyStroke("ctrl KP_RIGHT")), "caret-end-line");
	harness.check(im.get(KeyStroke.getKeyStroke("PASTE")), "paste-from-clipboard");
	harness.check(im.get(KeyStroke.getKeyStroke("shift ctrl RIGHT")), "selection-end-line");
	harness.check(im.get(KeyStroke.getKeyStroke("ctrl BACK_SLASH")), "unselect");
	harness.check(im.get(KeyStroke.getKeyStroke("ctrl A")), "select-all");
	harness.check(im.get(KeyStroke.getKeyStroke("shift KP_RIGHT")), "selection-forward");
	harness.check(im.get(KeyStroke.getKeyStroke("CUT")), "cut-to-clipboard");
	harness.check(im.get(KeyStroke.getKeyStroke("ctrl LEFT")), "caret-begin-line");
	harness.check(im.get(KeyStroke.getKeyStroke("BACK_SPACE")), "delete-previous");
	harness.check(im.get(KeyStroke.getKeyStroke("shift ctrl KP_LEFT")), "selection-begin-line");
	harness.check(im.get(KeyStroke.getKeyStroke("ctrl C")), "copy-to-clipboard");
	harness.check(im.get(KeyStroke.getKeyStroke("shift END")), "selection-end-line");
	harness.check(im.get(KeyStroke.getKeyStroke("ctrl RIGHT")), "caret-end-line");
	harness.check(im.get(KeyStroke.getKeyStroke("DELETE")), "delete-next");
	harness.check(im.get(KeyStroke.getKeyStroke("ENTER")), "notify-field-accept");
	harness.check(im.get(KeyStroke.getKeyStroke("shift LEFT")), "selection-backward");
    
    harness.checkPoint("PopupMenu");
    harness.check(defaults.get("PopupMenu.border") instanceof BorderUIResource.CompoundBorderUIResource);
    harness.check(defaults.get("PopupMenu.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("PopupMenu.selectedWindowInputMapBindings") instanceof Object[]);
    harness.check(defaults.get("PopupMenu.selectedWindowInputMapBindings.RightToLeft") instanceof Object[]);
    
    harness.checkPoint("ProgressBar");
    harness.check(defaults.get("ProgressBar.border") instanceof BorderUIResource.LineBorderUIResource);
    harness.check(defaults.get("ProgressBar.cellLength"), new Integer(1));
    harness.check(defaults.get("ProgressBar.cellSpacing"), new Integer(0));
    harness.check(defaults.get("ProgressBar.cycleTime"), new Integer(3000));
    harness.check(defaults.get("ProgressBar.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("ProgressBar.repaintInterval"), new Integer(50));
    
    harness.checkPoint("RadioButton");
    harness.check(defaults.get("RadioButton.border") instanceof BorderUIResource.CompoundBorderUIResource);
    harness.check(defaults.get("RadioButton.focusInputMap") instanceof InputMapUIResource);
    harness.check(defaults.get("RadioButton.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("RadioButton.icon") instanceof Icon);
    harness.check(defaults.get("RadioButton.margin"), new InsetsUIResource(2, 2, 2, 2));
    harness.check(defaults.get("RadioButton.textIconGap"), new Integer(4));
    harness.check(defaults.get("RadioButton.textShiftOffset"), new Integer(0));
    
    harness.checkPoint("RadioButtonMenuItem");
    harness.check(defaults.get("RadioButtonMenuItem.acceleratorFont"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("RadioButtonMenuItem.arrowIcon") instanceof Icon);
    harness.check(defaults.get("RadioButtonMenuItem.border") instanceof BasicBorders.MarginBorder);
    harness.check(defaults.get("RadioButtonMenuItem.borderPainted"), Boolean.FALSE);
    harness.check(defaults.get("RadioButtonMenuItem.checkIcon") instanceof Icon);
    harness.check(defaults.get("RadioButtonMenuItem.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("RadioButtonMenuItem.margin"), new InsetsUIResource(2, 2, 2, 2));
    harness.check(defaults.get("RootPane.defaultButtonWindowKeyBindings") instanceof Object[]);
    
    harness.checkPoint("ScrollBar");
    harness.check(defaults.get("ScrollBar.background"), new ColorUIResource(224, 224, 224));
    harness.check(defaults.get("ScrollBar.maximumThumbSize"), new DimensionUIResource(4096, 4096));
    harness.check(defaults.get("ScrollBar.minimumThumbSize"), new DimensionUIResource(8, 8));
    harness.check(defaults.get("ScrollBar.width"), new Integer(16));
    
    harness.check(defaults.get("ScrollPane.ancestorInputMap") instanceof InputMapUIResource);
    harness.check(defaults.get("ScrollPane.ancestorInputMap.RightToLeft") instanceof InputMapUIResource);
    harness.check(defaults.get("ScrollPane.border") instanceof BasicBorders.FieldBorder);
    harness.check(defaults.get("ScrollPane.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    
    harness.checkPoint("Slider");
    InputMap map = (InputMap) defaults.get("Slider.focusInputMap");
    KeyStroke[] keys = map.keys();
    InputMap focusInputMap = (InputMap) defaults.get("Slider.focusInputMap");
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
    
    InputMap rightToLeftMap = (InputMap) defaults.get("Slider.focusInputMap.RightToLeft");
    keys = rightToLeftMap != null ? rightToLeftMap.keys() : new KeyStroke[] {};
    keyList = Arrays.asList(keys);
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
    
    harness.check(defaults.get("Slider.focusInsets"), new InsetsUIResource(2, 2, 2, 2));
    
    harness.checkPoint("Spinner");
    harness.check(defaults.get("Spinner.ancestorInputMap") instanceof InputMapUIResource);
    harness.check(defaults.get("Spinner.arrowButtonSize"), new DimensionUIResource(16, 5));
    harness.check(defaults.get("Spinner.border") instanceof BasicBorders.FieldBorder);
    harness.check(defaults.get("Spinner.editorBorderPainted"), Boolean.FALSE);
    harness.check(defaults.get("Spinner.font"), new FontUIResource("MonoSpaced", Font.PLAIN, 12));
    
    harness.checkPoint("SplitPane");
    harness.check(defaults.get("SplitPane.ancestorInputMap") instanceof InputMapUIResource);
    harness.check(defaults.get("SplitPane.border") instanceof BasicBorders.SplitPaneBorder);
    harness.check(defaults.get("SplitPane.dividerSize"), new Integer(7));
    
    harness.checkPoint("SplitPaneDivider");
    harness.check(defaults.get("SplitPaneDivider.border") instanceof Border);
    
    harness.checkPoint("TabbedPane");
    harness.check(defaults.get("TabbedPane.ancestorInputMap") instanceof InputMapUIResource);
    harness.check(defaults.get("TabbedPane.contentBorderInsets"), new InsetsUIResource(2, 2, 3, 3));
    harness.check(defaults.get("TabbedPane.focusInputMap") instanceof InputMapUIResource);
    Object tab = UIManager.get("TabbedPane.focusInputMap");
    InputMapUIResource tabim = (InputMapUIResource) tab;
    harness.check(tabim.keys().length == 10);
    harness.check(tabim.get(KeyStroke.getKeyStroke("ctrl DOWN")),"requestFocusForVisibleComponent");
    harness.check(tabim.get(KeyStroke.getKeyStroke("KP_UP")),"navigateUp");
    harness.check(tabim.get(KeyStroke.getKeyStroke("LEFT")),"navigateLeft");
    harness.check(tabim.get(KeyStroke.getKeyStroke("ctrl KP_DOWN")),"requestFocusForVisibleComponent");
    harness.check(tabim.get(KeyStroke.getKeyStroke("UP")),"navigateUp");
    harness.check(tabim.get(KeyStroke.getKeyStroke("KP_DOWN")),"navigateDown");
    harness.check(tabim.get(KeyStroke.getKeyStroke("KP_LEFT")),"navigateLeft");
    harness.check(tabim.get(KeyStroke.getKeyStroke("RIGHT")),"navigateRight");
    harness.check(tabim.get(KeyStroke.getKeyStroke("KP_RIGHT")),"navigateRight");
    harness.check(tabim.get(KeyStroke.getKeyStroke("DOWN")),"navigateDown");
    harness.check(defaults.get("TabbedPane.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("TabbedPane.selectedTabPadInsets"), new InsetsUIResource(2, 2, 2, 1));
    harness.check(defaults.get("TabbedPane.tabAreaInsets"), new InsetsUIResource(3, 2, 0, 2));
    harness.check(defaults.get("TabbedPane.tabInsets"), new InsetsUIResource(0, 4, 1, 4));
    harness.check(defaults.get("TabbedPane.tabRunOverlay"), new Integer(2));
    harness.check(defaults.get("TabbedPane.textIconGap"), new Integer(4));
    
    harness.checkPoint("Table");
    harness.check(defaults.get("Table.ancestorInputMap") instanceof InputMapUIResource);
    harness.check(defaults.get("Table.ancestorInputMap.RightToLeft") instanceof InputMapUIResource);
    harness.check(defaults.get("Table.focusCellHighlightBorder") instanceof BorderUIResource.LineBorderUIResource);
    harness.check(defaults.get("Table.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("Table.gridColor"), new ColorUIResource(128, 128, 128));
    harness.check(defaults.get("Table.scrollPaneBorder") instanceof BorderUIResource.BevelBorderUIResource);
    
    harness.checkPoint("TableHeader");
    harness.check(defaults.get("TableHeader.cellBorder"), null);
    harness.check(defaults.get("TableHeader.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    
    harness.checkPoint("TextArea");
    harness.check(defaults.get("TextArea.border") instanceof BasicBorders.MarginBorder);
    harness.check(defaults.get("TextArea.caretBlinkRate"), new Integer(500));
    harness.check(defaults.get("TextArea.font"), new FontUIResource("MonoSpaced", Font.PLAIN, 12));
    harness.check(defaults.get("TextArea.margin"), new InsetsUIResource(0, 0, 0, 0));
    harness.check(UIManager.get("TextArea.focusInputMap") instanceof InputMapUIResource);
    Object ta = UIManager.get("TextArea.focusInputMap");
    InputMapUIResource taim = (InputMapUIResource) ta;
    harness.check(taim.keys().length == 55);
    harness.check(taim.get(KeyStroke.getKeyStroke("shift UP")), "selection-up");
    harness.check(taim.get(KeyStroke.getKeyStroke("ctrl RIGHT")), "caret-next-word");
    harness.check(taim.get(KeyStroke.getKeyStroke("shift ctrl LEFT")), "selection-previous-word");
    harness.check(taim.get(KeyStroke.getKeyStroke("shift KP_UP")), "selection-up");
    harness.check(taim.get(KeyStroke.getKeyStroke("DOWN")), "caret-down");
    harness.check(taim.get(KeyStroke.getKeyStroke("shift ctrl T")), "previous-link-action");
    harness.check(taim.get(KeyStroke.getKeyStroke("ctrl LEFT")), "caret-previous-word");
    harness.check(taim.get(KeyStroke.getKeyStroke("CUT")), "cut-to-clipboard");
    harness.check(taim.get(KeyStroke.getKeyStroke("END")), "caret-end-line");
    harness.check(taim.get(KeyStroke.getKeyStroke("shift PAGE_UP")), "selection-page-up");
    harness.check(taim.get(KeyStroke.getKeyStroke("KP_UP")), "caret-up");
    harness.check(taim.get(KeyStroke.getKeyStroke("DELETE")), "delete-next");
    harness.check(taim.get(KeyStroke.getKeyStroke("ctrl HOME")), "caret-begin");
    harness.check(taim.get(KeyStroke.getKeyStroke("shift LEFT")), "selection-backward");
    harness.check(taim.get(KeyStroke.getKeyStroke("ctrl END")), "caret-end");
    harness.check(taim.get(KeyStroke.getKeyStroke("BACK_SPACE")), "delete-previous");
    harness.check(taim.get(KeyStroke.getKeyStroke("shift ctrl RIGHT")), "selection-next-word");
    harness.check(taim.get(KeyStroke.getKeyStroke("LEFT")), "caret-backward");
    harness.check(taim.get(KeyStroke.getKeyStroke("KP_LEFT")), "caret-backward");
    harness.check(taim.get(KeyStroke.getKeyStroke("shift KP_RIGHT")), "selection-forward");
    harness.check(taim.get(KeyStroke.getKeyStroke("ctrl SPACE")), "activate-link-action");
    harness.check(taim.get(KeyStroke.getKeyStroke("ctrl H")), "delete-previous");
    harness.check(taim.get(KeyStroke.getKeyStroke("ctrl BACK_SLASH")), "unselect");
    harness.check(taim.get(KeyStroke.getKeyStroke("ENTER")), "insert-break");
    harness.check(taim.get(KeyStroke.getKeyStroke("shift HOME")), "selection-begin-line");
    harness.check(taim.get(KeyStroke.getKeyStroke("RIGHT")), "caret-forward");
    harness.check(taim.get(KeyStroke.getKeyStroke("shift ctrl PAGE_UP")), "selection-page-left");
    harness.check(taim.get(KeyStroke.getKeyStroke("shift DOWN")), "selection-down");
    harness.check(taim.get(KeyStroke.getKeyStroke("PAGE_DOWN")), "page-down");
    harness.check(taim.get(KeyStroke.getKeyStroke("shift KP_LEFT")), "selection-backward");
    harness.check(taim.get(KeyStroke.getKeyStroke("shift ctrl O")), "toggle-componentOrientation");
    harness.check(taim.get(KeyStroke.getKeyStroke("ctrl X")), "cut-to-clipboard");
    harness.check(taim.get(KeyStroke.getKeyStroke("shift ctrl PAGE_DOWN")), "selection-page-right");
    harness.check(taim.get(KeyStroke.getKeyStroke("ctrl C")), "copy-to-clipboard");
    harness.check(taim.get(KeyStroke.getKeyStroke("ctrl KP_RIGHT")), "caret-next-word");
    harness.check(taim.get(KeyStroke.getKeyStroke("shift END")), "selection-end-line");
    harness.check(taim.get(KeyStroke.getKeyStroke("ctrl KP_LEFT")), "caret-previous-word");
    harness.check(taim.get(KeyStroke.getKeyStroke("HOME")), "caret-begin-line");
    harness.check(taim.get(KeyStroke.getKeyStroke("ctrl V")), "paste-from-clipboard");
    harness.check(taim.get(KeyStroke.getKeyStroke("KP_DOWN")), "caret-down");
    harness.check(taim.get(KeyStroke.getKeyStroke("ctrl A")), "select-all");
    harness.check(taim.get(KeyStroke.getKeyStroke("shift RIGHT")), "selection-forward");
    harness.check(taim.get(KeyStroke.getKeyStroke("shift ctrl END")), "selection-end");
    harness.check(taim.get(KeyStroke.getKeyStroke("COPY")), "copy-to-clipboard");
    harness.check(taim.get(KeyStroke.getKeyStroke("shift ctrl KP_LEFT")), "selection-previous-word");
    harness.check(taim.get(KeyStroke.getKeyStroke("ctrl T")), "next-link-action");
    harness.check(taim.get(KeyStroke.getKeyStroke("shift KP_DOWN")), "selection-down");
    harness.check(taim.get(KeyStroke.getKeyStroke("TAB")), "insert-tab");
    harness.check(taim.get(KeyStroke.getKeyStroke("UP")), "caret-up");
    harness.check(taim.get(KeyStroke.getKeyStroke("shift ctrl HOME")), "selection-begin");
    harness.check(taim.get(KeyStroke.getKeyStroke("shift PAGE_DOWN")), "selection-page-down");
    harness.check(taim.get(KeyStroke.getKeyStroke("KP_RIGHT")), "caret-forward");
    harness.check(taim.get(KeyStroke.getKeyStroke("shift ctrl KP_RIGHT")), "selection-next-word");
    harness.check(taim.get(KeyStroke.getKeyStroke("PAGE_UP")), "page-up");
    harness.check(taim.get(KeyStroke.getKeyStroke("PASTE")), "paste-from-clipboard");   
    
    harness.checkPoint("TextField");
    harness.check(defaults.get("TextField.border") instanceof BasicBorders.FieldBorder);
    harness.check(defaults.get("TextField.caretBlinkRate"), new Integer(500));
    harness.check(defaults.get("TextField.font"), new FontUIResource("SansSerif", Font.PLAIN, 12));
    harness.check(defaults.get("TextField.margin"), new InsetsUIResource(0, 0, 0, 0));
    harness.check(UIManager.get("TextField.focusInputMap") instanceof InputMapUIResource);
    Object tf = UIManager.get("TextField.focusInputMap");
    InputMapUIResource tfim = (InputMapUIResource) tf;
    harness.check(tfim.keys().length == 33);
    harness.check(tfim.get(KeyStroke.getKeyStroke("ENTER")), "notify-field-accept");
    harness.check(tfim.get(KeyStroke.getKeyStroke("LEFT")), "caret-backward");
    harness.check(tfim.get(KeyStroke.getKeyStroke("RIGHT")), "caret-forward");
    harness.check(tfim.get(KeyStroke.getKeyStroke("BACK_SPACE")), "delete-previous");
    harness.check(tfim.get(KeyStroke.getKeyStroke("ctrl X")), "cut-to-clipboard");
    harness.check(tfim.get(KeyStroke.getKeyStroke("ctrl C")), "copy-to-clipboard");
    harness.check(tfim.get(KeyStroke.getKeyStroke("ctrl V")), "paste-from-clipboard");
    harness.check(tfim.get(KeyStroke.getKeyStroke("shift LEFT")), "selection-backward");
    harness.check(tfim.get(KeyStroke.getKeyStroke("shift RIGHT")), "selection-forward");
    harness.check(tfim.get(KeyStroke.getKeyStroke("HOME")), "caret-begin-line");
    harness.check(tfim.get(KeyStroke.getKeyStroke("END")), "caret-end-line");
    harness.check(tfim.get(KeyStroke.getKeyStroke("DELETE")), "delete-next");
    harness.check(tfim.get(KeyStroke.getKeyStroke("shift ctrl O")), "toggle-componentOrientation");
    harness.check(tfim.get(KeyStroke.getKeyStroke("shift KP_LEFT")), "selection-backward");
    harness.check(tfim.get(KeyStroke.getKeyStroke("ctrl H")), "delete-previous");
    harness.check(tfim.get(KeyStroke.getKeyStroke("KP_LEFT")), "caret-backward");
    harness.check(tfim.get(KeyStroke.getKeyStroke("KP_RIGHT")), "caret-forward");
    harness.check(tfim.get(KeyStroke.getKeyStroke("shift ctrl KP_RIGHT")), "selection-next-word");
    harness.check(tfim.get(KeyStroke.getKeyStroke("COPY")), "copy-to-clipboard");
    harness.check(tfim.get(KeyStroke.getKeyStroke("shift HOME")), "selection-begin-line");
    harness.check(tfim.get(KeyStroke.getKeyStroke("shift ctrl LEFT")), "selection-previous-word");
    harness.check(tfim.get(KeyStroke.getKeyStroke("ctrl KP_LEFT")), "caret-previous-word");
    harness.check(tfim.get(KeyStroke.getKeyStroke("ctrl KP_RIGHT")), "caret-next-word");
    harness.check(tfim.get(KeyStroke.getKeyStroke("PASTE")), "paste-from-clipboard");
    harness.check(tfim.get(KeyStroke.getKeyStroke("shift ctrl RIGHT")), "selection-next-word");
    harness.check(tfim.get(KeyStroke.getKeyStroke("ctrl BACK_SLASH")), "unselect");
    harness.check(tfim.get(KeyStroke.getKeyStroke("ctrl A")), "select-all");
    harness.check(tfim.get(KeyStroke.getKeyStroke("shift KP_RIGHT")), "selection-forward");
    harness.check(tfim.get(KeyStroke.getKeyStroke("CUT")), "cut-to-clipboard");
    harness.check(tfim.get(KeyStroke.getKeyStroke("ctrl LEFT")), "caret-previous-word");
    harness.check(tfim.get(KeyStroke.getKeyStroke("shift ctrl KP_LEFT")), "selection-previous-word");
    harness.check(tfim.get(KeyStroke.getKeyStroke("shift END")), "selection-end-line");
    harness.check(tfim.get(KeyStroke.getKeyStroke("ctrl RIGHT")), "caret-next-word");
	
    harness.checkPoint("TextPane");
    harness.check(defaults.get("TextPane.background"), new ColorUIResource(255, 255, 255));
    harness.check(defaults.get("TextPane.border") instanceof BasicBorders.MarginBorder);
    harness.check(defaults.get("TextPane.caretBlinkRate"), new Integer(500));
    harness.check(defaults.get("TextPane.font"), new FontUIResource("Serif", Font.PLAIN, 12));
    harness.check(defaults.get("TextPane.margin"), new InsetsUIResource(3, 3, 3, 3));
    harness.check(UIManager.get("TextPane.focusInputMap") instanceof InputMapUIResource);
    Object tp = UIManager.get("TextPane.focusInputMap");
    InputMapUIResource tpim = (InputMapUIResource) tp;
    harness.check(tpim.keys().length == 55);
    harness.check(tpim.get(KeyStroke.getKeyStroke("shift UP")), "selection-up"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("ctrl RIGHT")), "caret-next-word"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("shift ctrl LEFT")), "selection-previous-word"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("shift KP_UP")), "selection-up"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("DOWN")), "caret-down"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("shift ctrl T")), "previous-link-action"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("ctrl LEFT")), "caret-previous-word"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("CUT")), "cut-to-clipboard"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("END")), "caret-end-line"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("shift PAGE_UP")), "selection-page-up"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("KP_UP")), "caret-up"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("DELETE")), "delete-next"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("ctrl HOME")), "caret-begin"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("shift LEFT")), "selection-backward"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("ctrl END")), "caret-end"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("BACK_SPACE")), "delete-previous"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("shift ctrl RIGHT")), "selection-next-word"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("LEFT")), "caret-backward"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("KP_LEFT")), "caret-backward"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("shift KP_RIGHT")), "selection-forward"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("ctrl SPACE")), "activate-link-action"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("ctrl H")), "delete-previous"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("ctrl BACK_SLASH")), "unselect"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("ENTER")), "insert-break"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("shift HOME")), "selection-begin-line"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("RIGHT")), "caret-forward"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("shift ctrl PAGE_UP")), "selection-page-left"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("shift DOWN")), "selection-down"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("PAGE_DOWN")), "page-down"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("shift KP_LEFT")), "selection-backward"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("shift ctrl O")), "toggle-componentOrientation"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("ctrl X")), "cut-to-clipboard"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("shift ctrl PAGE_DOWN")), "selection-page-right"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("ctrl C")), "copy-to-clipboard"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("ctrl KP_RIGHT")), "caret-next-word"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("shift END")), "selection-end-line"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("ctrl KP_LEFT")), "caret-previous-word"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("HOME")), "caret-begin-line"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("ctrl V")), "paste-from-clipboard"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("KP_DOWN")), "caret-down"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("ctrl A")), "select-all"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("shift RIGHT")), "selection-forward"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("shift ctrl END")), "selection-end"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("COPY")), "copy-to-clipboard"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("shift ctrl KP_LEFT")), "selection-previous-word"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("ctrl T")), "next-link-action"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("shift KP_DOWN")), "selection-down"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("TAB")), "insert-tab"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("UP")), "caret-up"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("shift ctrl HOME")), "selection-begin"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("shift PAGE_DOWN")), "selection-page-down"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("KP_RIGHT")), "caret-forward"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("shift ctrl KP_RIGHT")), "selection-next-word"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("PAGE_UP")), "page-up"); 
    harness.check(tpim.get(KeyStroke.getKeyStroke("PASTE")), "paste-from-clipboard");
    
    harness.checkPoint("TitledBorder");
    harness.check(defaults.get("TitledBorder.border") instanceof BorderUIResource.EtchedBorderUIResource);
    harness.check(defaults.get("TitledBorder.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    
    harness.checkPoint("ToggleButton");
    harness.check(defaults.get("ToggleButton.border") instanceof BorderUIResource.CompoundBorderUIResource);
    harness.check(defaults.get("ToggleButton.focusInputMap") instanceof InputMapUIResource);
    Object to = UIManager.get("ToggleButton.focusInputMap");
    InputMapUIResource toim = (InputMapUIResource) to;
    harness.check(toim.keys().length == 2);
    harness.check(toim.get(KeyStroke.getKeyStroke("SPACE")),  "pressed");
    harness.check(toim.get(KeyStroke.getKeyStroke("released SPACE")), "released");
    harness.check(defaults.get("ToggleButton.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("ToggleButton.margin"), new InsetsUIResource(2, 14, 2, 14));
    harness.check(defaults.get("ToggleButton.textIconGap"), new Integer(4));
    harness.check(defaults.get("ToggleButton.textShiftOffset"), new Integer(0));
    
    harness.checkPoint("ToolBar");
    harness.check(defaults.get("ToolBar.ancestorInputMap") instanceof InputMapUIResource);
    harness.check(defaults.get("ToolBar.border") instanceof BorderUIResource.EtchedBorderUIResource);
    harness.check(defaults.get("ToolBar.dockingForeground"), new ColorUIResource(255, 0, 0));
    harness.check(defaults.get("ToolBar.floatingForeground"), new ColorUIResource(64, 64, 64));
    harness.check(defaults.get("ToolBar.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("ToolBar.separatorSize"), new DimensionUIResource(10, 10));
    
    harness.checkPoint("ToolTip");
    harness.check(defaults.get("ToolTip.border") instanceof BorderUIResource.LineBorderUIResource);
    harness.check(defaults.get("ToolTip.font"), new FontUIResource("SansSerif", Font.PLAIN, 12));
    
    harness.checkPoint("Tree");
    harness.check(defaults.get("Tree.ancestorInputMap") instanceof InputMapUIResource);
    harness.check(defaults.get("Tree.changeSelectionWithFocus"), Boolean.TRUE);
    harness.check(defaults.get("Tree.drawsFocusBorderAroundIcon"), Boolean.FALSE);
    harness.check(defaults.get("Tree.editorBorder") instanceof BorderUIResource.LineBorderUIResource);
    harness.check(defaults.get("Tree.focusInputMap") instanceof InputMapUIResource);
    Object tr = UIManager.get("Tree.focusInputMap");
    InputMapUIResource trim = (InputMapUIResource) tr;
    harness.check(trim.keys().length == 56);
	harness.check(trim.get(KeyStroke.getKeyStroke("ctrl DOWN")), "selectNextChangeLead");
	harness.check(trim.get(KeyStroke.getKeyStroke("shift UP")), "selectPreviousExtendSelection");
	harness.check(trim.get(KeyStroke.getKeyStroke("ctrl RIGHT")), "scrollRight");
	harness.check(trim.get(KeyStroke.getKeyStroke("shift KP_UP")), "selectPreviousExtendSelection");
	harness.check(trim.get(KeyStroke.getKeyStroke("DOWN")), "selectNext");
	harness.check(trim.get(KeyStroke.getKeyStroke("ctrl UP")), "selectPreviousChangeLead");
	harness.check(trim.get(KeyStroke.getKeyStroke("ctrl LEFT")), "scrollLeft");
	harness.check(trim.get(KeyStroke.getKeyStroke("CUT")), "cut");
	harness.check(trim.get(KeyStroke.getKeyStroke("END")), "selectLast");
	harness.check(trim.get(KeyStroke.getKeyStroke("shift PAGE_UP")), "scrollUpExtendSelection");
	harness.check(trim.get(KeyStroke.getKeyStroke("KP_UP")), "selectPrevious");
	harness.check(trim.get(KeyStroke.getKeyStroke("shift ctrl UP")), "selectPreviousExtendSelection");
	harness.check(trim.get(KeyStroke.getKeyStroke("ctrl HOME")), "selectFirstChangeLead");
	harness.check(trim.get(KeyStroke.getKeyStroke("ctrl END")), "selectLastChangeLead");
	harness.check(trim.get(KeyStroke.getKeyStroke("ctrl PAGE_DOWN")), "scrollDownChangeLead");
	harness.check(trim.get(KeyStroke.getKeyStroke("LEFT")), "selectParent");
	harness.check(trim.get(KeyStroke.getKeyStroke("ctrl PAGE_UP")), "scrollUpChangeLead");
	harness.check(trim.get(KeyStroke.getKeyStroke("KP_LEFT")), "selectParent");
	harness.check(trim.get(KeyStroke.getKeyStroke("SPACE")), "addToSelection");
	harness.check(trim.get(KeyStroke.getKeyStroke("ctrl SPACE")), "toggleAndAnchor");
	harness.check(trim.get(KeyStroke.getKeyStroke("shift SPACE")), "extendTo");
	harness.check(trim.get(KeyStroke.getKeyStroke("shift ctrl SPACE")), "moveSelectionTo");
	harness.check(trim.get(KeyStroke.getKeyStroke("ADD")), "expand");
	harness.check(trim.get(KeyStroke.getKeyStroke("ctrl BACK_SLASH")), "clearSelection");
	harness.check(trim.get(KeyStroke.getKeyStroke("shift ctrl DOWN")), "selectNextExtendSelection");
	harness.check(trim.get(KeyStroke.getKeyStroke("shift HOME")), "selectFirstExtendSelection");
	harness.check(trim.get(KeyStroke.getKeyStroke("RIGHT")), "selectChild");
	harness.check(trim.get(KeyStroke.getKeyStroke("shift ctrl PAGE_UP")), "scrollUpExtendSelection");
	harness.check(trim.get(KeyStroke.getKeyStroke("shift DOWN")), "selectNextExtendSelection");
	harness.check(trim.get(KeyStroke.getKeyStroke("PAGE_DOWN")), "scrollDownChangeSelection");
	harness.check(trim.get(KeyStroke.getKeyStroke("shift ctrl KP_UP")), "selectPreviousExtendSelection");
	harness.check(trim.get(KeyStroke.getKeyStroke("SUBTRACT")), "collapse");
	harness.check(trim.get(KeyStroke.getKeyStroke("ctrl X")), "cut");
	harness.check(trim.get(KeyStroke.getKeyStroke("shift ctrl PAGE_DOWN")), "scrollDownExtendSelection");
	harness.check(trim.get(KeyStroke.getKeyStroke("ctrl SLASH")), "selectAll");
	harness.check(trim.get(KeyStroke.getKeyStroke("ctrl C")), "copy");
	harness.check(trim.get(KeyStroke.getKeyStroke("ctrl KP_RIGHT")), "scrollRight");
	harness.check(trim.get(KeyStroke.getKeyStroke("shift END")), "selectLastExtendSelection");
	harness.check(trim.get(KeyStroke.getKeyStroke("shift ctrl KP_DOWN")), "selectNextExtendSelection");
	harness.check(trim.get(KeyStroke.getKeyStroke("ctrl KP_LEFT")), "scrollLeft");
	harness.check(trim.get(KeyStroke.getKeyStroke("HOME")), "selectFirst");
	harness.check(trim.get(KeyStroke.getKeyStroke("ctrl V")), "paste");
	harness.check(trim.get(KeyStroke.getKeyStroke("KP_DOWN")), "selectNext");
	harness.check(trim.get(KeyStroke.getKeyStroke("ctrl A")), "selectAll");
	harness.check(trim.get(KeyStroke.getKeyStroke("ctrl KP_DOWN")), "selectNextChangeLead");
	harness.check(trim.get(KeyStroke.getKeyStroke("shift ctrl END")), "selectLastExtendSelection");
	harness.check(trim.get(KeyStroke.getKeyStroke("COPY")), "copy");
	harness.check(trim.get(KeyStroke.getKeyStroke("ctrl KP_UP")), "selectPreviousChangeLead");
	harness.check(trim.get(KeyStroke.getKeyStroke("shift KP_DOWN")), "selectNextExtendSelection");
	harness.check(trim.get(KeyStroke.getKeyStroke("UP")), "selectPrevious");
	harness.check(trim.get(KeyStroke.getKeyStroke("shift ctrl HOME")), "selectFirstExtendSelection");
	harness.check(trim.get(KeyStroke.getKeyStroke("shift PAGE_DOWN")), "scrollDownExtendSelection");
	harness.check(trim.get(KeyStroke.getKeyStroke("KP_RIGHT")), "selectChild");
	harness.check(trim.get(KeyStroke.getKeyStroke("F2")), "startEditing");
	harness.check(trim.get(KeyStroke.getKeyStroke("PAGE_UP")), "scrollUpChangeSelection");
	harness.check(trim.get(KeyStroke.getKeyStroke("PASTE")), "paste");    
    harness.check(defaults.get("Tree.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
    harness.check(defaults.get("Tree.hash"), new ColorUIResource(128, 128, 128));
    harness.check(defaults.get("Tree.leftChildIndent"), new Integer(7));
    harness.check(defaults.get("Tree.rightChildIndent"), new Integer(13));
    harness.check(defaults.get("Tree.rowHeight"), new Integer(16));
    harness.check(defaults.get("Tree.scrollsOnExpand"), Boolean.TRUE);
    harness.check(defaults.get("Tree.selectionBorderColor"), new ColorUIResource(0, 0, 0));
    harness.check(defaults.get("Tree.focusInputMap.RightToLeft") instanceof InputMapUIResource);

    harness.checkPoint("Viewport");
    harness.check(defaults.get("Viewport.font"), new FontUIResource("Dialog", Font.PLAIN, 12));
  }
}
