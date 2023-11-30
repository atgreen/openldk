// Tags: JDK1.2

// Copyright (C) 2004 Free Software Foundation, Inc.
// Written by Michael Koch (konqueror@gmx.de)

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

package gnu.testlet.javax.swing.plaf.metal.MetalTheme;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import javax.swing.plaf.*;
import javax.swing.plaf.metal.*;

public class MetalThemeTest extends MetalTheme implements Testlet
{
  ColorUIResource primary1 = new ColorUIResource(1, 0, 0);
  ColorUIResource primary2 = new ColorUIResource(2, 0, 0);
  ColorUIResource primary3 = new ColorUIResource(3, 0, 0);
  ColorUIResource secondary1 = new ColorUIResource(4, 0, 0);
  ColorUIResource secondary2 = new ColorUIResource(5, 0, 0);
  ColorUIResource secondary3 = new ColorUIResource(6, 0, 0);
  
  private void check(TestHarness h, ColorUIResource color, int index)
  {
    h.check(color.getRed(), index, "wrong color returned");
  }
  
  public void test(TestHarness h)
  {
    check(h, getAcceleratorForeground(), 1);
    check(h, getAcceleratorSelectedForeground(), 0);
    check(h, getBlack(), 0);
    check(h, getControl(), 6);
    check(h, getControlDarkShadow(), 4);
    check(h, getControlDisabled(), 5);
    check(h, getControlHighlight(), 255);
    check(h, getControlInfo(), 0);
    check(h, getControlShadow(), 5);
    check(h, getControlTextColor(), 0);
    check(h, getDesktopColor(), 2);
    check(h, getFocusColor(), 2);
    check(h, getHighlightedTextColor(), 0);
    check(h, getInactiveControlTextColor(), 5);
    check(h, getInactiveSystemTextColor(), 5);
    check(h, getMenuBackground(), 6);
    check(h, getMenuDisabledForeground() , 5);
    check(h, getMenuForeground(), 0);
    check(h, getMenuSelectedBackground(), 2);
    check(h, getMenuSelectedForeground(), 0);
    check(h, getPrimaryControl(), 3);
    check(h, getPrimaryControlDarkShadow(), 1);
    check(h, getPrimaryControlHighlight(), 255);
    check(h, getPrimaryControlInfo(), 0);
    check(h, getPrimaryControlShadow(), 2);
    check(h, getSeparatorBackground(), 255);
    check(h, getSeparatorForeground(), 1);
    check(h, getSystemTextColor(), 0);
    check(h, getTextHighlightColor(), 3);
    check(h, getUserTextColor(), 0);
    check(h, getWhite(), 255);
    check(h, getWindowBackground(), 255);
    check(h, getWindowTitleBackground(), 3);
    check(h, getWindowTitleForeground(), 0);
    check(h, getWindowTitleInactiveBackground(), 6);
    check(h, getWindowTitleInactiveForeground(), 0);
  }

  public ColorUIResource getPrimary1() { return primary1; }
  public ColorUIResource getPrimary2() { return primary2; }
  public ColorUIResource getPrimary3() { return primary3; }
  public ColorUIResource getSecondary1() { return secondary1; }
  public ColorUIResource getSecondary2() { return secondary2; }
  public ColorUIResource getSecondary3() { return secondary3; }

  public String getName() { return "mauve testcase"; }

  public FontUIResource getControlTextFont() { return null; }
  public FontUIResource getMenuTextFont() { return null; }
  public FontUIResource getSubTextFont() { return null; }
  public FontUIResource getSystemTextFont() { return null; }
  public FontUIResource getUserTextFont() { return null; }
  public FontUIResource getWindowTitleFont() { return null; }
}
