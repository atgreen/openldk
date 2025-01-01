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
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.javax.swing.plaf.basic.BasicLookAndFeel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.UIDefaults;
import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.basic.BasicLookAndFeel;

/**
 * Some checks for the initSystemColorDefaults() method in the 
 * {@link BasicLookAndFeel} class.  
 */
public class initSystemColorDefaults implements Testlet
{

  /**
   * Runs the test using the specified harness.  
   * 
   * @param harness  the test harness (<code>null</code> not allowed).
   */
  public void test(TestHarness harness) 
  {
    MyBasicLookAndFeel laf = new MyBasicLookAndFeel();
    UIDefaults defaults = new UIDefaults();
    laf.initSystemColorDefaults(defaults);
    harness.check(defaults.get("activeCaption"), new ColorUIResource(0, 0, 128));
    harness.check(defaults.get("activeCaptionBorder"), new ColorUIResource(192, 192, 192));
    harness.check(defaults.get("activeCaptionText"), new ColorUIResource(255, 255, 255));
    harness.check(defaults.get("control"), new ColorUIResource(192, 192, 192));
    harness.check(defaults.get("controlDkShadow"), new ColorUIResource(0, 0, 0));
    harness.check(defaults.get("controlHighlight"), new ColorUIResource(192, 192, 192));
    harness.check(defaults.get("controlLtHighlight"), new ColorUIResource(255, 255, 255));
    harness.check(defaults.get("controlShadow"), new ColorUIResource(128, 128, 128));
    harness.check(defaults.get("controlText"), new ColorUIResource(0, 0, 0));
    harness.check(defaults.get("desktop"), new ColorUIResource(0, 92, 92));
    harness.check(defaults.get("inactiveCaption"), new ColorUIResource(128, 128, 128));
    harness.check(defaults.get("inactiveCaptionBorder"), new ColorUIResource(192, 192, 192));
    harness.check(defaults.get("inactiveCaptionText"), new ColorUIResource(192, 192, 192));
    harness.check(defaults.get("info"), new ColorUIResource(255, 255, 225));
    harness.check(defaults.get("infoText"), new ColorUIResource(0, 0, 0));
    harness.check(defaults.get("menu"), new ColorUIResource(192, 192, 192));
    harness.check(defaults.get("menuText"), new ColorUIResource(0, 0, 0));
    harness.check(defaults.get("scrollbar"), new ColorUIResource(224, 224, 224));
    harness.check(defaults.get("text"), new ColorUIResource(192, 192, 192));
    harness.check(defaults.get("textHighlight"), new ColorUIResource(0, 0, 128));
    harness.check(defaults.get("textHighlightText"), new ColorUIResource(255, 255, 255));
    harness.check(defaults.get("textInactiveText"), new ColorUIResource(128, 128, 128));
    harness.check(defaults.get("textText"), new ColorUIResource(0, 0, 0));
    harness.check(defaults.get("window"), new ColorUIResource(255, 255, 255));
    harness.check(defaults.get("windowBorder"), new ColorUIResource(0, 0, 0));
    harness.check(defaults.get("windowText"), new ColorUIResource(0, 0, 0));
  }
}
