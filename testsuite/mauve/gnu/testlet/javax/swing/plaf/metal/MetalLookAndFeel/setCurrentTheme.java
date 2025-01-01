// Tags: JDK1.2

// Copyright (C) 2005, 2006 Roman Kennke <roman@kennke.org>

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

import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.metal.DefaultMetalTheme;
import javax.swing.plaf.metal.MetalLookAndFeel;

/**
 * Checks if MetalLookAndFeel.setCurrentTheme works correctly.
 *
 * @author Roman Kennke
 */
public class setCurrentTheme implements Testlet
{
  public void test(TestHarness h)
  {
    DefaultMetalTheme theme = new DefaultMetalTheme();
    MetalLookAndFeel.setCurrentTheme(theme);
    MetalLookAndFeel laf = new MetalLookAndFeel();
    Color c1 = laf.getDefaults().getColor("Button.background");
    h.check(c1, theme.getControl());
    MetalLookAndFeel.setCurrentTheme(new TestTheme());
    c1 = laf.getDefaults().getColor("Button.background");
    h.check(c1, Color.red);

    // reset the theme so that other tests won't be affected
    MetalLookAndFeel.setCurrentTheme(new DefaultMetalTheme());
  }

  class TestTheme extends DefaultMetalTheme 
  {
    protected ColorUIResource getSecondary3()
    {
      return new ColorUIResource(Color.red);
    }
  }
  
}
