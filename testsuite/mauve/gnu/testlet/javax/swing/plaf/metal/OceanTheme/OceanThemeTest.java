// Tags: JDK1.5

// Copyright (C) Roman Kennke (kennke@aicas.com)

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

package gnu.testlet.javax.swing.plaf.metal.OceanTheme;

import java.awt.Color;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.metal.OceanTheme;

/**
 * Checks if the values of the OceanTheme are correct.
 * @author Roman Kennke (kennke@aicas.com)
 */
public class OceanThemeTest implements Testlet
{
  /**
   * Extends OceanTheme to make protected methods public.
   */
  class TestOceanTheme extends OceanTheme
  {
    public ColorUIResource getPrimary1()
    {
      return super.getPrimary1();
    }
    public ColorUIResource getPrimary2()
    {
      return super.getPrimary2();
    }
    public ColorUIResource getPrimary3()
    {
      return super.getPrimary3();
    }
    public ColorUIResource getSecondary1()
    {
      return super.getSecondary1();
    }
    public ColorUIResource getSecondary2()
    {
      return super.getSecondary2();
    }
    public ColorUIResource getSecondary3()
    {
      return super.getSecondary3();
    }
    public ColorUIResource getBlack()
    {
      return super.getBlack();
    }
  }

  public void test(TestHarness harness)
  {
    TestOceanTheme theme = new TestOceanTheme();
    harness.check(theme.getBlack(), new Color(51, 51, 51));
    harness.check(theme.getControlTextColor(), new Color(51, 51, 51));
    harness.check(theme.getDesktopColor(), Color.WHITE);
    harness.check(theme.getInactiveControlTextColor(), new Color(153, 153, 153));
    harness.check(theme.getMenuDisabledForeground(), new Color(153, 153, 153));
    harness.check(theme.getName(), "Ocean");
    harness.check(theme.getPrimary1(), new Color(99, 130, 191));
    harness.check(theme.getPrimary2(), new Color(163, 184, 204));
    harness.check(theme.getPrimary3(), new Color(184, 207, 229));
    harness.check(theme.getSecondary1(), new Color(122, 138, 153));
    harness.check(theme.getSecondary2(), new Color(184, 207, 229));
    harness.check(theme.getSecondary3(), new Color(238, 238, 238));
  }

}
