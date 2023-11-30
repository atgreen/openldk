// Tags: not-a-test

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

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

package gnu.testlet.javax.swing.UIManager;

import javax.swing.LookAndFeel;
import javax.swing.UIDefaults;

/**
 * A minimal look and feel class for use in testing.
 */
public class MyLookAndFeel extends LookAndFeel {
  public MyLookAndFeel() {
  }
  public String getDescription() {
    return "MyLookAndFeel Description";
  }
  public String getID() {
    return "MyLookAndFeel ID";
  }
  public String getName() {
    return "MyLookAndFeel Name";
  }
  public boolean isNativeLookAndFeel() {
    return false;
  }
  public boolean isSupportedLookAndFeel() {
    return true;
  }
  public UIDefaults getDefaults()
  {
    UIDefaults def = new UIDefaults();
    def.put("LabelUI", "gnu.testlet.javax.swing.UIManager.TestLabelUI");
    return def;
  }

}
