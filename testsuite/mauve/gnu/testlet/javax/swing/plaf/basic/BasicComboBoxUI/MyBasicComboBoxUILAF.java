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

package gnu.testlet.javax.swing.plaf.basic.BasicComboBoxUI;

import javax.swing.UIDefaults;
import javax.swing.plaf.basic.BasicLookAndFeel;

public class MyBasicComboBoxUILAF extends BasicLookAndFeel {

  public String getID() 
  {
    return "MyBasicLookAndFeel";
  }
  public String getName() 
  {
    return "MyBasicLookAndFeel";
  }
  public String getDescription() 
  {
    return "MyBasicLookAndFeel";
  }
  public boolean isSupportedLookAndFeel() 
  {
    return true;    
  }
  public boolean isNativeLookAndFeel() 
  {
    return false;    
  }
  public void initSystemColorDefaults(UIDefaults defaults)
  {
    super.initSystemColorDefaults(defaults);
  }
  public void initComponentDefaults(UIDefaults defaults)
  {
    super.initComponentDefaults(defaults);
  }
  public void initClassDefaults(UIDefaults defaults)
  {
    super.initClassDefaults(defaults);
  }
}
