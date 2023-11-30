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

package gnu.testlet.javax.swing.plaf.basic.BasicButtonUI;

import javax.swing.plaf.basic.BasicButtonUI;

/**
 * Provides access to protected methods.
 */
public class MyBasicButtonUI extends BasicButtonUI 
{
  public MyBasicButtonUI()
  {
    super();
  }
  public int getDefaultTextIconGapField() 
  {
    return this.defaultTextIconGap;
  }
  public int getDefaultTextShiftOffsetField()
  {
    return this.defaultTextShiftOffset;
  }
  public void setDefaultTextShiftOffsetField(int offset)
  {
    this.defaultTextShiftOffset = offset;
  }
  public String getPropertyPrefix()
  {
    return super.getPropertyPrefix();
  }
  public int getTextShiftOffset()
  {
    return super.getTextShiftOffset();
  }
  public void setTextShiftOffset()
  {
    super.setTextShiftOffset();
  }
  public void clearTextShiftOffset()
  {
    super.clearTextShiftOffset();
  }
}
