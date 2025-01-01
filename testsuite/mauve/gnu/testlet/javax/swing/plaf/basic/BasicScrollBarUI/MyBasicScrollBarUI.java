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

package gnu.testlet.javax.swing.plaf.basic.BasicScrollBarUI;

import java.awt.Dimension;
import java.awt.Rectangle;

import javax.swing.JButton;
import javax.swing.JScrollBar;
import javax.swing.plaf.basic.BasicScrollBarUI;

/**
 * Provides access to protected fields and methods.
 */
public class MyBasicScrollBarUI extends BasicScrollBarUI 
{
  public Dimension getMinimumThumbSize()
  {
    return super.getMinimumThumbSize();
  }
  
  public Dimension getMaximumThumbSize() 
  {
    return super.getMaximumThumbSize();
  }
  
  public Rectangle getTrackBounds() 
  {
    return super.getTrackBounds();
  }
  
  public Rectangle getThumbBounds()
  {
    return super.getThumbBounds();
  }

  /**
   * Overrides createIncreaseButton() to enable public access in tests.
   *
   * @param o the orientation
   */
  public JButton createIncreaseButton(int o)
  {
    return super.createIncreaseButton(o);
  }

  /**
   * Overrides createDecreaseButton() to enable public access in tests.
   *
   * @param o the orientation
   */
  public JButton createDecreaseButton(int o)
  {
    return super.createDecreaseButton(o);
  }

  /**
   * Overrides installDefaults() to enable public access in tests.
   */
  public void installDefaults()
  {
    super.installDefaults();
  }

  /**
   * Overrides installComponents() to enable public access in tests.
   */
  public void installComponents()
  {
    super.installComponents();
  }

  /**
   * Returns the value of the (otherwise protected) field incrButton.
   *
   * @return the value of the incrButton field
   */
  public JButton getIncrButton()
  {
    return incrButton;
  }

  /**
   * Sets the value of the (otherwise protected) field incrButton.
   *
   * @param b the button to set
   */
  public void setIncrButton(JButton b)
  {
    incrButton = b;
  }

  /**
   * Returns the value of the (otherwise protected) field decrButton.
   *
   * @return the value of the decrButton field
   */
  public JButton getDecrButton()
  {
    return decrButton;
  }

  /**
   * Sets the value of the (otherwise protected) field decrButton.
   *
   * @param b the button to set
   */
  public void setDecrButton(JButton b)
  {
    decrButton = b;
  }

  /**
   * Sets the value of the (otherwise protected) field scrollbar.
   *
   * @param sb the scrollbar to set
   */
  public void setScrollbar(JScrollBar sb)
  {
    scrollbar = sb;
  }
}
