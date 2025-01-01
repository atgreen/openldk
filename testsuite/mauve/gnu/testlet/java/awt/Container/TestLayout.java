// Tags: not-a-test

// Copyright (C) 2005 Roman Kennke <kennke@aicas.com>

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

package gnu.testlet.java.awt.Container;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.LayoutManager2;

public class TestLayout implements LayoutManager2
{

  float alignmentX = 0.0F;
  float alignmentY = 0.0F;

  public void addLayoutComponent(Component component, Object constraints)
  {
  }

  public Dimension maximumLayoutSize(Container target)
  {
    return null;
  }

  public float getLayoutAlignmentX(Container target)
  {
    return alignmentX;
  }

  public float getLayoutAlignmentY(Container target)
  {
    return alignmentY;
  }

  public void invalidateLayout(Container target)
  {
  }

  public void addLayoutComponent(String name, Component component)
  {
  }

  public void removeLayoutComponent(Component component)
  {
  }

  public Dimension preferredLayoutSize(Container parent)
  {
    return null;
  }

  public Dimension minimumLayoutSize(Container parent)
  {
    return null;
  }

  public void layoutContainer(Container parent)
  {
  }

}
