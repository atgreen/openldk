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

package gnu.testlet.javax.swing.plaf.basic.BasicSliderUI;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Rectangle;

import javax.swing.JSlider;
import javax.swing.plaf.basic.BasicSliderUI;

/**
 * A utility class to provide access to protected attributes and methods.
 */
public class MyBasicSliderUI extends BasicSliderUI {
    
  public MyBasicSliderUI(JSlider slider) 
  {
    super(slider);    
  }

  public JSlider getSlider() 
  {
    return this.slider;
  }
  
  public Color getFocusColor() 
  {
    return super.getFocusColor();
  }
  
  public Color getHighlightColor() 
  {
    return super.getHighlightColor();
  }
  
  public Color getShadowColor() 
  {
    return super.getShadowColor();
  }
  
  public int getTrackBuffer() 
  {
    return super.trackBuffer;
  }
  
  public int getTickLength() 
  {
    return super.getTickLength();
  }
  
  public Dimension getThumbSize() {
    return super.getThumbSize();
  }
  
  public Rectangle getFocusRect() 
  {
    return this.focusRect;
  }
  
  public Rectangle getContentRect() 
  {
    return this.contentRect;    
  }
  
  public Rectangle getTrackRect() 
  {
    return this.trackRect;
  }
  
  public Rectangle getThumbRect() 
  {
    return this.thumbRect;
  }
  
  public Rectangle getTickRect() 
  {
    return this.tickRect;
  }
  
  public void calculateGeometry() 
  {
    super.calculateGeometry();    
  }
  
  public int xPositionForValue(int v)
  {
    return super.xPositionForValue(v);
  }
  
  public int yPositionForValue(int v) 
  {
    return super.yPositionForValue(v);
  }
}
