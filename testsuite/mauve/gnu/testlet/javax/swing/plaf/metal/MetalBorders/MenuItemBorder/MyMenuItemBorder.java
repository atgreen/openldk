/*
 * Created on Jul 26, 2005
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package gnu.testlet.javax.swing.plaf.metal.MetalBorders.MenuItemBorder;

import java.awt.Insets;

import javax.swing.plaf.metal.MetalBorders;

/**
 *
 */
public class MyMenuItemBorder extends MetalBorders.MenuItemBorder 
{
  public MyMenuItemBorder() 
  {
  }
  
  /**
   * Provides access to the border insets.
   * 
   * @return The border insets.
   */
  public static Insets getBorderInsets()
  {
    return MetalBorders.MenuItemBorder.borderInsets;
  }
}
