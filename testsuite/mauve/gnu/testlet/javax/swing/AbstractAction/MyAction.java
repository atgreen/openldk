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
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.javax.swing.AbstractAction;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Icon;

/**
 * A custom action used for testing.
 */
public class MyAction extends AbstractAction {

  public MyAction()
  {
    super(); 
  }
  
  public MyAction(String name) 
  {
    super(name); 
  }
  
  public MyAction(String name, Icon icon) 
  {
    super(name, icon); 
  }
  
  public void actionPerformed(ActionEvent e) {
    
  }
  
  public Object clone() throws CloneNotSupportedException 
  {
    return super.clone(); 
  }
  
}
