/* paramString.java 
   Copyright (C) 2006 RedHat
   
This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/

// Tags: JDK1.1

package gnu.testlet.java.awt.event.ComponentEvent;

import java.awt.Button;
import java.awt.event.ComponentEvent;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class paramString implements Testlet
{

  public void test(TestHarness harness)
  {
    Button button = new Button();
    ComponentEvent event = new ComponentEvent(button, 
                                              ComponentEvent.COMPONENT_MOVED);

    // Check that previous code produced incorrect string representation.
    harness.check(! event.paramString().equalsIgnoreCase(
              "COMPONENT_MOVED java.awt.Rectangle[x=0,y=0,width=0,height=0]"));
    
    // Check that current code produces correct string representation.
    harness.check(event.paramString(), "COMPONENT_MOVED (0,0 0x0)");
    
    // Check that correct string representation is returned when
    // an invalid event ID is given.
    event = new ComponentEvent(button, ComponentEvent.COMPONENT_MOVED + 1024);
    harness.check(event.paramString(), "unknown type");
  }

}
