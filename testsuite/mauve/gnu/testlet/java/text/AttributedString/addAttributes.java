// Tags: JDK1.2

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

package gnu.testlet.java.text.AttributedString;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Color;
import java.awt.font.TextAttribute;
import java.text.AttributedCharacterIterator;
import java.text.AttributedString;
import java.util.HashMap;

/**
 * Some checks for the addAttributes method in the {@link AttributedString} 
 * class.  
 */
public class addAttributes implements Testlet
{

  /**
   * Runs the test using the specified harness. 
   * 
   * @param harness  the test harness (<code>null</code> not allowed).
   */
  public void test(TestHarness harness) 
  {
    harness.checkPoint("addAttributes(Map, int, int)"); 
    AttributedString as = new AttributedString("ABCDEFG");
    HashMap attributes = new HashMap();
    attributes.put(TextAttribute.BACKGROUND, Color.red);
    attributes.put(TextAttribute.FOREGROUND, Color.yellow);
    as.addAttributes(attributes, 2, 4);
    AttributedCharacterIterator aci = as.getIterator();
    aci.first();
    harness.check(aci.getRunStart(TextAttribute.BACKGROUND), 0);
    harness.check(aci.getRunLimit(TextAttribute.BACKGROUND), 2);
    aci.next();
    aci.next();
    harness.check(aci.getRunStart(TextAttribute.BACKGROUND), 2);
    harness.check(aci.getRunLimit(TextAttribute.BACKGROUND), 4);
    aci.next();
    aci.next();
    harness.check(aci.getRunStart(TextAttribute.BACKGROUND), 4);
    harness.check(aci.getRunLimit(TextAttribute.BACKGROUND), 7);

    // check null map
    boolean pass = false;
    try
    {
      as.addAttributes(null, 2, 4);   
    }
    catch (NullPointerException e) 
    {
      pass = true;   
    }
    harness.check(pass);
    
    // check negative beginIndex
    pass = false;
    try
    {
      as.addAttributes(attributes, -1, 4);   
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
    
    // check endIndex > string length
    pass = false;
    try
    {
      as.addAttributes(attributes, 2, 8);   
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;   
    }
    harness.check(pass);
    
    // check indices with zero range
    pass = false;
    try
    {
      as.addAttributes(attributes, 2, 2);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
    
  }

}
