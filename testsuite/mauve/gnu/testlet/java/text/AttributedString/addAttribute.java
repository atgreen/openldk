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
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
// Boston, MA 02110-1301 USA.

package gnu.testlet.java.text.AttributedString;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.text.AttributedCharacterIterator;
import java.text.AttributedString;
import java.util.Locale;
import java.util.Map;

/**
 * Some checks for the addAttribute() methods in the {@link AttributedString} 
 * class.  
 */
public class addAttribute implements Testlet
{

  /**
   * Runs the test using the specified harness. 
   * 
   * @param harness  the test harness (<code>null</code> not allowed).
   */
  public void test(TestHarness harness) 
  {
    test1(harness);
    test2(harness);
  }

  private void test1(TestHarness harness) 
  {
    harness.checkPoint("addAttribute(AttributedCharacterIterator.Attribute, Object);"); 
    AttributedString as = new AttributedString("ABCDEFG");
    as.addAttribute(AttributedCharacterIterator.Attribute.LANGUAGE, Locale.ENGLISH);
    AttributedCharacterIterator aci = as.getIterator();
    harness.check(aci.getRunStart(AttributedCharacterIterator.Attribute.LANGUAGE) == 0);
    harness.check(aci.getRunLimit(AttributedCharacterIterator.Attribute.LANGUAGE) == 7);
    
    // test adding an attribute to a zero length string
    boolean pass = false;
    as = new AttributedString("");
    try
    {
      as.addAttribute(AttributedCharacterIterator.Attribute.LANGUAGE, "Unknown");
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;   
    }
    harness.check(pass);
    
    // test adding an attribute with a null value (permitted)
    pass = true;
    as = new AttributedString("123");
    try
    {
      as.addAttribute(AttributedCharacterIterator.Attribute.LANGUAGE, null);
    }
    catch (Exception e) 
    {
      pass = false;   
    }
    harness.check(pass);
    aci = as.getIterator();
    Map attributes = aci.getAttributes();
    harness.check(attributes.get(AttributedCharacterIterator.Attribute.LANGUAGE), null);
  }

  private void test2(TestHarness harness) 
  {
     harness.checkPoint("addAttribute(AttributedCharacterIterator.Attribute, Object, int, int);"); 
     AttributedString as = new AttributedString("ABCDEFG");
     as.addAttribute(AttributedCharacterIterator.Attribute.LANGUAGE, "Unknown", 2, 4);
     AttributedCharacterIterator aci = as.getIterator();
     harness.check(aci.getRunStart(AttributedCharacterIterator.Attribute.LANGUAGE), 0);
     harness.check(aci.getRunLimit(AttributedCharacterIterator.Attribute.LANGUAGE), 2);
     aci.next();
     aci.next();
     aci.next();
     harness.check(aci.getRunStart(AttributedCharacterIterator.Attribute.LANGUAGE), 2);
     harness.check(aci.getRunLimit(AttributedCharacterIterator.Attribute.LANGUAGE), 4);     
 
     // if beginIndex < 0, there should be an IllegalArgumentException
     boolean pass = false;
     try
     {
       as = new AttributedString("ABC");
       as.addAttribute(AttributedCharacterIterator.Attribute.LANGUAGE, Locale.FRANCE, -1, 1);
     }
     catch (IllegalArgumentException e)
     {
       pass = true;  
     }
     harness.check(pass);
     
     // if end index > length of string, there should be an 
     // IllegalArgumentException
     pass = false;
     try
     {
       as = new AttributedString("XYZ");
       as.addAttribute(AttributedCharacterIterator.Attribute.LANGUAGE, 
            Locale.FRANCE, 1, 3);      
     }
     catch (IllegalArgumentException e)
     {
       pass = true;
     }
     harness.check(true);
     
     // if start index == end index, there should be an IllegalArgumentException
     pass = false;
     try
     {
       as = new AttributedString("123");
       as.addAttribute(AttributedCharacterIterator.Attribute.LANGUAGE, 
            Locale.FRANCE, 1, 1);      
     }
     catch (IllegalArgumentException e)
     {
       pass = true;  
     }
     harness.check(pass);
  }

}
