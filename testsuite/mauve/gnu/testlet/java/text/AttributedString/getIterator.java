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
import java.text.CharacterIterator;
import java.util.Locale;

/**
* Some checks for the getIterator method in the {@link AttributedString} 
* class.  
*/
public class getIterator implements Testlet
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
    test3(harness);
  }
  
  public void test1(TestHarness harness) 
  {
    harness.checkPoint("getIterator()"); 
    AttributedString as = new AttributedString("ABC");
    AttributedCharacterIterator aci = as.getIterator();
    harness.check(aci.current() == 'A');
    harness.check(aci.next() == 'B');
    harness.check(aci.next() == 'C');
    harness.check(aci.next() == CharacterIterator.DONE);

    AttributedString as2 = new AttributedString("");
    AttributedCharacterIterator aci2 = as2.getIterator();
    harness.check(aci2.current() == CharacterIterator.DONE);
  }

  public void test2(TestHarness harness) 
  {
    harness.checkPoint("getIterator(AttributedCharacterIterator.Attribute[])");
    AttributedString as = new AttributedString("ABCDEF");
    as.addAttribute(AttributedCharacterIterator.Attribute.LANGUAGE, Locale.FRENCH);
    as.addAttribute(TextAttribute.BACKGROUND, Color.red, 0, 3);
    as.addAttribute(TextAttribute.FOREGROUND, Color.blue, 2, 4);
    AttributedCharacterIterator.Attribute[] attributes = new AttributedCharacterIterator.Attribute[2];
    attributes[0] = TextAttribute.BACKGROUND;
    attributes[1] = TextAttribute.FOREGROUND;
    AttributedCharacterIterator aci = as.getIterator(attributes);
    harness.check(aci.getAttribute(TextAttribute.BACKGROUND), Color.red);
    harness.check(aci.getAttribute(TextAttribute.FOREGROUND), null);
    harness.check(aci.next() == 'B');
    harness.check(aci.getAttribute(TextAttribute.BACKGROUND), Color.red);
    harness.check(aci.getAttribute(TextAttribute.FOREGROUND), null);
    harness.check(aci.next() == 'C');
    harness.check(aci.getAttribute(TextAttribute.BACKGROUND), Color.red);
    harness.check(aci.getAttribute(TextAttribute.FOREGROUND), Color.blue);
    harness.check(aci.next() == 'D');
    harness.check(aci.getAttribute(TextAttribute.BACKGROUND), null);
    harness.check(aci.getAttribute(TextAttribute.FOREGROUND), Color.blue);
    harness.check(aci.next() == 'E');
    harness.check(aci.getAttribute(TextAttribute.BACKGROUND), null);
    harness.check(aci.getAttribute(TextAttribute.FOREGROUND), null);
    harness.check(aci.next() == 'F');
    harness.check(aci.getAttribute(TextAttribute.BACKGROUND), null);
    harness.check(aci.getAttribute(TextAttribute.FOREGROUND), null);
    
    // a null argument is equivalent to a regular iterator
    AttributedString as2 = new AttributedString("ABC");
    AttributedCharacterIterator aci2 = as2.getIterator(null);
    harness.check(aci2.current() == 'A');
    harness.check(aci2.next() == 'B');
    harness.check(aci2.next() == 'C');
    harness.check(aci2.next() == CharacterIterator.DONE);

    AttributedString as3 = new AttributedString("");
    AttributedCharacterIterator aci3 = as3.getIterator(null);
    harness.check(aci3.current() == CharacterIterator.DONE);
  } 
 
  public void test3(TestHarness harness) 
  {
    harness.checkPoint("getIterator(AttributedCharacterIterator.Attribute[], int, int)"); 
    
    // if beginIndex < 0, there should be an IllegalArgumentException
    boolean pass = false;
    try
    {
      AttributedString as = new AttributedString("ABC");
      as.getIterator(null, -1, 1);
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
      AttributedString as = new AttributedString("XYZ");
      as.getIterator(null, 2, 4);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(true);
    
    // if start index > end index, there should be an IllegalArgumentException
    pass = false;
    try
    {
      AttributedString as = new AttributedString("123");
      as.getIterator(null, 2, 1);    
    }
    catch (IllegalArgumentException e)
    {
      pass = true;  
    }
    harness.check(pass);
  } 

}
