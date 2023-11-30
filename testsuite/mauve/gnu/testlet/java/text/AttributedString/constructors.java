// Tags: JDK1.2

// Copyright (C) 2005, 2006, David Gilbert <david.gilbert@object-refinery.com>

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

import java.awt.Color;
import java.awt.font.TextAttribute;
import java.text.AttributedCharacterIterator;
import java.text.AttributedString;
import java.util.HashMap;

/**
 * Some checks for the constructors in the {@link AttributedString} class.  
 */
public class constructors implements Testlet
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not allowed).
   */
  public void test(TestHarness harness) 
  {
    testConstructor1(harness);
    testConstructor2(harness);
    testConstructor3(harness);
    testConstructor4(harness);
    testConstructor5(harness);
  }

  private void testConstructor1(TestHarness harness) 
  {
    harness.checkPoint("AttributedString(AttributedCharacterIterator);");
    
    // it isn't specified, but we assume a NullPointerException if the iterator
    // is null
    boolean pass = false;
    try
    {
      /* AttributedString as = */ 
          new AttributedString((AttributedCharacterIterator) null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  }

  private void testConstructor2(TestHarness harness) 
  {
    harness.checkPoint("AttributedString(AttributedCharacterIterator, int, int);");
    AttributedString source = new AttributedString("ABCDEFGHIJ");
    AttributedCharacterIterator sourceACI = source.getIterator();
    
    // should get an IllegalArgumentException if the start index is outside the
    // valid range
    boolean pass = false;
    try
    {
      /*AttributedString as =*/ new AttributedString(sourceACI, -1, 2);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);

    // should get an IllegalArgumentException if the end index is outside the
    // valid range
    pass = false;
    try
    {
      /*AttributedString as =*/ new AttributedString(sourceACI, 2, 12);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // it isn't specified, but we assume a NullPointerException if the iterator
    // is null
    pass = false;
    try
    {
      /* AttributedString as = */ 
          new AttributedString((AttributedCharacterIterator) null, 1, 5);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  }

  private void testConstructor3(TestHarness harness) 
  {
    harness.checkPoint("AttributedString(AttributedCharacterIterator, int, int," +
                "AttributedCharacterIterator.Attribute[]);");
    
    AttributedString as0 = new AttributedString("ABCDEFGHIJ");
    as0.addAttribute(TextAttribute.LANGUAGE, "English");
    as0.addAttribute(TextAttribute.FOREGROUND, Color.red, 2, 4);
    as0.addAttribute(TextAttribute.BACKGROUND, Color.yellow, 3, 5);
    
    // try extracting no attributes...
    AttributedString as = new AttributedString(as0.getIterator(), 1, 8, 
            new AttributedCharacterIterator.Attribute[] {});
    AttributedCharacterIterator aci = as.getIterator();
    harness.check(aci.getRunLimit(TextAttribute.LANGUAGE), 7);
    harness.check(aci.getRunLimit(TextAttribute.FOREGROUND), 7);
    harness.check(aci.getRunLimit(TextAttribute.BACKGROUND), 7);
    
    // try extracting just one attribute...
    as = new AttributedString(as0.getIterator(), 1, 8, 
            new AttributedCharacterIterator.Attribute[] {TextAttribute.FOREGROUND});
    aci = as.getIterator();
    harness.check(aci.getRunLimit(TextAttribute.LANGUAGE), 7);
    harness.check(aci.getRunLimit(TextAttribute.FOREGROUND), 1);
    aci.setIndex(1);
    harness.check(aci.getRunLimit(TextAttribute.LANGUAGE), 7);
    harness.check(aci.getRunLimit(TextAttribute.FOREGROUND), 3);
    aci.setIndex(3);
    harness.check(aci.getRunLimit(TextAttribute.LANGUAGE), 7);
    harness.check(aci.getRunLimit(TextAttribute.FOREGROUND), 7);
    
    // null iterator should throw NullPointerException
    boolean pass = false;
    try
    {
      /*AttributedString as =*/ new AttributedString(null, 0, 3, 
            new AttributedCharacterIterator.Attribute[] {TextAttribute.FONT});
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // try start > string length
    AttributedString as1 = new AttributedString("ABC");
    
    pass = false;
    try
    {
      /*AttributedString as =*/ new AttributedString(as1.getIterator(), 3, 4, 
            new AttributedCharacterIterator.Attribute[] {TextAttribute.FONT});
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);

    // try end > string length
    pass = false;
    try
    {
      /*AttributedString as =*/ new AttributedString(as1.getIterator(), 1, 4, 
            new AttributedCharacterIterator.Attribute[] {TextAttribute.FONT});
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // try start > end
    pass = false;
    try
    {
      /*AttributedString as =*/ new AttributedString(as1.getIterator(), 1, 0, 
            new AttributedCharacterIterator.Attribute[] {TextAttribute.FONT});
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
  
  }

  private void testConstructor4(TestHarness harness) 
  {
    harness.checkPoint("AttributedString(String);");
    
    // try null argument - the API spec doesn't say what happens.
    boolean pass = false;
    try
    {
      /* AttributedString as = */ new AttributedString((String) null);   
    }
    catch (NullPointerException e) 
    {
      pass = true;   
    }
    harness.check(pass);
  }

  private void testConstructor5(TestHarness harness) 
  {
    harness.checkPoint("AttributedString(String, map);");
    HashMap map = new HashMap();
    map.put(AttributedCharacterIterator.Attribute.LANGUAGE, "English");    
    AttributedString as = new AttributedString("ABC", map);
    AttributedCharacterIterator aci = as.getIterator();
    harness.check(aci.first() == 'A');
    harness.check(aci.getAttribute(AttributedCharacterIterator.Attribute.LANGUAGE).equals("English"));
    harness.check(aci.getRunLimit() == 3);
    harness.check(aci.getRunStart() == 0);
    
    // test null string - not specified, assuming NullPointerException
    boolean pass = false;
    try 
    {
      /* AttributedString as = */ new AttributedString(null, new HashMap());   
    }
    catch (NullPointerException e) 
    {
      pass = true;   
    }
    harness.check(pass);
    
    // test null map - not specified, assuming NullPointerException.
    pass = false;
    try
    {
      /* AttributedString as = */ new AttributedString("ABC", null);
    }
    catch (NullPointerException e)
    {
      pass = true;   
    }
    harness.check(true);
    
    // test empty string with non-empty map
    pass = false;
    try
    {
      /* AttributedString as = */ new AttributedString("", map);   
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    
  }

}
