/* getRunLimit.java -- some checks for the getRunLimit() methods in the
       AttributedCharacterIterator interface.
   Copyright (C) 2006 David Gilbert <david.gilbert@object-refinery.com>
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

// Tags: JDK1.2

package gnu.testlet.java.text.AttributedCharacterIterator;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Color;
import java.awt.font.TextAttribute;
import java.text.AttributedCharacterIterator;
import java.text.AttributedString;
import java.util.HashSet;
import java.util.Set;

public class getRunLimit implements Testlet
{
  public void test(TestHarness harness)
  {
    test1(harness);
    test2(harness);
    test3(harness);
  }
  
  public void test1(TestHarness harness)
  {
    harness.checkPoint("()");
    AttributedString as = new AttributedString("ABCDEFGHIJ");
    as.addAttribute(TextAttribute.LANGUAGE, "English");
    as.addAttribute(TextAttribute.FOREGROUND, Color.red, 2, 4);
    as.addAttribute(TextAttribute.BACKGROUND, Color.blue, 7, 8);
    AttributedCharacterIterator aci = as.getIterator();
    harness.check(aci.getRunLimit(), 2);
    aci.setIndex(2);
    harness.check(aci.getRunLimit(), 4);
    aci.setIndex(5);
    harness.check(aci.getRunLimit(), 7);
    aci.setIndex(7);
    harness.check(aci.getRunLimit(), 8);
    aci.setIndex(8);
    harness.check(aci.getRunLimit(), 10);
    
    // try an empty string
    as = new AttributedString("");
    aci = as.getIterator();
    harness.check(aci.getRunLimit(), 0);
    
    // try a string with no attributes
    as = new AttributedString("ABC");
    aci = as.getIterator();
    harness.check(aci.getRunLimit(), 3);
  }
  
  public void test2(TestHarness harness)
  {
    harness.checkPoint("(AttributedCharacterIterator.Attribute)");
    AttributedString as = new AttributedString("ABCDEFGHIJ");
    as.addAttribute(TextAttribute.LANGUAGE, "English");
    as.addAttribute(TextAttribute.FOREGROUND, Color.red, 2, 4);
    AttributedCharacterIterator aci = as.getIterator();
    harness.check(aci.getRunLimit(TextAttribute.LANGUAGE), 10);
    harness.check(aci.getRunLimit(TextAttribute.FOREGROUND), 2);
    harness.check(aci.getRunLimit(TextAttribute.BACKGROUND), 10);
    aci.setIndex(2);
    harness.check(aci.getRunLimit(TextAttribute.LANGUAGE), 10);
    harness.check(aci.getRunLimit(TextAttribute.FOREGROUND), 4);
    harness.check(aci.getRunLimit(TextAttribute.BACKGROUND), 10);
    aci.setIndex(4);
    harness.check(aci.getRunLimit(TextAttribute.LANGUAGE), 10);
    harness.check(aci.getRunLimit(TextAttribute.FOREGROUND), 10);
    harness.check(aci.getRunLimit(TextAttribute.BACKGROUND), 10);
    
    // try an empty string
    as = new AttributedString("");
    aci = as.getIterator();
    harness.check(aci.getRunLimit(TextAttribute.FOREGROUND), 0);
    
    // try a string with no attributes
    as = new AttributedString("ABC");
    aci = as.getIterator();
    harness.check(aci.getRunLimit(TextAttribute.FOREGROUND), 3);
  }
  
  public void test3(TestHarness harness)
  {
    harness.checkPoint("(Set)");    
    AttributedString as = new AttributedString("ABCDEFGHIJ");
    as.addAttribute(TextAttribute.LANGUAGE, "English");
    as.addAttribute(TextAttribute.FOREGROUND, Color.red, 2, 4);
    as.addAttribute(TextAttribute.BACKGROUND, Color.yellow, 3, 5);
    AttributedCharacterIterator aci = as.getIterator();
    Set set0 = new HashSet();
    Set set1 = new HashSet();
    set1.add(TextAttribute.LANGUAGE);
    Set set2 = new HashSet();
    set2.add(TextAttribute.FOREGROUND);
    set2.add(TextAttribute.BACKGROUND);
    Set set3 = new HashSet();
    set3.add(TextAttribute.LANGUAGE);
    set3.add(TextAttribute.FOREGROUND);
    set3.add(TextAttribute.BACKGROUND);
    harness.check(aci.getRunLimit(set0), 10);
    harness.check(aci.getRunLimit(set1), 10);
    harness.check(aci.getRunLimit(set2), 2);
    harness.check(aci.getRunLimit(set3), 2);
    aci.setIndex(2);
    harness.check(aci.getRunLimit(set0), 10);
    harness.check(aci.getRunLimit(set1), 10);
    harness.check(aci.getRunLimit(set2), 3);
    harness.check(aci.getRunLimit(set3), 3);
    aci.setIndex(3);
    harness.check(aci.getRunLimit(set0), 10);
    harness.check(aci.getRunLimit(set1), 10);
    harness.check(aci.getRunLimit(set2), 4);
    harness.check(aci.getRunLimit(set3), 4);
    aci.setIndex(4);
    harness.check(aci.getRunLimit(set0), 10);
    harness.check(aci.getRunLimit(set1), 10);
    harness.check(aci.getRunLimit(set2), 5);
    harness.check(aci.getRunLimit(set3), 5);
    aci.setIndex(5);
    harness.check(aci.getRunLimit(set0), 10);
    harness.check(aci.getRunLimit(set1), 10);
    harness.check(aci.getRunLimit(set2), 10);
    harness.check(aci.getRunLimit(set3), 10);
    // try an empty string
    as = new AttributedString("");
    aci = as.getIterator();
    harness.check(aci.getRunLimit(set0), 0);
    harness.check(aci.getRunLimit(set1), 0);
    harness.check(aci.getRunLimit(set2), 0);
    harness.check(aci.getRunLimit(set3), 0);
    
    // try a string with no attributes
    as = new AttributedString("ABC");
    aci = as.getIterator();
    harness.check(aci.getRunLimit(set0), 3);
    harness.check(aci.getRunLimit(set1), 3);
    harness.check(aci.getRunLimit(set2), 3);
    harness.check(aci.getRunLimit(set3), 3);
  }
}
