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

package gnu.testlet.java.text.AttributedCharacterIterator;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Color;
import java.awt.font.TextAttribute;
import java.text.AttributedCharacterIterator;
import java.text.AttributedString;
import java.util.Set;

/**
 * Some checks for the getRunStart() methods in the 
 * {@link AttributedCharacterIterator} interface.  
 */
public class getRunStart implements Testlet
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

  private void test1(TestHarness harness) 
  {
    harness.checkPoint("getRunStart();");
    AttributedString as = new AttributedString("ABCDEFG");
    as.addAttribute(TextAttribute.LANGUAGE, "English");
    as.addAttribute(TextAttribute.FOREGROUND, Color.red, 2, 4);
    AttributedCharacterIterator aci = as.getIterator();
    harness.check(aci.getRunStart(), 0);
    aci.setIndex(3);
    harness.check(aci.getRunStart(), 2);
  }

  private void test2(TestHarness harness) 
  {
    harness.checkPoint("getRunStart(AttributedCharacterIterator.Attribute);");
    AttributedString as = new AttributedString("ABCDEFG");
    as.addAttribute(TextAttribute.LANGUAGE, "English");
    as.addAttribute(TextAttribute.FOREGROUND, Color.red, 2, 4);
    AttributedCharacterIterator aci = as.getIterator();
    harness.check(aci.getRunStart(TextAttribute.LANGUAGE), 0);
    aci.setIndex(3);
    harness.check(aci.getRunStart(TextAttribute.FOREGROUND), 2);
  }

  private void test3(TestHarness harness) 
  {
    harness.checkPoint("getRunStart(Set);");
    AttributedString as = new AttributedString("ABCDEFG");
    as.addAttribute(TextAttribute.LANGUAGE, "English");
    AttributedCharacterIterator aci = as.getIterator();
    
    // try null set
    harness.check(aci.getRunStart((Set) null), 0);
    
    AttributedCharacterIterator aci2 = as.getIterator(null, 4, 7);
    harness.check(aci2.getRunStart((Set) null), 4);
  }

}
