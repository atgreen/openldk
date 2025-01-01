// Tags: JDK1.1

// Copyright (C) 2005 David Gilbert (david.gilbert@object-refinery.com)

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

package gnu.testlet.java.awt.FontClass;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Font;

/**
 * Some checks for the decode() method.
 */
public class decode implements Testlet
{
  /**
   * Some checks for the decode() method.
   * 
   * @param harness  the test harness.
   */
  public void test(TestHarness harness)
  {
    // regular usage...
    Font f1 = Font.decode("Dialog-PLAIN-18");
    harness.check(f1.getFamily(), "Dialog");      // check 1
    harness.check(f1.getStyle(), Font.PLAIN);     // check 2
    harness.check(f1.getSize(), 18);              // check 3
  
    // spaces are acceptable too...
    f1 = Font.decode("Dialog BOLD 16");           
    harness.check(f1.getFamily(), "Dialog");      // check 4
    harness.check(f1.getStyle(), Font.BOLD);      // check 5
    harness.check(f1.getSize(), 16);              // check 6

    // unknown name defaults to 'Dialog' 
    f1 = Font.decode("123-PLAIN-18");
    harness.check(f1.getFamily(), "Dialog");      // check 7
    harness.check(f1.getStyle(), Font.PLAIN);     // check 8
    harness.check(f1.getSize(), 18);              // check 9

    // style is not case sensitive
    f1 = Font.decode("Dialog-BoLd-17");
    harness.check(f1.getFamily(), "Dialog");      // check 10
    harness.check(f1.getStyle(), Font.BOLD);      // check 11
    harness.check(f1.getSize(), 17);              // check 12
    
    // unknown style reverts to PLAIN
    f1 = Font.decode("Dialog-NotAStyle-18");
    harness.check(f1.getFamily(), "Dialog");      // check 13
    harness.check(f1.getStyle(), Font.PLAIN);     // check 14
    harness.check(f1.getSize(), 18);              // check 15

    // invalid size defaults to 12
    f1 = Font.decode("Dialog-BOLDITALIC-ZZ");
    harness.check(f1.getFamily(), "Dialog");      // check 16
    harness.check(f1.getStyle(), Font.BOLD + Font.ITALIC);  // check 17
    harness.check(f1.getSize(), 12);              // check 18
    
    f1 = Font.decode("Dialog");
    harness.check(f1.getFamily(), "Dialog");      // check 19
    harness.check(f1.getStyle(), Font.PLAIN);     // check 20
    harness.check(f1.getSize(), 12);              // check 21

    // null should be equivalent to 'Dialog-PLAIN-12'
    f1 = Font.decode(null);
    harness.check(f1.getFamily(), "Dialog");      // check 22
    harness.check(f1.getStyle(), Font.PLAIN);     // check 23
    harness.check(f1.getSize(), 12);              // check 24
    
  }

}