/* Text4.java -- Whitespace handling tests
   Copyright (C) 2007 Copyright (C) 2005 Audrius Meskauskas
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

package gnu.testlet.javax.swing.text.html.parser.ParserDelegator;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests the "whitespace clash" - two spaces, one before and one after
 * the B tag, must mutate into the single space. Tailored to pass JDK 1.6
 * and 1.7. 
 */
public class Text4 extends Text implements Testlet
{
  public void test(TestHarness a_harness)
  {
    harness = a_harness;
    hideImplied = true;
    
    // Two spaces, one before and one after, must mutate into the single one.
    verify("abc<b>def </b> ghi",
           "<html><head></head><body>61'a 62'b 63'c <b>64'd 65'e 66'f 20 </b>67'g 68'h 69'i </body></html>",
           "whitespace clash 1"
          );
    
    verify("abc <b> def</b>ghi",
           "<html><head></head><body>61'a 62'b 63'c 20 <b>64'd 65'e 66'f </b>67'g 68'h 69'i </body></html>",
           "whitespace clash 1"
          );
  }

}
