// Tags: JDK1.2 GNU

// Copyright (C) 2005, 2006 Audrius Meskauskas <audriusa@bluewin.ch>

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

package gnu.testlet.gnu.javax.swing.text.html.parser.support.low.Buffer;

import gnu.javax.swing.text.html.parser.support.low.Buffer;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.gnu.javax.swing.text.html.parser.support.Parser.TestCase;

/**
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class Buffer_Test
  extends TestCase
  implements Testlet
{
  public void test(TestHarness harness)
  {
    h = harness;
    testAppend();
    testDelete();
  }

  public void testAppend()
  {
    Buffer.INITIAL_SIZE = 2;

    Buffer b = new Buffer("01");
    b.append('A', 0);
    b.append('B', 0);
    assertEquals(b.toString(), "01AB");
  }

  public void testDelete()
  {
    Buffer b = new Buffer("0123456789ABCDEFGHIJKLMN");
    b.delete(2, 7);
    assertEquals(b.toString(), "01789ABCDEFGHIJKLMN");
  }
}
