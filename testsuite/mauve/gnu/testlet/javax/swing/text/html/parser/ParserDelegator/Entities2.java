// Tags: JDK1.2
// Uses: ../../../../../../gnu/javax/swing/text/html/parser/support/Parser/TestCase

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


package gnu.testlet.javax.swing.text.html.parser.ParserDelegator;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.gnu.javax.swing.text.html.parser.support.Parser.TestCase;

import javax.swing.text.html.parser.DTDConstants;
import javax.swing.text.html.parser.Entity;

/**
 * Entity test, with Sun passes since 1.5.0 
 * 
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class Entities2
  extends TestCase
  implements Testlet
{
  public void test(TestHarness harness)
  {
    h = harness;
    testPublicSystemGeneralParameter();
    testName2type();
  }

  public void testName2type()
  {
    assertEquals("PUBLIC", Entity.name2type("PUBLIC"), DTDConstants.PUBLIC);
    assertEquals("SDATA", Entity.name2type("SDATA"), DTDConstants.SDATA);
    assertEquals("PI", Entity.name2type("PI"), DTDConstants.PI);
    assertEquals("STARTTAG", Entity.name2type("STARTTAG"), DTDConstants.STARTTAG);
    assertEquals("ENDTAG", Entity.name2type("ENDTAG"), DTDConstants.ENDTAG);
    assertEquals("MS", Entity.name2type("MS"), DTDConstants.MS);
    assertEquals("MD", Entity.name2type("MD"), DTDConstants.MD);
    assertEquals("SYSTEM", Entity.name2type("SYSTEM"), DTDConstants.SYSTEM);

    assertEquals("surely unknown ", Entity.name2type("audrius"),
                 DTDConstants.CDATA
                );
  }

  public void testPublicSystemGeneralParameter()
  {
    int[] pu_sy = new int[] { DTDConstants.PUBLIC, DTDConstants.SYSTEM, 0 };

    int[] gen_par =
      new int[] { DTDConstants.GENERAL, DTDConstants.PARAMETER, 0 };

    for (int ps = 0; ps < pu_sy.length; ps++)
      {
        for (int gp = 0; gp < gen_par.length; gp++)
          {
            Entity e = new Entity(null, 0, null);
            e.type = pu_sy [ ps ] | gen_par [ gp ];

            assertEquals(e.isGeneral(), gen_par [ gp ] == DTDConstants.GENERAL);
            assertEquals(e.isParameter(),
                         gen_par [ gp ] == DTDConstants.PARAMETER
                        );

            assertEquals((e.type & DTDConstants.SYSTEM) != 0,
                         pu_sy [ ps ] == DTDConstants.SYSTEM
                        );

            assertEquals((e.type & DTDConstants.PUBLIC) != 0,
                         pu_sy [ ps ] == DTDConstants.PUBLIC
                        );

            assertEquals((e.type & DTDConstants.GENERAL) != 0,
                         gen_par [ gp ] == DTDConstants.GENERAL
                        );

            assertEquals((e.type & DTDConstants.PARAMETER) != 0,
                         gen_par [ gp ] == DTDConstants.PARAMETER
                        );
          }
      }
  }

  protected void setUp()
                throws Exception
  {
    super.setUp();
  }

  protected void tearDown()
                   throws Exception
  {
    super.tearDown();
  }
}
