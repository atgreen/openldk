// Tags: JDK1.2

// Copyright (C) 2005 Audrius Meskauskas, AudriusA@Bluewin.ch

// // This file is part of Mauve.

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


package gnu.testlet.javax.swing.text.html.parser.Entity;

import javax.swing.text.html.parser.Element;
import javax.swing.text.html.parser.Entity;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.text.html.parser.DTDConstants;

/**
 * @author Audrius Meskauskas (AudriusA@Bluewin.ch)
 */
public class Entity_Test
  implements Testlet
{
  private Element element;
  TestHarness harness;

  public Entity_Test()
  {
  }

  public void test(TestHarness a_harness)
  {
    harness = a_harness;
    try
      {
      testName2type();
      testPublicSystemGeneralParameter();
      element = null;
      }
    catch (Throwable exc)
      {
      exc.printStackTrace();
      if (exc != null)
        harness.fail(exc.getClass().getName() + ":" + exc.getMessage());
      else
        harness.fail("exception");
      }
  }

  public void testName2type()
  {
    harness.check(Entity.name2type("PUBLIC"), DTDConstants.PUBLIC, "PUBLIC");
    harness.check(Entity.name2type("SDATA"), DTDConstants.SDATA, "SDATA");
    harness.check(Entity.name2type("PI"), DTDConstants.PI, "PI");
    harness.check(Entity.name2type("STARTTAG"), DTDConstants.STARTTAG,
                  "STARTTAG"
                 );
    harness.check(Entity.name2type("ENDTAG"), DTDConstants.ENDTAG, "ENDTAG");
    harness.check(Entity.name2type("MS"), DTDConstants.MS, "MS");
    harness.check(Entity.name2type("MD"), DTDConstants.MD, "MD");
    harness.check(Entity.name2type("SYSTEM"), DTDConstants.SYSTEM, "SYSTEM");

    harness.check(Entity.name2type("audrius"), DTDConstants.CDATA,
                  "surely unknown "
                 );
  }

  public void testPublicSystemGeneralParameter()
  {
    int pu_sy[] = new int[] { DTDConstants.PUBLIC, DTDConstants.SYSTEM, 0 };

    int gen_par[] =
      new int[] { DTDConstants.GENERAL, DTDConstants.PARAMETER, 0 };

    for (int ps = 0; ps < pu_sy.length; ps++)
      {
      for (int gp = 0; gp < gen_par.length; gp++)
        {
        Entity e = new Entity(null, 0, null);
        e.type = pu_sy [ ps ] | gen_par [ gp ];

        harness.check(e.isGeneral(), gen_par [ gp ] == DTDConstants.GENERAL);
        harness.check(e.isParameter(), gen_par [ gp ] == DTDConstants.PARAMETER);

        harness.check((e.type & DTDConstants.SYSTEM) != 0,
                      pu_sy [ ps ] == DTDConstants.SYSTEM
                     );

        harness.check((e.type & DTDConstants.PUBLIC) != 0,
                      pu_sy [ ps ] == DTDConstants.PUBLIC
                     );

        harness.check((e.type & DTDConstants.GENERAL) != 0,
                      gen_par [ gp ] == DTDConstants.GENERAL
                     );

        harness.check((e.type & DTDConstants.PARAMETER) != 0,
                      gen_par [ gp ] == DTDConstants.PARAMETER
                     );
        }
      }
  }
}
