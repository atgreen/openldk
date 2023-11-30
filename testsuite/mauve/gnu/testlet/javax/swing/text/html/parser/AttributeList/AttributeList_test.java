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

package gnu.testlet.javax.swing.text.html.parser.AttributeList;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.text.html.parser.AttributeList;
import javax.swing.text.html.parser.DTDConstants;

/**
 * @author Audrius Meskauskas (AudriusA@Bluewin.ch)
 */
public class AttributeList_test
  implements Testlet, DTDConstants
{

  public void test(TestHarness a_harness)
  {
    String type;
    for (int i = 0; i < 20; i++)
      {
        type = AttributeList.type2name(i);
        if (type != null)
          a_harness.check(i, AttributeList.name2type(type));
      };

    a_harness.check(AttributeList.type2name(CDATA), "CDATA");
    a_harness.check(AttributeList.name2type("CDATA"), CDATA);

    a_harness.check(AttributeList.type2name(ENTITY), "ENTITY");
    a_harness.check(AttributeList.name2type("ENTITY"), ENTITY);

    a_harness.check(AttributeList.type2name(NAME), "NAME");
    a_harness.check(AttributeList.name2type("NAME"), NAME);
  }
}
