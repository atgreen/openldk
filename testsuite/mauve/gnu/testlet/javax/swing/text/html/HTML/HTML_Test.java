// Tags: JDK1.2

// Copyright (C) 2005 Audrius Meskauskas (AudriusA@Bioinformatics.org)

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

package gnu.testlet.javax.swing.text.html.HTML;

import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.html.HTML;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;


/**
 * Test the javax.swing.text.html.HTML public methods.
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class HTML_Test
  implements Testlet
{
  TestHarness harness;

  public void test(TestHarness a_harness)
  {
    harness = a_harness;
    testConstructor();
    testGetAttributeKey();
    testGetTag();
    testGetIntegerAttributeValue();
  }

  void assertNotNull(String about, Object notNull)
  {
    if (notNull == null)
      harness.fail(about);
  }


  void testConstructor()
  {
    harness.checkPoint("constructor");
    new HTML();
  }

  void testGetAttributeKey()
  {
    harness.checkPoint("getAttributeKey");

    // Test the known tags.
    String mine[] = toStrings(HTML.getAllAttributeKeys());

    for (int i = 0; i < mine.length; i++)
      assertNotNull(HTML.getAttributeKey(mine [ i ]) + " must be found",
                    mine [ i ]
                   );

    // Test the unknown attribute.
    harness.check(HTML.getTag("audrius"), null, "surely unknown HTML attribute");
  }

  void testGetIntegerAttributeValue()
  {
    harness.checkPoint("getIntegerAttributeValue");

    SimpleAttributeSet ase = new SimpleAttributeSet();
    ase.addAttribute(HTML.getAttributeKey("size"), "222");
    harness.check(222,
                  HTML.getIntegerAttributeValue(ase,
                                                HTML.getAttributeKey("size"),
                                                333
                                               ), "attribute must be found"
                 );

    harness.check(333,
                  HTML.getIntegerAttributeValue(ase,
                                                HTML.getAttributeKey("href"),
                                                333
                                               ),
                  "the default value must be returned"
                 );
  }

  void testGetTag()
  {
    harness.checkPoint("getTag");

    /* Testing the known tags: */
    String mine[] = toStrings(HTML.getAllTags());

    for (int i = 0; i < mine.length; i++)
      assertNotNull(mine [ i ]+" tag must be found", HTML.getTag(mine [ i ]));

    /* Testing the unknown tag: */
    harness.check(HTML.getTag("audrius"), null, "surely unknown  HTML tag");
  }

  private String[] toStrings(Object objs[])
  {
    String a[] = new String[ objs.length ];

    for (int i = 0; i < a.length; i++)
      a [ i ] = objs [ i ].toString();

    return a;
  }
}
