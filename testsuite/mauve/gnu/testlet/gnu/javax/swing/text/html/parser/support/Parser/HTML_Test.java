// Tags: JDK1.2 GNU
// Uses: TestCase

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


package gnu.testlet.gnu.javax.swing.text.html.parser.support.Parser;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.html.HTML;

public class HTML_Test
  extends TestCase
  implements Testlet
{
  public void test(TestHarness harness)
  {
    h = harness;
    testCaseSensitivity();
    testConstructor();
    testGetAttributeKey();
    testGetIntegerAttributeValue();
    testGetTag();
  }

  /**
   * By the language definition, HTML tags and attributes are case
   * insensitive. Hence if it is not clearly specified, in which case
   * the tag name must be, it should be expected to come as in
   * lowercase, as in uppercase. This should be true for HTML.getTag(String)
   * and for HTML.getAttributeKey(String).
   *
   * In some implementations these two functions may be case sensitive.
   * As this requirement is not mentioned in the documentation,
   * and also it is not documented, in which case the name must be supplied,
   * this will be reported as an error in this test.
   * The GNU CLASSPATH implementation is case insensitive.
   */
  public void testCaseSensitivity()
  {
    String def = "case sensitivity";
    assertEquals("html=Html", HTML.getTag("html"), HTML.getTag("HtmL"));
    assertEquals("html=HTML", HTML.getTag("html"), HTML.getTag("HTML"));
    assertEquals("size=SIZE", HTML.getAttributeKey("size"),
                 HTML.getAttributeKey("SIZE")
                );
    assertEquals("size=SizE", HTML.getAttributeKey("size"),
                 HTML.getAttributeKey("SizE")
                );
  }

  public void testConstructor()
  {
    new HTML();
  }

  public void testGetAttributeKey()
  {
    // Test the known tags.
    String[] mine = toStrings(HTML.getAllAttributeKeys());

    for (int i = 0; i < mine.length; i++)
      assertNotNull(mine [ i ], HTML.getAttributeKey(mine [ i ]));

    // Test the unknown tag.
    assertNull("surely unknown", HTML.getTag("audrius"));
  }

  public void testGetIntegerAttributeValue()
  {
    SimpleAttributeSet ase = new SimpleAttributeSet();
    ase.addAttribute(HTML.getAttributeKey("size"), "222");
    assertEquals(222,
                 HTML.getIntegerAttributeValue(ase,
                                               HTML.getAttributeKey("size"), 333
                                              )
                );

    assertEquals(333,
                 HTML.getIntegerAttributeValue(ase,
                                               HTML.getAttributeKey("href"), 333
                                              )
                );
  }

  public void testGetTag()
  {
    // known tags:
    String[] mine = toStrings(HTML.getAllTags());

    for (int i = 0; i < mine.length; i++)
      assertNotNull(mine [ i ], HTML.getTag(mine [ i ]));

    // unknown tag
    assertNull("surely unknown", HTML.getTag("audrius"));
  }

  private String[] toStrings(Object[] objs)
  {
    String[] a = new String[ objs.length ];

    for (int i = 0; i < a.length; i++)
      a [ i ] = objs [ i ].toString();

    return a;
  }
}
