// Tags: JDK1.2
// Uses: ../../../../../../gnu/javax/swing/text/html/parser/support/Parser/Parser_Test ../../../../../../gnu/javax/swing/text/html/parser/support/Parser/TestCase

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
import gnu.testlet.gnu.javax.swing.text.html.parser.support.Parser.Parser_Test;
import gnu.testlet.gnu.javax.swing.text.html.parser.support.Parser.TestCase;

import javax.swing.text.MutableAttributeSet;
import javax.swing.text.html.HTML;

/**
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class Text2
  extends TestCase
  implements Testlet
{
  public void test(TestHarness harness)
  {
    h = harness;
    try
      {
        testTextParsing();
      }
    catch (Exception ex)
      {
        ex.printStackTrace();
        harness.fail("Exception: " + ex);
      }
  }

  public void testTextParsing()
                       throws Exception
  {
    Parser_Test v =
      new Parser_Test()
      {
        public void handleSimpleTag(HTML.Tag tag,
                                    MutableAttributeSet attributes, int position
                                   )
        {
          if (!tag.toString().equalsIgnoreCase("#pcdata"))
            out.append("<" + tag + ">");
        }

        public void handleStartTag(HTML.Tag tag,
                                   MutableAttributeSet attributes, int position
                                  )
        {
          out.append("<" + tag + ">");
        }

        public void handleText(char[] chars, int position)
        {
          for (int i = 0; i < chars.length; i++)
            {
              out.append(Integer.toHexString(chars [ i ]));
              if (chars [ i ] > ' ')
                out.append("'" + chars [ i ]);
              out.append(" ");
            }
        }

        public void handleEndTag(HTML.Tag tag, int position)
        {
          out.append("</" + tag + ">");
        }
      };

    v.hideImplied = true;

    // NON - preformatted mode:
    // Everything mutates into spaces, multiple spaces mustates
    // into single one, all whitespace around tags is consumed.
    v.verify("\r \n \t {abc      r\rn\nt}\t \r\n \r \t",
             "<html><head></head><body>7b'{ 61'a 62'b 63'c 20 72'r 20" +
             " 6e'n 20 74't 7d'} </body></html>"
            );

    v.verify("   abba   ",
             "<html><head></head><body>61'a 62'b 62'b 61'a </body></html>"
            );

    v.verify("  \r ab  \t \r \n  ba   ",
             "<html><head></head><body>61'a 62'b 20 62'b 61'a </body></html>"
            );

    // Preformatted mode (in PRE tag):
    // Heading/closing spaces and tabs preserve. ONE  \r, \n or \r\n is removed.
    // /r mutates into \n
    v.verify("<pre>\n\n\n\n   abba   \r\t \r\n</pre>",
             "<html><head></head><body><pre>a a a 20 20 20 61'a 62'b 62'b" +
             " 61'a 20 20 20 a 9 20 </pre></body></html>"
            );

    v.verify("<pre>   abba   </pre>",
             "<html><head></head><body><pre>20 20 20 61'a 62'b 62'b 61'a 20 " +
             "20 20 </pre></body></html>"
            );

    v.verify("<pre>\r\n   abba   </pre>",
             "<html><head></head><body><pre>20 20 20 61'a 62'b 62'b 61'a 20 " +
             "20 20 </pre></body></html>"
            );

    v.verify("<pre>\r\n\r\n   abba   \r\n</pre>",
             "<html><head></head><body><pre>a 20 20 20 61'a 62'b 62'b 61'a 20 20" +
             " 20 </pre></body></html>"
            );

    v.verify("<pre> \r ab  \t \r \n  ba   </pre>",
             "<html><head></head><body><pre>20 a 20 61'a 62'b 20 20 9 20 a" +
             " 20 a 20 20 62'b 61'a 20 20 20 </pre></body></html>"
            );

    v.verify("<pre> \r\n ab  \t \r\n \n  ba   </pre>",
             "<html><head></head><body><pre>20 a 20 61'a 62'b 20 20 9 20 a" +
             " 20 a 20 20 62'b 61'a 20 20 20 </pre></body></html>"
            );

    // In TEXTAREA tag, same.
    v.verify("<textarea>\n\n\n\n   abba \r\n</textarea>",
             "<html><head></head><body><textarea>a a a 20 20 20 61'a " +
             "62'b 62'b 61'a 20 </textarea></body></html>"
            );

    v.verify("<textarea>   abba   </textarea>",
             "<html><head></head><body><textarea>20 20 20 61'a 62'b 62'b 61'a 20 " +
             "20 20 </textarea></body></html>"
            );

    v.verify("<textarea> \r ab  \t \r \n  ba   </textarea>",
             "<html><head></head><body><textarea>20 a 20 61'a 62'b 20 20 9 20 a" +
             " 20 a 20 20 62'b 61'a 20 20 20 </textarea></body></html>"
            );
  }
}
