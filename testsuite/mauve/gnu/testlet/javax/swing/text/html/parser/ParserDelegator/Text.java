// Tags: JDK1.2
// Uses: parsingTester

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

package gnu.testlet.javax.swing.text.html.parser.ParserDelegator;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.text.MutableAttributeSet;
import javax.swing.text.html.HTML;

/**
 * Verifies if the \r, \n , \r\n and \t are handled correctly in normal
 * and pre-formatted text sections.
 * @author Audrius Meskauskas (AudriusA@Bluewin.ch)
 */
public class Text
  extends parsingTester
  implements Testlet
{
  public void handleSimpleTag(HTML.Tag tag, MutableAttributeSet attributes,
                              int position
                             )
  {
    out.append("<" + tag + ">");
  }

  public void handleStartTag(HTML.Tag tag, MutableAttributeSet attributes,
                             int position
                            )
  {
    out.append("<" + tag + ">");
  }

  public void handleText(char chars[], int position)
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

  public void test(TestHarness a_harness)
  {
    harness = a_harness;
    hideImplied = true;

    /*
    Non-preformatted text.
    \t, \r and \n mutate into spaces, then multiple spaces mutate
    into single one, all whitespace around tags is consumed.
    */
    verify("\r \n \t {abc      r\rn\nt}\t \r\n \r \t",
           "<html><head></head><body>7b'{ 61'a 62'b 63'c 20 72'r 20" +
           " 6e'n 20 74't 7d'} </body></html>", "normal"
          );

    verify("   abba   ",
           "<html><head></head><body>61'a 62'b 62'b 61'a </body></html>",
           "single word"
          );

    verify("  \r ab  \t \r \n  ba   ",
           "<html><head></head><body>61'a 62'b 20 62'b 61'a </body></html>",
           "line breaks"
          );

    /*
     Pre-formatted text.
     Heading/closing spaces and tabs preserved.
     ONE  bounding \r, \n or \r\n is removed.
     \r or \r\n mutate into \n. Tabs are
     preserved.
    */
    verify("<pre>\n\n\n\n   abba   \r\t \r\n</pre>",
           "<html><head></head><body><pre>a a a 20 20 20 61'a 62'b 62'b" +
           " 61'a 20 20 20 a 9 20 </pre></body></html>", "pre"
          );

    verify("<pre>   abba   </pre>",
           "<html><head></head><body><pre>20 20 20 61'a 62'b 62'b 61'a 20 " +
           "20 20 </pre></body></html>", "pre"
          );

    verify("<pre>\r\n   abba   </pre>",
           "<html><head></head><body><pre>20 20 20 61'a 62'b 62'b 61'a 20 " +
           "20 20 </pre></body></html>", "pre, single word"
          );

    verify("<pre>\r\n\r\n   abba   \r\n</pre>",
           "<html><head></head><body><pre>a 20 20 20 61'a 62'b 62'b 61'a 20 20" +
           " 20 </pre></body></html>",
           "pre, single word with line breaks around"
          );

    verify("<pre> \r ab  \t \r \n  ba   </pre>",
           "<html><head></head><body><pre>20 a 20 61'a 62'b 20 20 9 20 a" +
           " 20 a 20 20 62'b 61'a 20 20 20 </pre></body></html>",
           "pre, line breaks"
          );

    verify("<pre> \r\n ab  \t \r\n \n  ba   </pre>",
           "<html><head></head><body><pre>20 a 20 61'a 62'b 20 20 9 20 a" +
           " 20 a 20 20 62'b 61'a 20 20 20 </pre></body></html>", "pre"
          );

    // In TEXTAREA tag, same.
    verify("<textarea>\n\n\n\n   abba \r\n</textarea>",
           "<html><head></head><body><textarea>a a a 20 20 20 61'a " +
           "62'b 62'b 61'a 20 </textarea></body></html>",
           "textarea, single word with line breaks around"
          );

    verify("<textarea>   abba   </textarea>",
           "<html><head></head><body><textarea>20 20 20 61'a 62'b 62'b 61'a 20 " +
           "20 20 </textarea></body></html>", "textarea, singe word"
          );

    verify("<textarea> \r ab  \t \r \n  ba   </textarea>",
           "<html><head></head><body><textarea>20 a 20 61'a 62'b 20 20 9 20 a" +
           " 20 a 20 20 62'b 61'a 20 20 20 </textarea></body></html>",
           " textarea"
          );
  }
}
