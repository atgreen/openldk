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

package gnu.testlet.javax.swing.text.html.parser.ParserDelegator;

import java.util.Random;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests parsing of randomly generated HTML tables, with and without
 * caption, random presence of the table/row closing tags, randomly
 * inserted spaces where witespace should not matter and
 * randomly varying number of rows and columns in the certain interval.
 */
public class randomTables
  extends parsingTester
  implements Testlet
{
  /**
   * Number of randomly generated tables to test.
   */
  static int TABLES_TO_TEST = 101;

  Random r = new Random();

  public String s()
  {
    if (r.nextBoolean())
      return "";

    StringBuffer b = new StringBuffer();
    int spc = r.nextInt(4);
    for (int i = 0; i < spc; i++)
      {
      b.append(' ');
      }
    return b.toString();
  }

  /**
   * Try variable randomly generated table.
   */
  public void test(TestHarness a_harness)
  {
    harness = a_harness;

    hideImplied = true;
    for (int i = 0; i < TABLES_TO_TEST; i++)
      {
        try {
          new table().test();
        }
        catch (Exception ex) {
          harness.fail(ex.toString());
        }
      }
  }

  class table
  {
    final String [][] rows;
    final boolean caption = r.nextBoolean();

    table()
    {
    int nrows = r.nextInt(5) + 1;
    rows = new String[ nrows ][];
    for (int i = 0; i < rows.length; i++)
      {
      int ncol = r.nextInt(5) + 1;
      rows [ i ] = new String[ ncol ];
      for (int j = 0; j < rows [ i ].length; j++)
        {
        rows [ i ] [ j ] = "C_" + i + "_" + j;
        }
      }
    }

    public String getHtml()
    {
      StringBuffer b = new StringBuffer("<html><head></head><body><table>");
      if (caption) b.append("<caption>capt</caption>");

      for (int row = 0; row < rows.length; row++)
        {
        b.append("<tr" + s() + ">");
        for (int col = 0; col < rows [ row ].length; col++)
          {
          b.append("<td" + s() + ">");
          b.append( rows [ row ] [ col ] );
          if (r.nextBoolean())
            b.append("</td" + s() + ">");
          }
        if (r.nextBoolean())
          b.append("</tr" + s() + ">");
        }
      b.append("</table></body></html>");
      return b.toString();
    }

    public String getTrace()
    {
      StringBuffer b =
        new StringBuffer("<html><head></head><body><table>");
      if (caption) b.append("<caption>'capt'</caption>");
      for (int row = 0; row < rows.length; row++)
        {
        b.append("<tr>");
        for (int col = 0; col < rows [ row ].length; col++)
          {
          b.append("<td>'" + rows [ row ] [ col ] + "'</td>");
          }
        b.append("</tr>");
        }
      b.append("</table></body></html>");
      return b.toString();
    }

    void test() throws Exception
    {
      String trace = getTrace();
      String html = getHtml();
      verify(html, trace, html+":"+trace);
    }
  }

}
