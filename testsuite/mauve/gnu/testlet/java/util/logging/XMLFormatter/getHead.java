// Tags: JDK1.4

// Copyright (C) 2004 Sascha Brawer <brawer@dandelis.ch>

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
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

package gnu.testlet.java.util.logging.XMLFormatter;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.util.logging.Formatter;
import java.util.logging.XMLFormatter;
import java.util.logging.StreamHandler;

/**
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
public class getHead
  implements Testlet
{
  public void test(TestHarness h)
  {
    Formatter formatter;
    StreamHandler handler;

    formatter = new XMLFormatter();
    handler = new StreamHandler();

    // Check point "no encoding set".
    h.checkPoint("no encoding set");
    h.check(formatter.getHead(handler),
            getExpectedHead(System.getProperty("file.encoding")));


    // Check point "UTF-8".
    h.checkPoint("UTF-8");
    try
      {
        handler.setEncoding("UTF-8");
      }
    catch (Exception ex)
      {
        h.check(false);
        h.debug(ex);
      }
    h.check(formatter.getHead(handler), getExpectedHead("UTF-8"));


    /* Check point "getHead(null)".
     *
     * The behavior of passing null is not specified, but we want to
     * check that we do the same as Sun's reference implementation.
     */
    h.checkPoint("getHead(null)");
    try
      {
        formatter.getHead(null);
        h.check(false);
      }
    catch (Exception ex)
      {
        h.check(ex instanceof NullPointerException);
      }
  }


  /**
   * Replaces any occurence of the vertical bar character by the
   * platform-specific line separator.
   *
   * @throws NullPointerException if <code>pat</code> is
   * <code>null</code>.
   */
  public static String replaceLineSeparator(String pat)
  {
    String lineSeparator = System.getProperty("line.separator");

    /* FIXME: This will return the wrong result on platforms where the
     * line.separator property is not exactly one character, for
     * example on Microsoft Windows. Write a decent implementation.
     */
    return pat.replace('|', lineSeparator.charAt(0));
  }


  private String getExpectedHead(String encodingName)
  {
    return replaceLineSeparator("<?xml version=\"1.0\" encoding=\""
      + encodingName
      + "\" standalone=\"no\"?>|<!DOCTYPE log SYSTEM \"logger.dtd\">"
      + "|<log>|");
  }
}
