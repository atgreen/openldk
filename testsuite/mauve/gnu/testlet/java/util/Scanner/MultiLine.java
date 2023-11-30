// Copyright (c) 2007 Hernadi Laszlo

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

// Tags: JDK1.5

/**
 * 
 */
package gnu.testlet.java.util.Scanner;

import java.io.ByteArrayInputStream;

import java.util.Scanner;

/**
 * @author E0327023 Hernadi Laszlo
 * @ 14.02.2007 - 12:17:51
 *
 */
public class MultiLine extends Base
{

  /* (non-Javadoc)
   * @see gnu.testlet.java.util.Scanner.TestBase#doTest()
   */
  @Override protected void doTest ()
  {
    this.myHarness.checkPoint ("Multi line read - linewise");

    String[] lines =
    {
    "1. Line: aaa bbb ccc", "2. line: aaa bbb aaa", "3. line: bbb aaa"};
    String newLine = System.getProperty ("line.separator");
    int i;
    String tmp = lines[0];
    String result;
    byte[]buffer;

    for (i = 1; i < lines.length; i++)
      {
	tmp += newLine + lines[i];
      }
    tmp += newLine;
    buffer = tmp.getBytes ();
    ByteArrayInputStream inStr = new ByteArrayInputStream (buffer);

    Scanner s = new Scanner (inStr);
    for (i = 0; i < lines.length; i++)
      {
	this.myHarness.check (s.hasNextLine (), true,
			      (i + 1) + ". hasNextLine()");
	result = s.nextLine ();
	this.myHarness.check (result, lines[i],
			      (i + 1) + ". nextLine() [" + result + "]");
      }
  }

}
