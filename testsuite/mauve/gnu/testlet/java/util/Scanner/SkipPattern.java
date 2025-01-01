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

package gnu.testlet.java.util.Scanner;

import java.util.Scanner;

/**
 * @author E0327023 Hernadi Laszlo
 * @ 19.02.2007 - 03:16:19
 *
 */
public class SkipPattern extends Base
{

  private static final String FISH_STR = "1 fish 2 fish red fish blue fish ";	//$NON-NLS-1$

  /* (non-Javadoc)
   * @see gnu.testlet.java.util.Scanner.TestBase#doTest()
   */
  @Override protected void doTest ()
  {
    Scanner s = new Scanner(FISH_STR);
    String[] values = { "1", "2", "red", "blue"};
    int i;
    String tmpStr;

    for (i = 0; i < values.length; i++)
      {
	tmpStr = s.next ();
	myHarness.check(tmpStr.equals (values[i]), tmpStr + " = " + values[i]);
	s = s.skip (" ").skip ("fish");
      }

  }

}
