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

import java.util.Scanner;

/**
 * @author E0327023 Hernadi Laszlo
 * @ 24.02.2007 - 20:10:24
 *
 */
public class FindWithinHorizon extends Base
{

  /* (non-Javadoc)
   * @see gnu.testlet.java.util.Scanner.TestBase#doTest()
   */
  @Override protected void doTest ()
  {
    String fishString = "1 fish 2 fish red fish blue fish";
    Scanner s = new Scanner (fishString);
    String tmpStr = "";
    int i;

    for (i = 0; i < fishString.length (); i++)
      {
	// from : http://www.cs.princeton.edu/introcs/15inout/In.java.html
	// (?s) for DOTALL mode so . matches any character, including a line termination character
	// 1 says look only one character ahead
	// consider precompiling the pattern
	tmpStr += s.findWithinHorizon ("(?s).", 1);
      }
    myHarness.check (tmpStr.equals(fishString), "\"" + tmpStr + "\" == \"" + fishString + "\"");
    if (s.hasNext ())
      myHarness.fail ("should not has next...");
  }

}
