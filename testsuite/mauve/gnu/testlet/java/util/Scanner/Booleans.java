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
// Tags: JDK1.5
/**
 * 
 */
package gnu.testlet.java.util.Scanner;

import java.util.Random;
import java.util.Scanner;

/**
 * @author E0327023 Hernadi Laszlo
 * @ 15.02.2007 - 13:10:31
 *
 */
public class Booleans extends Base
{
	/**
	 * The amount of booleans to generate and test.
	 */
  private final static int AMOUNT = 100;


  /* (non-Javadoc)
   * @see gnu.testlet.java.util.Scanner.TestBase#doTest()
   */
  @Override protected void doTest ()
  {
    boolean[]values = new boolean[AMOUNT];
    boolean hasNext;
    boolean aktValue;
    String inStr;
    int i;
    Random rand = new Random (System.currentTimeMillis ());

    for (i = 0; i < values.length; i++)
      {
	values[i] = rand.nextBoolean ();
      }
    inStr = "" + values[0];

    for (i = 1; i < values.length; i++)
      {
	inStr += " " + values[i];
      }

    Scanner s = new Scanner (inStr);
    i = 0;
    hasNext = s.hasNextBoolean ();
    while (hasNext)
      {
	aktValue = s.nextBoolean ();
	this.myHarness.check (aktValue, values[i],
			      "nextBoolean() is wrong : " + aktValue +
			      " != " + values[i]);
	i++;
	hasNext = s.hasNextBoolean ();
      }
    this.myHarness.check (i, values.length,
			  "not all values (" + i + " / " + values.length +
			  ")");
  }

}
