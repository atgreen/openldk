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

import java.util.Random;
import java.util.Scanner;

/**
 * @author E0327023 Hernadi Laszlo
 * @ 15.02.2007 - 21:06:41
 *
 */
public class DoubleFloat extends Base
{
	/**
	 * The amount of doubles and floats to generate and test.
	 */
  private final static int AMOUNT = 10000;


  public DoubleFloat ()
  {
    this.isEnabled = false;
  }

  /* (non-Javadoc)
   * @see gnu.testlet.java.util.Scanner.TestBase#doTest()
   */
  @Override protected void doTest ()
  {
    double[] doubleZ = new double[AMOUNT];
    float[] floatZ = new float[doubleZ.length];
    long runID = System.currentTimeMillis ();
    Random rand = new Random (runID);
    int i;
    StringBuffer tmpStr = new StringBuffer (1000);
    String inStr;

    double aktDouble;
    float aktFloat;

    for (i = 0; i < doubleZ.length; i++)
      {
	doubleZ[i] = rand.nextDouble () - 0.5d;
	floatZ[i] = rand.nextFloat () - 0.5f;
      }

    tmpStr.append (doubleZ[0] + " " + floatZ[0]);
    for (i = 1; i < doubleZ.length; i++)
      {
	tmpStr.append (" " + doubleZ[i] + " " + floatZ[i]);
      }

    inStr = tmpStr.toString ();
    Scanner s = new Scanner (inStr);
    //s.setUseLocale (false);
//              Scanner s = new Scanner(inStr);

    i = 0;
    while (s.hasNext () && i < doubleZ.length)
      {
	if (s.hasNextDouble ())
	  {
	    aktDouble = s.nextDouble ();
	    this.myHarness.check (aktDouble, doubleZ[i],
				  "#" + i + " : bad nextDouble() (" +
				  aktDouble + " != " + doubleZ[i]);
	  }
	else
	  {
	    this.myHarness.fail ("#" + i + " : not double... (" + s.next () +
				 ")");
	  }
	if (s.hasNextFloat ())
	  {
	    aktFloat = s.nextFloat ();
	    this.myHarness.check (aktFloat, floatZ[i],
				  "#" + i + " : bad nextFloat() (" +
				  aktFloat + " != " + floatZ[i]);
	  }
	else
	  {
	    this.myHarness.fail ("#" + i + " : not float... (" + s.next () +
				 ")");
	  }
	i++;
      }

    if (i != doubleZ.length)
      {
	this.myHarness.fail ("not all (" + i + " / " + doubleZ.length);
      }

  }

}
