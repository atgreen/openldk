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

import java.math.BigInteger;

import java.util.Scanner;

/**
 * @author E0327023 Hernadi Laszlo
 * @ 26.02.2007 - 05:28:44
 *
 */
public class Radix extends Base
{

  /* (non-Javadoc)
   * @see gnu.testlet.java.util.Scanner.TestBase#doTest()
   */
  @Override protected void doTest ()
  {
    String testStr = "5F 7FFF 4F3F3F6F 3F3F2EF3FFEE 4FFAAEEFFAA";
    Scanner s = new Scanner (testStr);

    myHarness.check(s.hasNextByte (16), "hasNextByte(16)");
    myHarness.check(s.nextByte(16), 95, "nextByte is 95");
    myHarness.check(s.hasNextShort(16), "hasNextShort(16)");
    myHarness.check(s.nextShort(16), 32767, "nextShort is 32767");
    myHarness.check(s.hasNextInt(16), "hasNextInt(16)");
    myHarness.check(s.nextInt(16), 1329545071, "nextInt is 1329545071");
    myHarness.check(s.hasNextLong(16), "hasNextLong(16)");
    myHarness.check(s.nextLong(16), 69540603232238L, "nextLong is 69540603232238");
    myHarness.check(s.hasNextBigInteger(16), "hasNextBigInteger(16)");
    myHarness.check(s.nextBigInteger(16), BigInteger.valueOf(5496130961322L),
		    "nextBigInteger is 5496130961322");

    s = new Scanner (testStr).useRadix (16);
    myHarness.check(s.radix(), 16, "radix was not set to 16");

    myHarness.check(s.hasNextByte (), "hasNextByte()");
    myHarness.check(s.nextByte(), 95, "nextByte is 95");
    myHarness.check(s.hasNextShort(), "hasNextShort()");
    myHarness.check(s.nextShort(), 32767, "nextShort is 32767");
    myHarness.check(s.hasNextInt(), "hasNextInt()");
    myHarness.check(s.nextInt(), 1329545071, "nextInt is 1329545071");
    myHarness.check(s.hasNextLong(), "hasNextLong()");
    myHarness.check(s.nextLong(), 69540603232238L, "nextLong is 69540603232238");
    myHarness.check(s.hasNextBigInteger(), "hasNextBigInteger()");
    myHarness.check(s.nextBigInteger(), BigInteger.valueOf(5496130961322L),
		    "nextBigInteger is 5496130961322");

  }

}
