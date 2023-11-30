/* Copyright (C) 1999 Hewlett-Packard Company

   This file is part of Mauve.

   Mauve is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Mauve is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Mauve; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.
*/

// Tags: JDK1.0
// Uses: NewNumber

package gnu.testlet.java.lang.Number;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class NumberTest implements Testlet
{
  protected static TestHarness harness;
	public void test_Basics()
	{
	  NewNumber _newnum = new NewNumber();
		NewNumber newnum = new NewNumber(300);
		NewNumber newnum1 = new NewNumber(Integer.MAX_VALUE);
		if ( newnum.byteValue() != (byte)300)
			harness.fail( "Error : test_Basics failed -1 ");
		if ( newnum1.shortValue() != (short)Integer.MAX_VALUE)
			harness.fail( "Error : test_Basics failed -2 ");
	} 
	public void testall()
	{
		test_Basics();
	}

  public void test (TestHarness the_harness)
  {
    harness = the_harness;
    testall ();
  }

}

