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

package gnu.testlet.java.lang.Cloneable;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class CloneableTest	 implements Testlet,  Cloneable
{
	int   a = 20;
	char  b = 'b';
	Float c = new Float( 10.0f );
	
  protected static TestHarness harness;
	public void test_clone()
	{
		CloneableTest tst = null;
		try {
			tst = (CloneableTest)clone();
		}
		catch ( CloneNotSupportedException e )
		{
			harness.fail("Error: CloneNotSupportedException should not be thrown here");
		}

		if ( tst == null )
			harness.fail("Error: Clone method on Object did not work properly");
		else
		{
			if (!( tst.a == a && tst.b == b && tst.c.floatValue() == c.floatValue()))
			{
				harness.fail("Error: Clone method on Object did not clone data properly");
			}
		}	
	}

	public void test_array()
	{
		int []ia = new int[5];
		int i;

		for (i = 0; i < ia.length; i++) {
			ia[i] = i;
		}
		Cloneable c;
		Object o = ia;

		if (!(ia instanceof Cloneable)) {
			harness.fail("Error: arrays should implement Cloneable");
		}

		int []ib = (int[])ia.clone();
		Class cla = ia.getClass();
		Class clb = ib.getClass();
		if (cla != clb) {
			harness.fail("Error: array classes should be equal");
		}
		for (i = 0; i < ia.length; i++) {
			if (ib[i] != ia[i]) {
				harness.fail("Error: mismatch on cloned array at " + i);
			}
		}
	}

	public void testall()
	{
		test_clone();
		test_array();
	}

  public void test (TestHarness the_harness)
  {
    harness = the_harness;
    testall ();
  }

	
}
