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

package gnu.testlet.java.lang.Object;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class ObjectTest	implements Testlet
{
	boolean finFlag = false;

  protected static TestHarness harness;
	public void test_getClass()
	{
		Integer i = new Integer(10);
		Class cls = i.getClass();
		if ( cls == null )
			harness.fail("Error: test_getClass returned null");
		
		ObjectTest obj = new ObjectTest();
		if ( obj.getClass() != getClass())
			harness.fail("Error: test_getClass returned wrong class");

	}

	public void test_toString()
	{
		if ( toString() == null )
			harness.fail("Error: test_toString returned null string");
		if ( !toString().equals(getClass().getName()+"@"+
			                    Integer.toHexString(hashCode())))
			harness.fail("Error: test_toString returned wrong string");

	}

	public void test_equals()
	{
		Object nu = this;

		// reflexive
		if ( this != nu )
			harness.fail("Error: test_equals returned wrong results - 1");
		if ( !this.equals( nu ))
			harness.fail("Error: test_equals returned wrong results - 2");

		if ( !nu.equals( nu ))
			harness.fail("Error: test_equals returned wrong results - 3");
		

		// symmetric
		Object nu1 = nu;

		if ( ! ( nu.equals(nu1) && nu1.equals(nu)))
			harness.fail("Error: test_equals returned wrong results - 4");

		// transitive
		if ( ! ( nu.equals(nu1) && nu1.equals(this) && equals(nu)))
			harness.fail("Error: test_equals returned wrong results - 5");

		Object p = null;
		if ( equals( p ))
			harness.fail("Error: test_equals returned wrong results - 6");
	}

	public void test_hashCode()
	{
		Object s = this;
		if ( s.hashCode() != hashCode() )
			harness.fail("Error: test_hashCode returned wrong results - 1");

	    int hash = s.hashCode();

		if ( hash != s.hashCode())
			harness.fail("Error: test_hashCode returned wrong results - 2");
	} 


	public void test_clone()
	{
		try {
			clone();
			harness.fail("Error: test_clone did not raise CloneNotSupportedException");
		}
		catch ( CloneNotSupportedException e ){}

		java.util.Vector v = new java.util.Vector();
		java.util.Vector vclone;
		try {
			vclone = (java.util.Vector)v.clone();
		}
		catch ( Exception e ){
			if (e instanceof CloneNotSupportedException) {
			harness.fail("Error: test_clone should not raise CloneNotSupportedException"+
				               " on Vector " );
			} else {
			harness.fail("Error: test_clone should not raise Exception "+
				               e + " on Vector " );
			}
		}

	/*	if (!(( vclone != v ) && ( vclone.getClass() == v.getClass()) && 
			(vclone.equals( v) )))
			harness.fail("Error: test_clone did not return proper values");
	*/
	}

	public void testall()
	{
		test_getClass();
		test_toString();
		test_equals();
		test_hashCode();
		test_clone();
	}

  public void test (TestHarness the_harness)
  {
    harness = the_harness;
    testall ();
  }

}
