// Tags: JDK1.2
// Uses: TestBean1 TestBean2 TestBean3 TestBean4

//Copyright (C) 2004 Robert Schuster <thebohemian@gmx.net>

//Mauve is free software; you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation; either version 2, or (at your option)
//any later version.

//Mauve is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.

//You should have received a copy of the GNU General Public License
//along with Mauve; see the file COPYING.  If not, write to
//the Free Software Foundation, 59 Temple Place - Suite 330,
//Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.beans.Beans;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.beans.Beans;

/** Various <code>java.beans.Beans.instantiate</code> tests.
 * 
 * @author Robert Schuster
 */
public class instantiate_1 implements Testlet
{

    public void test(TestHarness harness)
    {
	ClassLoader cl = getClass().getClassLoader();

        /** Tries to instantiate a Bean with a <code>private</code>
         * constructor and expects an <code>IllegalAccessException</code>
         * wrapped in an <code>ClassNotFoundException</code> to be thrown.
         */
        try
        {
            Beans.instantiate(cl, "gnu.testlet.java.beans.Beans.TestBean1");

            // If this is called then the instantiation succeeded.
            harness.fail("Private constructor 1");
        }
        catch (Exception e)
        {
            harness.check(
                e instanceof ClassNotFoundException,
                "Private constructor 2");
            harness.check(
                e.getCause() instanceof IllegalAccessException,
                "Private constructor 3");
        }

        /** Tries to instantiate a Bean that throws a RuntimeException
         * in its constructor and expects an <code>RuntimeException</code>
         * wrapped in an <code>ClassNotFoundException</code> to be thrown.
         */
        try
        {
            Beans.instantiate(cl, "gnu.testlet.java.beans.Beans.TestBean2");

            // If this is called then the instantiation succeeded.
            harness.fail("Exception in Constructor 1");
        }
        catch (Exception e)
        {
            harness.check(
                e instanceof ClassNotFoundException,
                "Exception in Constructor 2");

            harness.check(
                e.getCause() instanceof RuntimeException,
                "Exception in Constructor 3");
        }

	/** TestBean3 does not provide a zero-argument constructor. This results in
	 * an InstantiationException that is wrapped in a ClassNotFoundException.
	 */
	try
	{
		Beans.instantiate(cl, "gnu.testlet.java.beans.Beans.TestBean3");

		// If this is called then the instantiation succeeded.
		harness.fail("Missing zero-argument constructor 1");
	}
	catch (Exception e)
	{
		harness.check(
			e instanceof ClassNotFoundException,
			"Missing zero-argument constructor 2");

		harness.check(
			e.getCause() instanceof InstantiationException,
			"Missing zero-argument constructor 3");
	}

	/* TestBean4 throws a specific TestBean4.Error. The Bean.instantiate
	 * method should not intercept this. That means we can catch the
	 * the TestBean4.Error instance here.
	 */
	try
	{
		Beans.instantiate(cl, "gnu.testlet.java.beans.Beans.TestBean4");

		// If this is called then the instantiation succeeded.
		harness.fail("specific Error in constructor 1");
	}
	catch (TestBean4.Error _)
	{
		harness.check(true, "specific Error in constructor 2");
	}
	catch(Exception e) {
		harness.fail("specific Error in constructor 3");
	}
	
    }
}
