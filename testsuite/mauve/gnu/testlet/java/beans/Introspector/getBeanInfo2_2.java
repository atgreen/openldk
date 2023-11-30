// Tags: JDK1.2
// Uses: getBeanInfo2_2TestClass

//Copyright (C) 2004  Robert Schuster <theBohemian@gmx.net>

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

package gnu.testlet.java.beans.Introspector;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.MethodDescriptor;
import java.lang.reflect.Method;

/** This is the second test of the <strong>method</strong> retrieving
 * mechanism of the Introspector class.
 * 
 * <p>See {@link getBeanInfo2_2TestClass} for
 * details on what is expected.</p>
 * 
 * <p>The test goes like this:</p>
 * <p>There is a test class having a number of well-known methods. All
 * <code>public</code> methods should be available in a
 * <code>MethodDescriptor</code> array.</p>
 * 
 * @author Robert Schuster <theBohemian@gmx.net>
 */
public class getBeanInfo2_2 implements Testlet
{

    public void test(TestHarness harness)
    {
        propertyTest(harness);
    }

    void propertyTest(TestHarness harness)
    {
        BeanInfo bi = null;
        Class testClass = getBeanInfo2_2TestClass.class;

        Method[] expectedMethods = null;

        try
        {
            // these are the method that have to show up
            expectedMethods =
                new Method[] {
                	
                    testClass.getMethod(
                        "setSomething1",
                        new Class[] {
                            Integer.TYPE,
                            Integer.TYPE,
                            Integer.TYPE }),
                            
                    testClass.getMethod(
                        "getSomething2",
                        new Class[] {
                            String.class,
                            String.class,
                            String.class }),
                            
                    testClass.getMethod(
                        "isSomething3",
                        new Class[] {
                            String.class,
                            String.class,
                            String.class })
            };
        }
        catch (NoSuchMethodException nsme)
        {
            // if that happens check the test class (compiler?)
            harness.fail("TEST_PREPARATION_NOSUCHMETHODEXCEPTION");
        }

        // checks wether all expected methods have been retrieved successfully
        for (int i = 0; i < expectedMethods.length; i++)
        {
            harness.check(
                expectedMethods[i] != null,
                "TEST_PREPARATION_METHOD_INSTANCE_AVAILABLE");
        }

        // retrieves the BeanInfo instance of the test class
        try
        {
            bi = Introspector.getBeanInfo(testClass, testClass.getSuperclass());
        }
        catch (IntrospectionException ie)
        {
            harness.fail("TEST_PREPARATION_INTROSPECTION_EXCEPTION");
        }

        harness.check(bi != null, "TEST_PREPARATION_VALID_BEANINFO");

        MethodDescriptor[] methodDescriptors = bi.getMethodDescriptors();

        // Since we only introspected the test class the number of method descriptors must match
        // the number of expected methods.
        harness.check(
            methodDescriptors.length,
            expectedMethods.length,
            "MATCH_EXPECTED_METHOD_COUNT");

        for (int i = 0; i < expectedMethods.length; i++)
        {
            harness.check(
                containsMethod(methodDescriptors, expectedMethods[i]),
                "EXPECTED_METHOD_AVAILABLE: " + expectedMethods[i].getName());
        }

    }

    /** Returns whether a given method array contains a certain method.
     * 
     * @param methods The array of method to be examined.
     * @param key The method which is looked up.
     * @return Whether the key method is in the array.
     */
    boolean containsMethod(MethodDescriptor[] methodDescriptors, Method key)
    {
        for (int i = 0; i < methodDescriptors.length; i++)
        {
            if (key.equals(methodDescriptors[i].getMethod()))
                return true;
        }

        return false;
    }

}
