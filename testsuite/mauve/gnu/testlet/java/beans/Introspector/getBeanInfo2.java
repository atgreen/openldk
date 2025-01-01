// Tags: JDK1.2
// Uses: getBeanInfoTestClass

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

/** This tests the <strong>method</strong> retrieving mechanism of the
 * Introspector class.
 * 
 * <p>See {@link getBeanInfoTestClass} for details on what is
 * expected.</p>
 * 
 * <p>The method test goes like this:</p>
 * <p>There is a test class having a number of well-known methods. All
 * <code>public</code> methods should be available in a
 * <code>MethodDescriptor</code> array. There are two methods
 * which are not <code>public</code> and these should not be
 * available in the array.</p>
 * 
 * @author Robert Schuster <theBohemian@gmx.net>
 */
public class getBeanInfo2 implements Testlet
{

    public void test(TestHarness harness)
    {
        propertyTest(harness);
    }

    void propertyTest(TestHarness harness)
    {
        BeanInfo bi = null;
        Class testClass = getBeanInfoTestClass.class;

        Method[] expectedMethods = null;
        Method[] unexpectedMethods = null;

        try
        {
            // these are the method that have to show up
            expectedMethods =
                new Method[] {
                    testClass.getMethod(
                        "setCorrectProperty",
                        new Class[] { Integer.TYPE }),
                        
                    testClass.getMethod("getCorrectProperty", new Class[0]),
                    
                    testClass.getMethod(
                        "getCorrectReadOnlyProperty",
                        new Class[0]),
                        
                    testClass.getMethod(
                        "setCorrectWriteOnlyProperty",
                        new Class[] { Integer.TYPE }),
                        
                    testClass.getMethod(
                        "setSomeStaticValue",
                        new Class[] { Integer.TYPE }),
                        
                    testClass.getMethod("getSomeStaticValue", new Class[0]),
                    };

            /* these are the method that have not to show up - it is neccessary to use
             * getDeclaredMethod() here because the wanted methods are not public.
             */
            unexpectedMethods =
                new Method[] {
                    testClass.getDeclaredMethod(
                        "setSomeValue",
                        new Class[] { Integer.TYPE }),
                        
                    testClass.getDeclaredMethod("getSomeValue", new Class[0]),
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

        // checks wether all unexpected methods have been retrieved successfully
        for (int i = 0; i < unexpectedMethods.length; i++)
        {
            harness.check(
                unexpectedMethods[i] != null,
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

        for (int i = 0; i < unexpectedMethods.length; i++)
        {
            harness.check(
                !containsMethod(methodDescriptors, unexpectedMethods[i]),
                "UNEXPECTED_METHOD_UNAVAILABE: "
                    + unexpectedMethods[i].getName());
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
