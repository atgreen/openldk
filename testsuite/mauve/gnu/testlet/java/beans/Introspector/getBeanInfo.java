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
import java.beans.PropertyDescriptor;
import java.lang.reflect.Method;

/** This tests the property retrieving mechanism of the
 * Introspector class.
 * 
 * <p>See {@link getBeanInfoTestClass} for details on what is
 * expected.</p>
 * 
 * <p>The property test goes like this:</p>
 * <p>There is a test class having a number of well-known methods. Some of them should
 * make a property other should not. At first <code>Method</code> instances for
 * the properties are retrieved using reflection. Then the <code>BeanInfo</code>
 * instance of the test class is retrieved. Finally the test checks whether the
 * <code>BeanInfo</code> contains the right <code>PropertyDescription</code>
 * by testing their read and write methods against the methods that were retrieved
 * in the beginning.</p>
 * 
 * @author Robert Schuster <theBohemian@gmx.net>
 */
public class getBeanInfo implements Testlet
{
	
	public void test(TestHarness harness) {
		propertyTest(harness);
	}
	
    void propertyTest(TestHarness harness)
    {
        BeanInfo bi = null;
        Class testClass = getBeanInfoTestClass.class;

        Method readWritePropertyReadMethod = null;
        Method readWritePropertyWriteMethod = null;
        Method readOnlyPropertyReadMethod = null;
        Method writeOnlyPropertyWriteMethod = null;

        try
        {
            readWritePropertyReadMethod =
                testClass.getMethod("getCorrectProperty", new Class[0]);

            readWritePropertyWriteMethod =
                testClass.getMethod(
                    "setCorrectProperty",
                    new Class[] { Integer.TYPE });

            readOnlyPropertyReadMethod =
                testClass.getMethod("getCorrectReadOnlyProperty", new Class[0]);

            writeOnlyPropertyWriteMethod =
                testClass.getMethod(
                    "setCorrectWriteOnlyProperty",
                    new Class[] { Integer.TYPE });
        }
        catch (NoSuchMethodException nsme)
        {
            // if that happens check the test class (compiler?)
            harness.fail("TEST_PREPARATION_NOSUCHMETHODEXCEPTION");
        }

        // checks availability of the instances. If any of these is null a NoSuchMethodException could have caused this 
        harness.check(
            readWritePropertyReadMethod != null,
            "TEST_PREPARATION_METHOD_INSTANCE_1");
        harness.check(
            readWritePropertyWriteMethod != null,
            "TEST_PREPARATION_METHOD_INSTANCE_2");
        harness.check(
            readOnlyPropertyReadMethod != null,
            "TEST_PREPARATION_METHOD_INSTANCE_3");
        harness.check(
            writeOnlyPropertyWriteMethod != null,
            "TEST_PREPARATION_METHOD_INSTANCE_4");

        try
        {
            // introspects only the test class (we are not interested in the Object.getClass() method)
            bi = Introspector.getBeanInfo(testClass, Object.class);
        }
        catch (IntrospectionException ie)
        {
            harness.fail("TEST_PREPARATION_INTROSPECTION_EXCEPTION");
        }

        harness.check(bi != null, "TEST_PREPARATION_VALID_BEANINFO");

        PropertyDescriptor[] propertyDescriptors = bi.getPropertyDescriptors();

        // expects 3 properties: read-write, read-only and write-only 
        harness.check(
            propertyDescriptors.length,
            3,
            "PROPERTY_DESCRIPTORS_ARRAY: expected 3 entries");

        // boolean flagging whether we reached a certain property
        boolean readWritePresent = false;

        // boolean flagging whether we reached a certain property
        boolean readOnlyPresent = false;

        // boolean flagging whether we reached a certain property
        boolean writeOnlyPresent = false;

        for (int i = 0; i < 3; i++)
        {
            String name = propertyDescriptors[i].getName();

            if (name.equals("correctProperty"))
            {
                harness.check(readWritePresent, false, "READ_WRITE_PRESENT1");
                readWritePresent = true;

                harness.check(
                    propertyDescriptors[i].getReadMethod(),
                    readWritePropertyReadMethod,
                    "READ_WRITE_READ_METHOD");
                harness.check(
                    propertyDescriptors[i].getWriteMethod(),
                    readWritePropertyWriteMethod,
                    "READ_WRITE_WRITE_METHOD");
            }
            else if (name.equals("correctReadOnlyProperty"))
            {
                harness.check(readOnlyPresent, false, "READ_ONLY_PRESENT1");
                readOnlyPresent = true;

                harness.check(
                    propertyDescriptors[i].getReadMethod(),
                    readOnlyPropertyReadMethod,
                    "READ_ONLY_READ_METHOD");
                harness.check(
                    propertyDescriptors[i].getWriteMethod(),
                    null,
                    "READ_ONLY_WRITE_METHOD");
            }
            else if (name.equals("correctWriteOnlyProperty"))
            {
                harness.check(writeOnlyPresent, false, "WRITE_ONLY_PRESENT1");
                writeOnlyPresent = true;

                harness.check(
                    propertyDescriptors[i].getReadMethod(),
                    null,
                    "WRITE_ONLY_READ_METHOD");
                harness.check(
                    propertyDescriptors[i].getWriteMethod(),
                    writeOnlyPropertyWriteMethod,
                    "WRITE_ONLY_WRITE_METHOD");
            }
        }

        // after all methods have been checked we expect having reached all properties
        harness.check(readWritePresent, "READ_WRITE_PRESENT2");
        harness.check(readOnlyPresent, "READ_ONLY_PRESENT2");
        harness.check(writeOnlyPresent, "WRITE_ONLY_PRESENT2");
    }
}
