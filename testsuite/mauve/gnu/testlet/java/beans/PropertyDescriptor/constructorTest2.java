// Tags: JDK1.1

//Copyright (C) 1998 Anthony Green <green@cygnus.com>
//Copyright (C) 1998 Tom Tromey <tromey@cygnus.com>
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

package gnu.testlet.java.beans.PropertyDescriptor;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.beans.PropertyDescriptor;
import java.beans.IntrospectionException;
import java.lang.reflect.Method;

/**
 * 
 * @author Anthony Green <green@cygnus.com>
 * @author Tom Tromey <tromey@cygnus.com>
 * @author Robert Schuster <thebohemian@gmx.net>
 */
public class constructorTest2 implements Testlet
{
    /** Read method of <code>int</code> property "prop1".
     * 
     * @return 0.
     */
    public int getProp1()
    {
        return 0;
    }

    /** Write method of <code>int</code> property "prop1".
     * 
     * @param val Unused.
     */
    public void setProp1(int val)
    {}

    /** Read method of <code>boolean</code> property "prop2".
     *
     * This is the "get" variant of a <code>boolean</code> getter. 
     * 
     * @return <code>false</code>.
     */
    public boolean getProp2()
    {
        return false;
    }

    /** Write method of <code>boolean</code> property "prop2".
     * 
     * @param val Unused.
     */
    public void setProp2(boolean val)
    {}

    /** Read method of <code>boolean</code> property "prop3".
     *
     * This is the "is" variant of a <code>boolean</code> getter. 
     * 
     * @return <code>false</code>.
     */
    public boolean isProp3()
    {
        return false;
    }

    /** Write method of <code>boolean</code> property "prop3".
     * 
     * @param val Unused.
     */
    public void setProp3(boolean val)
    {}

    /** Read method of <code>int</code> property "prop4".
     *
     * This demonstrates an "is" variant of a <code>int</code> getter,
     * which is allowed in Sun's implementation of <code>PropertyDescriptor</code> 
     * 
     * @return <code>0</code>.
     */
    public int isProp4()
    {
        return 0;
    }

    /** Write method of <code>int</code> property "prop4".
     * 
     * @param val Unused.
     */
    public void setProp4(int val)
    {}

    /** Read method of <code>boolean</code> property "prop5".<br/>
     * <br>
     * This is the "is" variant of the "getter". The "get" variant exists, too.
     * Sun's implementation of <code>PropertyDescriptor</code> allows that.<br/>
     * <br/> 
     * Note: The difference to "prop6" is the order in which the methods are declared
     * in the source code.
     * 
     * @return <code>false</code>.
     */
    public boolean isProp5()
    {
        return false;
    }

    /** Read method of <code>boolean</code> property "prop5".<br/>
     * <br>
     * This is the "get" variant of the "getter". The "is" variant exists, too.
     * Sun's implementation of <code>PropertyDescriptor</code> allows that.<br/>
     * <br/> 
     * Note: The difference to "prop6" is the order in which the methods are declared
     * in the source code.
     * 
     * @return <code>false</code>.
     */
    public boolean getProp5()
    {
        return false;
    }

    /** Write method of <code>boolean</code> property "prop5".
     * 
     * @param val Unused.
     */
    public void setProp5(boolean val)
    {}

    /** Read method of <code>boolean</code> property "prop6".<br/>
     * <br>
     * This is the "get" variant of the "getter". The "is" variant exists, too.
     * Sun's implementation of <code>PropertyDescriptor</code> allows that.<br/>
     * <br/> 
     * Note: The difference to "prop5" is the order in which the methods are declared
     * in the source code.
     * 
     * @return <code>false</code>.
     */
    public boolean getProp6()
    {
        return false;
    }

    /** Read method of <code>boolean</code> property "prop6".<br/>
     * <br>
     * This is the "is" variant of the "getter". The "get" variant exists, too.
     * Sun's implementation of <code>PropertyDescriptor</code> allows that.<br/>
     * <br/> 
     * Note: The difference to "prop5" is the order in which the methods are declared
     * in the source code.
     * 
     * @return <code>false</code>.
     */
    public boolean isProp6()
    {
        return false;
    }

    /** Write method of <code>boolean</code> property "prop6".
     * 
     * @param val Unused.
     */
    public void setProp6(boolean val)
    {}

    /** <code>boolean</code> read method of property "prop7" that
     * has multiple "setter" or write methods with different argument types:
     * {@link setProp7(boolean)} and {@link setProp(int)}.<br/>
     * <br>
     * Sun's implementation of <code>PropertyDescriptor</code> allows that.<br/>
     * 
     * @return <code>false</code>.
     */
    public boolean getProp7()
    {
        return false;
    }

    /** <code>boolean</code> write method of property "prop7" that
     * has multiple "setter" or write methods with different argument types.
     * See {@link setProp(int)} for the other write method.<br/>
     * <br>
     * Sun's implementation of <code>PropertyDescriptor</code> allows that.<br/>
     * 
     * @param val Unused.
     */
    public void setProp7(boolean val)
    {}

    /** <code>int</code> write method of property "prop7" that
     * has multiple "setter" or write methods with different argument types.
     * See {@link setProp(boolean)} for the other write method.<br/>
     * <br>
     * Sun's implementation of <code>PropertyDescriptor</code> allows that.<br/>
     * 
     * @param val Unused.
     */
    public void setProp7(int val)
    {}

    /** Read method of <code>boolean</code> property "error1".
     * This is a read-only property which is not allowed when using the
     * <code>(String, Class)</code> constructor of <code>PropertyDescriptor</code>.<br/>
     * <br/>
     * This is the "get" variant of a read method.
     * 
     * @return <code>false</code>.
     */
    public boolean getError1()
    {
        return false;
    }

    /** Read method of <code>boolean</code> property "error2".
     * This is a read-only property which is not allowed when using the
     * <code>(String, Class)</code> constructor of <code>PropertyDescriptor</code>.<br/>
     * <br/>
     * This is the "is" variant of a read method.
     * 
     * @return <code>false</code>.
     */
    public boolean isError2()
    {
        return false;
    }

    /** Write method of <code>boolean</code> property "error3".
     * This is a write-only property which is not allowed when using the
     * <code>(String, Class)</code> constructor of <code>PropertyDescriptor</code>.<br/>
     * <br/>
     * 
     * @param val Unused.
     */
    public void setError3(boolean val)
    {}

    /** Invalid read method of property "error4". It is illegal
     * because read methods are not allowed to have a <code>void</code>
     * return type.<br/>
     * <br/>
     * This method is the "get" variant of a read method. 
     */
    public void getError4()
    {}

    /** Write method of erroneous property "error4".
     * 
     * @param val Unused.
     */
    public void setError4(int val)
    {}

    /** Invalid read method of property "error5". It is illegal
     * because read methods are not allowed to have a <code>void</code>
     * return type.<br/>
     * <br/>
     * This method is the "is" variant of a read method. 
     */
    public void isError5()
    {}

    /** Write method of erroneous property "error5".
     * 
     * @param val Unused.
     */
    public void setError5(int val)
    {}

    /** Invalid read method of property "error6". It is illegal
     * because its return type is not compatible to the write method's 
     * argument type.<br/>
     * <br/>
     * This method is the "get" variant of a read method.
     * 
     * @return 0. 
     */
    public int getError6()
    {
        return 0;
    }

    /** Invalid write method of property "error6". It is illegal
     * because its argument type is not compatible to the read method's 
     * return type.<br/>
     * <br/>
     * 
     * @param val Unused. 
     */
    public void setError6(boolean val)
    {}

    /** Invalid read method of property "error7". It is illegal
     * because its return type is not compatible to the write method's 
     * argument type.<br/>
     * <br/>
     * This method is the "is" variant of a read method.
     * 
     * @return 0. 
     */
    public boolean isError7()
    {
        return false;
    }

    /** Invalid write method of property "error7". It is illegal
     * because its argument type is not compatible to the read method's 
     * return type.<br/>
     * <br/>
     * 
     * @param val Unused. 
     */
    public void setError7(int val)
    {}

    /** Invalid read method of property "error8". It is illegal
     * because its return type is not compatible to the write method's 
     * argument type. This demonstrates a type mismatch with non-primitives.<br/>
     * <br/>
     * This method is the "get" variant of a read method.
     *
     * XXX: I doubt this is legal because the return type of the read method is assignable
     * from the write method's argument type.
     *  
     * @return 0. 
     */
    public Object getError8()
    {
        return null;
    }

    /** Invalid write method of property "error8". It is illegal
    * because its argument type is not compatible to the read method's 
    * return type. This demonstrates a type mismatch with non-primitives.<br/>
    * <br/>
    * 
    * @param val Unused. 
    */
    public void setError8(String val)
    {}

	/** Invalid read method of property "error9". It is illegal
	 * because its return type is not compatible to the write method's 
	 * argument type. This demonstrates a type mismatch with non-primitives.<br/>
	 * <br/>
	 * This method is the "get" variant of a read method.
	 *  
	 * @return 0. 
	 */
    public String getError9()
    {
        return null;
    }

	/** Invalid write method of property "error9". It is illegal
	* because its argument type is not compatible to the read method's 
	* return type. This demonstrates a type mismatch with non-primitives.<br/>
	* <br/>
	* 
	* @param val Unused. 
	*/
    public void setError9(Object val)
    {}

	/** Read method of <code>boolean</code> property "error10".
	 * The property is invalid because its write method does not accept
	 * a single argument.
	 * 
	 * @return <code>false</code>.
	 */
    public boolean isError10()
    {
        return false;
    }
    
	/** Invalid write method of property "error10". It is illegal
	* because it does not accept a single argument.
	*/
    public void setError10()
    {}

	/** Read method of <code>boolean</code> property "error11".
	 * The property is invalid because its read method accepts an argument.<br/>
	 * <br>
	 * This is the "get" variant of a read method.
	 *
	 * @param arg Unused. 
	 * @return <code>false</code>.
	 */
    public boolean getError11(int arg)
    {
        return false;
    }
    
    /** Write method of <code>boolean</code> property "error11".
     * The property is invalid because of its read method.
     * 
     * @param val Unused.
     */
    public void setError11(boolean val)
    {}

	/** Read method of <code>boolean</code> property "error12".
	 * The property is invalid because its read method accepts an argument.<br/>
	 * <br>
	 * This is the "is" variant of a read method.
	 *
	 * @param arg Unused. 
	 * @return <code>false</code>.
	 */
    public boolean isError12(int arg)
    {
        return false;
    }

	/** Write method of <code>boolean</code> property "error12".
	 * The property is invalid because of its read method.
	 * 
	 * @param val Unused.
	 */
    public void setError12(boolean val)
    {}

	/** Read method of <code>boolean</code> property "error13".
	 * The property is invalid because of its write method.
	 * <br>
	 *
	 * @return <code>false</code>.
	 */
    public boolean isError13()
    {
        return false;
    }
    
	/** Write method of <code>boolean</code> property "error13".
	 * The property is invalid because this method accepts an
	 * extra argument.<br/>
	 * 
	 * @param val Unused.
	 * @param arg Unused.
	 */
    public void setError13(boolean val, int arg)
    {}

	/** Read method of illegally named <code>Object</code> property.
	 * 
	 * @return <code>null</code>.
	 */
    public Object get()
    {
        return null;
    }

	/** Write method of illegally named <code>Object</code> property.
	 * 
	 * @return <code>null</code>.
	 */
    public void set(Object val)
    {}

	/** Read method of illegally named <code>Object</code> property.
	 * The method name does not respect the requirements of a JavaBean.
	 * 
	 * @return <code>null</code>.
	 */
    public Object getknotted()
    {
        return null;
    }

	/** Write method of illegally named <code>Object</code> property.
	 * The method name does not respect the requirements of a JavaBean.
	 * 
	 * @param val Unused.
	 */
    public void setknotted(Object val)
    {}

    // Test the PropertyDescriptor(Class, String) constructor
    private void constructor1Tests(TestHarness harness)
    {
        String[] goodProperties =
            { "prop1", "prop2", "prop3", "prop4", "prop5", "prop6", "prop7" };
        for (int i = 0; i < goodProperties.length; i++)
        {
            harness.checkPoint(
                "new PropertyDescriptor("
                    + goodProperties[i]
                    + ") (should succeed)");
            PropertyDescriptor pd;
            try
            {
                pd =
                    new PropertyDescriptor(
                        goodProperties[i],
                        constructorTest2.class);
                harness.check(true);
            }
            catch (IntrospectionException e)
            {
                harness.debug(e);
                harness.check(false);
                continue;
            }
            harness.check(pd.getReadMethod() != null);
            harness.check(pd.getWriteMethod() != null);
        }

        harness.checkPoint("string constructor - binding");
        String[] bindProperties = { "prop5", "prop6" };
        for (int i = 0; i < bindProperties.length; i++)
        {
            harness.checkPoint(
                "new PropertyDescriptor("
                    + bindProperties[i]
                    + ") (check bindings)");
            try
            {
                PropertyDescriptor pd =
                    new PropertyDescriptor(
                        bindProperties[i],
                        constructorTest2.class);

                Class propType = pd.getPropertyType();

                Method readMethod = pd.getReadMethod();

                harness.check(readMethod != null);

                if (readMethod == null)
                {
                    continue;
                }

                harness.check(readMethod.getName().startsWith("is"));
                Class resType = readMethod.getReturnType();
                harness.check(resType.equals(propType));

                Method writeMethod = pd.getWriteMethod();
                harness.check(writeMethod != null);

                if (writeMethod == null)
                {
                    continue;
                }

                Class argType = writeMethod.getParameterTypes()[0];
                harness.check(argType.equals(propType));

            }
            catch (IntrospectionException e)
            {}

        }

        String[] badProperties =
            {
                "",
                "foo",
                "knotted",
                "???",
                "error1",
                "error2",
                "error3",
                "error4",
                "error5",
                "error6",
                "error7",
                "error8",
                "error9",
                "error10",
                "error11",
                "error12",
                "error13" };

        for (int i = 0; i < badProperties.length; i++)
        {
            harness.checkPoint(
                "new PropertyDescriptor("
                    + badProperties[i]
                    + ") (should fail)");
            boolean ok = false;
            try
            {
                new PropertyDescriptor(
                    badProperties[i],
                    constructorTest2.class);
            }
            catch (IntrospectionException e)
            {
                ok = true;
            }
            harness.check(ok);
        }
    }

    private void constructor2Tests(TestHarness harness)
    {
        String[][] goodProperties = { { "prop1", "getProp1", "setProp1" }, {
                "prop2", "getProp2", "setProp2" }, {
                "prop3", "isProp3", "setProp3" }, {
                "prop4", "isProp4", "setProp4" }, {
                "prop5", "isProp5", "setProp5" }, {
                "prop5", "getProp5", "setProp5" }, {
                "prop6", "isProp6", "setProp6" }, {
                "prop6", "getProp6", "setProp6" }, {
                "prop7", "getProp7", "setProp7" }, {
                "prop7", "getProp7", null }, {
                "prop7", null, "setProp7" }, {
                "prop7", null, null }, };

        for (int i = 0; i < goodProperties.length; i++)
        {
            harness.checkPoint(
                "new PropertyDescriptor("
                    + goodProperties[i][0]
                    + ", "
                    + goodProperties[i][1]
                    + ", "
                    + goodProperties[i][2]
                    + ") (should succeed)");

            boolean ok = true;

            try
            {
                new PropertyDescriptor(
                    goodProperties[i][0],
                    constructorTest2.class,
                    goodProperties[i][1],
                    goodProperties[i][2]);
            }
            catch (IntrospectionException e)
            {
                harness.debug(e);
                ok = false;
            }
            harness.check(ok);
        }

        String[][] badProperties = { { "foo", "getFoo", "setFoo" }, {
                "foo2", null, "setFoo2" }, {
                "foo3", "getFoo3", null }, {
                "bar", "", "" }, {
                "error4", "getError4", "setError4" }, {
                "error5", "isError5", "setError5" }, {
                "error6", "getError6", "setError6" }, {
                "error7", "isError7", "setError7" }, {
                "error8", "getError8", "setError8" }, {
                "error9", "getError9", "setError9" }, {
                "error10", "isError10", "setError10" }, {
                "error11", "getError11", "setError11" }, {
                "error12", "isError12", "setError12" }, {
                "error13", "isError13", "setError13" }, };

        for (int i = 0; i < badProperties.length; i++)
        {
            harness.checkPoint(
                "new PropertyDescriptor("
                    + badProperties[i][0]
                    + ", "
                    + badProperties[i][1]
                    + ", "
                    + badProperties[i][2]
                    + ") (should fail)");
            boolean ok = false;
            try
            {
                new PropertyDescriptor(
                    badProperties[i][0],
                    constructorTest2.class,
                    badProperties[i][1],
                    badProperties[i][2]);
            }
            catch (IntrospectionException e)
            {
                ok = true;
            }
            harness.check(ok);
        }
    }

    public void test(TestHarness harness)
    {
        constructor1Tests(harness);
        constructor2Tests(harness);
    }

}
