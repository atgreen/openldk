// Test for method java.lang.String.getClass().getMethod()

// Copyright (C) 2012, 2013, 2014, 2015 Pavel Tisnovsky <ptisnovs@redhat.com>

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
// the Free Software Foundation, Inc., 51 Franklin Street,
// Fifth Floor, Boston, MA 02110-1301 USA.

// Tags: JDK1.5

package gnu.testlet.java.lang.String.classInfo;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.lang.String;
import java.util.Map;
import java.util.HashMap;



/**
 * Test for method java.lang.String.getClass().getMethod()
 */
public class getMethod implements Testlet
{

    /**
     * Runs the test using the specified harness.
     *
     * @param harness  the test harness (<code>null</code> not permitted).
     */
    public void test(TestHarness harness)
    {
        // following methods should exist
        Map<String, Class[]> methodsThatShouldExist_jdk6 = new HashMap<String, Class[]>();
        methodsThatShouldExist_jdk6.put("equals", new Class[] {java.lang.Object.class});
        methodsThatShouldExist_jdk6.put("toString", new Class[] {});
        methodsThatShouldExist_jdk6.put("hashCode", new Class[] {});
        methodsThatShouldExist_jdk6.put("compareTo", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk6.put("compareTo", new Class[] {java.lang.Object.class});
        methodsThatShouldExist_jdk6.put("indexOf", new Class[] {int.class, int.class});
        methodsThatShouldExist_jdk6.put("indexOf", new Class[] {java.lang.String.class, int.class});
        methodsThatShouldExist_jdk6.put("indexOf", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk6.put("indexOf", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("valueOf", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("valueOf", new Class[] {long.class});
        methodsThatShouldExist_jdk6.put("valueOf", new Class[] {float.class});
        methodsThatShouldExist_jdk6.put("valueOf", new Class[] {double.class});
        methodsThatShouldExist_jdk6.put("valueOf", new Class[] {java.lang.Object.class});
        methodsThatShouldExist_jdk6.put("valueOf", new Class[] {(new char[1]).getClass(), int.class, int.class});
        methodsThatShouldExist_jdk6.put("valueOf", new Class[] {(new char[1]).getClass()});
        methodsThatShouldExist_jdk6.put("valueOf", new Class[] {char.class});
        methodsThatShouldExist_jdk6.put("valueOf", new Class[] {boolean.class});
        methodsThatShouldExist_jdk6.put("length", new Class[] {});
        methodsThatShouldExist_jdk6.put("isEmpty", new Class[] {});
        methodsThatShouldExist_jdk6.put("charAt", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("codePointAt", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("codePointBefore", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("codePointCount", new Class[] {int.class, int.class});
        methodsThatShouldExist_jdk6.put("offsetByCodePoints", new Class[] {int.class, int.class});
        methodsThatShouldExist_jdk6.put("getChars", new Class[] {int.class, int.class, (new char[1]).getClass(), int.class});
        methodsThatShouldExist_jdk6.put("getBytes", new Class[] {java.nio.charset.Charset.class});
        methodsThatShouldExist_jdk6.put("getBytes", new Class[] {});
        methodsThatShouldExist_jdk6.put("getBytes", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk6.put("getBytes", new Class[] {int.class, int.class, (new byte[1]).getClass(), int.class});
        methodsThatShouldExist_jdk6.put("contentEquals", new Class[] {java.lang.CharSequence.class});
        methodsThatShouldExist_jdk6.put("contentEquals", new Class[] {java.lang.StringBuffer.class});
        methodsThatShouldExist_jdk6.put("equalsIgnoreCase", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk6.put("compareToIgnoreCase", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk6.put("regionMatches", new Class[] {boolean.class, int.class, java.lang.String.class, int.class, int.class});
        methodsThatShouldExist_jdk6.put("regionMatches", new Class[] {int.class, java.lang.String.class, int.class, int.class});
        methodsThatShouldExist_jdk6.put("startsWith", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk6.put("startsWith", new Class[] {java.lang.String.class, int.class});
        methodsThatShouldExist_jdk6.put("endsWith", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk6.put("lastIndexOf", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("lastIndexOf", new Class[] {int.class, int.class});
        methodsThatShouldExist_jdk6.put("lastIndexOf", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk6.put("lastIndexOf", new Class[] {java.lang.String.class, int.class});
        methodsThatShouldExist_jdk6.put("substring", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("substring", new Class[] {int.class, int.class});
        methodsThatShouldExist_jdk6.put("subSequence", new Class[] {int.class, int.class});
        methodsThatShouldExist_jdk6.put("concat", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk6.put("replace", new Class[] {char.class, char.class});
        methodsThatShouldExist_jdk6.put("replace", new Class[] {java.lang.CharSequence.class, java.lang.CharSequence.class});
        methodsThatShouldExist_jdk6.put("matches", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk6.put("contains", new Class[] {java.lang.CharSequence.class});
        methodsThatShouldExist_jdk6.put("replaceFirst", new Class[] {java.lang.String.class, java.lang.String.class});
        methodsThatShouldExist_jdk6.put("replaceAll", new Class[] {java.lang.String.class, java.lang.String.class});
        methodsThatShouldExist_jdk6.put("split", new Class[] {java.lang.String.class, int.class});
        methodsThatShouldExist_jdk6.put("split", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk6.put("toLowerCase", new Class[] {java.util.Locale.class});
        methodsThatShouldExist_jdk6.put("toLowerCase", new Class[] {});
        methodsThatShouldExist_jdk6.put("toUpperCase", new Class[] {});
        methodsThatShouldExist_jdk6.put("toUpperCase", new Class[] {java.util.Locale.class});
        methodsThatShouldExist_jdk6.put("trim", new Class[] {});
        methodsThatShouldExist_jdk6.put("toCharArray", new Class[] {});
        methodsThatShouldExist_jdk6.put("format", new Class[] {java.util.Locale.class, java.lang.String.class, new java.lang.Object[0].getClass()});
        methodsThatShouldExist_jdk6.put("format", new Class[] {java.lang.String.class, new java.lang.Object[0].getClass()});
        methodsThatShouldExist_jdk6.put("copyValueOf", new Class[] {(new char[1]).getClass(), int.class, int.class});
        methodsThatShouldExist_jdk6.put("copyValueOf", new Class[] {(new char[1]).getClass()});
        methodsThatShouldExist_jdk6.put("intern", new Class[] {});
        methodsThatShouldExist_jdk6.put("wait", new Class[] {long.class, int.class});
        methodsThatShouldExist_jdk6.put("wait", new Class[] {long.class});
        methodsThatShouldExist_jdk6.put("wait", new Class[] {});
        methodsThatShouldExist_jdk6.put("getClass", new Class[] {});
        methodsThatShouldExist_jdk6.put("notify", new Class[] {});
        methodsThatShouldExist_jdk6.put("notifyAll", new Class[] {});

        Map<String, Class[]> methodsThatShouldExist_jdk7 = new HashMap<String, Class[]>();
        methodsThatShouldExist_jdk7.put("equals", new Class[] {java.lang.Object.class});
        methodsThatShouldExist_jdk7.put("toString", new Class[] {});
        methodsThatShouldExist_jdk7.put("hashCode", new Class[] {});
        methodsThatShouldExist_jdk7.put("compareTo", new Class[] {java.lang.Object.class});
        methodsThatShouldExist_jdk7.put("compareTo", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk7.put("indexOf", new Class[] {java.lang.String.class, int.class});
        methodsThatShouldExist_jdk7.put("indexOf", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("indexOf", new Class[] {int.class, int.class});
        methodsThatShouldExist_jdk7.put("indexOf", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk7.put("valueOf", new Class[] {float.class});
        methodsThatShouldExist_jdk7.put("valueOf", new Class[] {double.class});
        methodsThatShouldExist_jdk7.put("valueOf", new Class[] {boolean.class});
        methodsThatShouldExist_jdk7.put("valueOf", new Class[] {(new char[1]).getClass(), int.class, int.class});
        methodsThatShouldExist_jdk7.put("valueOf", new Class[] {(new char[1]).getClass()});
        methodsThatShouldExist_jdk7.put("valueOf", new Class[] {java.lang.Object.class});
        methodsThatShouldExist_jdk7.put("valueOf", new Class[] {char.class});
        methodsThatShouldExist_jdk7.put("valueOf", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("valueOf", new Class[] {long.class});
        methodsThatShouldExist_jdk7.put("contentEquals", new Class[] {java.lang.CharSequence.class});
        methodsThatShouldExist_jdk7.put("contentEquals", new Class[] {java.lang.StringBuffer.class});
        methodsThatShouldExist_jdk7.put("charAt", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("codePointAt", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("codePointBefore", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("codePointCount", new Class[] {int.class, int.class});
        methodsThatShouldExist_jdk7.put("compareToIgnoreCase", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk7.put("concat", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk7.put("contains", new Class[] {java.lang.CharSequence.class});
        methodsThatShouldExist_jdk7.put("copyValueOf", new Class[] {(new char[1]).getClass()});
        methodsThatShouldExist_jdk7.put("copyValueOf", new Class[] {(new char[1]).getClass(), int.class, int.class});
        methodsThatShouldExist_jdk7.put("endsWith", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk7.put("equalsIgnoreCase", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk7.put("format", new Class[] {java.lang.String.class, new java.lang.Object[0].getClass()});
        methodsThatShouldExist_jdk7.put("format", new Class[] {java.util.Locale.class, java.lang.String.class, new java.lang.Object[0].getClass()});
        methodsThatShouldExist_jdk7.put("getBytes", new Class[] {});
        methodsThatShouldExist_jdk7.put("getBytes", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk7.put("getBytes", new Class[] {int.class, int.class, (new byte[1]).getClass(), int.class});
        methodsThatShouldExist_jdk7.put("getBytes", new Class[] {java.nio.charset.Charset.class});
        methodsThatShouldExist_jdk7.put("getChars", new Class[] {int.class, int.class, (new char[1]).getClass(), int.class});
        methodsThatShouldExist_jdk7.put("intern", new Class[] {});
        methodsThatShouldExist_jdk7.put("isEmpty", new Class[] {});
        methodsThatShouldExist_jdk7.put("lastIndexOf", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("lastIndexOf", new Class[] {int.class, int.class});
        methodsThatShouldExist_jdk7.put("lastIndexOf", new Class[] {java.lang.String.class, int.class});
        methodsThatShouldExist_jdk7.put("lastIndexOf", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk7.put("length", new Class[] {});
        methodsThatShouldExist_jdk7.put("matches", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk7.put("offsetByCodePoints", new Class[] {int.class, int.class});
        methodsThatShouldExist_jdk7.put("regionMatches", new Class[] {int.class, java.lang.String.class, int.class, int.class});
        methodsThatShouldExist_jdk7.put("regionMatches", new Class[] {boolean.class, int.class, java.lang.String.class, int.class, int.class});
        methodsThatShouldExist_jdk7.put("replace", new Class[] {char.class, char.class});
        methodsThatShouldExist_jdk7.put("replace", new Class[] {java.lang.CharSequence.class, java.lang.CharSequence.class});
        methodsThatShouldExist_jdk7.put("replaceAll", new Class[] {java.lang.String.class, java.lang.String.class});
        methodsThatShouldExist_jdk7.put("replaceFirst", new Class[] {java.lang.String.class, java.lang.String.class});
        methodsThatShouldExist_jdk7.put("split", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk7.put("split", new Class[] {java.lang.String.class, int.class});
        methodsThatShouldExist_jdk7.put("startsWith", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk7.put("startsWith", new Class[] {java.lang.String.class, int.class});
        methodsThatShouldExist_jdk7.put("subSequence", new Class[] {int.class, int.class});
        methodsThatShouldExist_jdk7.put("substring", new Class[] {int.class, int.class});
        methodsThatShouldExist_jdk7.put("substring", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("toCharArray", new Class[] {});
        methodsThatShouldExist_jdk7.put("toLowerCase", new Class[] {});
        methodsThatShouldExist_jdk7.put("toLowerCase", new Class[] {java.util.Locale.class});
        methodsThatShouldExist_jdk7.put("toUpperCase", new Class[] {java.util.Locale.class});
        methodsThatShouldExist_jdk7.put("toUpperCase", new Class[] {});
        methodsThatShouldExist_jdk7.put("trim", new Class[] {});
        methodsThatShouldExist_jdk7.put("wait", new Class[] {long.class, int.class});
        methodsThatShouldExist_jdk7.put("wait", new Class[] {long.class});
        methodsThatShouldExist_jdk7.put("wait", new Class[] {});
        methodsThatShouldExist_jdk7.put("getClass", new Class[] {});
        methodsThatShouldExist_jdk7.put("notify", new Class[] {});
        methodsThatShouldExist_jdk7.put("notifyAll", new Class[] {});

        // get the right map containing method signatures
        Map<String, Class[]> methodsThatShouldExist = getJavaVersion() < 7 ? methodsThatShouldExist_jdk6 : methodsThatShouldExist_jdk7;

        // create instance of a class String
        final Object o = new String();

        // get a runtime class of an object "o"
        final Class c = o.getClass();

        // check if all required methods really exist
        for (Map.Entry<String, Class[]> methodThatShouldExists : methodsThatShouldExist.entrySet()) {
            try {
                java.lang.reflect.Method method = c.getMethod(methodThatShouldExists.getKey(), methodThatShouldExists.getValue());
                harness.check(method != null);
                String methodName = method.getName();
                harness.check(methodName != null);
                harness.check(methodName, methodThatShouldExists.getKey());
            }
            catch (Exception e) {
                harness.check(false);
            }
        }
    }

    /**
     * Returns version of Java. The input could have the following form: "1.7.0_06"
     * and we are interested only in "7" in this case.
     * 
     * @return Java version
     */
    protected int getJavaVersion() {
        String javaVersionStr = System.getProperty("java.version");
        String[] parts = javaVersionStr.split("\\.");
        return Integer.parseInt(parts[1]);
    }
}

