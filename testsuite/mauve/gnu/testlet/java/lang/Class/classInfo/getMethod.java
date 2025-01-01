// Test for method java.lang.Class.getClass().getMethod()

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

package gnu.testlet.java.lang.Class.classInfo;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.lang.Class;
import java.util.Map;
import java.util.HashMap;



/**
 * Test for method java.lang.Class.getClass().getMethod()
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
        methodsThatShouldExist_jdk6.put("forName", new Class[] {java.lang.String.class, boolean.class, java.lang.ClassLoader.class});
        methodsThatShouldExist_jdk6.put("forName", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk6.put("toString", new Class[] {});
        methodsThatShouldExist_jdk6.put("isAssignableFrom", new Class[] {java.lang.Class.class});
        methodsThatShouldExist_jdk6.put("isInstance", new Class[] {java.lang.Object.class});
        methodsThatShouldExist_jdk6.put("getModifiers", new Class[] {});
        methodsThatShouldExist_jdk6.put("isInterface", new Class[] {});
        methodsThatShouldExist_jdk6.put("isArray", new Class[] {});
        methodsThatShouldExist_jdk6.put("isPrimitive", new Class[] {});
        methodsThatShouldExist_jdk6.put("getSuperclass", new Class[] {});
        methodsThatShouldExist_jdk6.put("getComponentType", new Class[] {});
        methodsThatShouldExist_jdk6.put("getName", new Class[] {});
        methodsThatShouldExist_jdk6.put("newInstance", new Class[] {});
        methodsThatShouldExist_jdk6.put("isAnnotation", new Class[] {});
        methodsThatShouldExist_jdk6.put("isSynthetic", new Class[] {});
        methodsThatShouldExist_jdk6.put("getClassLoader", new Class[] {});
        methodsThatShouldExist_jdk6.put("getTypeParameters", new Class[] {});
        methodsThatShouldExist_jdk6.put("getGenericSuperclass", new Class[] {});
        methodsThatShouldExist_jdk6.put("getPackage", new Class[] {});
        methodsThatShouldExist_jdk6.put("getInterfaces", new Class[] {});
        methodsThatShouldExist_jdk6.put("getGenericInterfaces", new Class[] {});
        methodsThatShouldExist_jdk6.put("getSigners", new Class[] {});
        methodsThatShouldExist_jdk6.put("getEnclosingMethod", new Class[] {});
        methodsThatShouldExist_jdk6.put("getEnclosingConstructor", new Class[] {});
        methodsThatShouldExist_jdk6.put("getDeclaringClass", new Class[] {});
        methodsThatShouldExist_jdk6.put("getEnclosingClass", new Class[] {});
        methodsThatShouldExist_jdk6.put("getSimpleName", new Class[] {});
        methodsThatShouldExist_jdk6.put("getCanonicalName", new Class[] {});
        methodsThatShouldExist_jdk6.put("isAnonymousClass", new Class[] {});
        methodsThatShouldExist_jdk6.put("isLocalClass", new Class[] {});
        methodsThatShouldExist_jdk6.put("isMemberClass", new Class[] {});
        methodsThatShouldExist_jdk6.put("getClasses", new Class[] {});
        methodsThatShouldExist_jdk6.put("getFields", new Class[] {});
        methodsThatShouldExist_jdk6.put("getMethods", new Class[] {});
        methodsThatShouldExist_jdk6.put("getConstructors", new Class[] {});
        methodsThatShouldExist_jdk6.put("getField", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk6.put("getMethod", new Class[] {java.lang.String.class, new java.lang.Class[0].getClass()});
        methodsThatShouldExist_jdk6.put("getConstructor", new Class[] {new java.lang.Class[0].getClass()});
        methodsThatShouldExist_jdk6.put("getDeclaredClasses", new Class[] {});
        methodsThatShouldExist_jdk6.put("getDeclaredFields", new Class[] {});
        methodsThatShouldExist_jdk6.put("getDeclaredMethods", new Class[] {});
        methodsThatShouldExist_jdk6.put("getDeclaredConstructors", new Class[] {});
        methodsThatShouldExist_jdk6.put("getDeclaredField", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk6.put("getDeclaredMethod", new Class[] {java.lang.String.class, new java.lang.Class[0].getClass()});
        methodsThatShouldExist_jdk6.put("getDeclaredConstructor", new Class[] {new java.lang.Class[0].getClass()});
        methodsThatShouldExist_jdk6.put("getResourceAsStream", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk6.put("getResource", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk6.put("getProtectionDomain", new Class[] {});
        methodsThatShouldExist_jdk6.put("desiredAssertionStatus", new Class[] {});
        methodsThatShouldExist_jdk6.put("isEnum", new Class[] {});
        methodsThatShouldExist_jdk6.put("getEnumConstants", new Class[] {});
        methodsThatShouldExist_jdk6.put("cast", new Class[] {java.lang.Object.class});
        methodsThatShouldExist_jdk6.put("asSubclass", new Class[] {java.lang.Class.class});
        methodsThatShouldExist_jdk6.put("getAnnotation", new Class[] {java.lang.Class.class});
        methodsThatShouldExist_jdk6.put("isAnnotationPresent", new Class[] {java.lang.Class.class});
        methodsThatShouldExist_jdk6.put("getAnnotations", new Class[] {});
        methodsThatShouldExist_jdk6.put("getDeclaredAnnotations", new Class[] {});
        methodsThatShouldExist_jdk6.put("wait", new Class[] {long.class, int.class});
        methodsThatShouldExist_jdk6.put("wait", new Class[] {long.class});
        methodsThatShouldExist_jdk6.put("wait", new Class[] {});
        methodsThatShouldExist_jdk6.put("equals", new Class[] {java.lang.Object.class});
        methodsThatShouldExist_jdk6.put("hashCode", new Class[] {});
        methodsThatShouldExist_jdk6.put("getClass", new Class[] {});
        methodsThatShouldExist_jdk6.put("notify", new Class[] {});
        methodsThatShouldExist_jdk6.put("notifyAll", new Class[] {});

        Map<String, Class[]> methodsThatShouldExist_jdk7 = new HashMap<String, Class[]>();
        methodsThatShouldExist_jdk7.put("forName", new Class[] {java.lang.String.class, boolean.class, java.lang.ClassLoader.class});
        methodsThatShouldExist_jdk7.put("forName", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk7.put("toString", new Class[] {});
        methodsThatShouldExist_jdk7.put("isAssignableFrom", new Class[] {java.lang.Class.class});
        methodsThatShouldExist_jdk7.put("isInstance", new Class[] {java.lang.Object.class});
        methodsThatShouldExist_jdk7.put("getModifiers", new Class[] {});
        methodsThatShouldExist_jdk7.put("isInterface", new Class[] {});
        methodsThatShouldExist_jdk7.put("isArray", new Class[] {});
        methodsThatShouldExist_jdk7.put("isPrimitive", new Class[] {});
        methodsThatShouldExist_jdk7.put("getSuperclass", new Class[] {});
        methodsThatShouldExist_jdk7.put("getComponentType", new Class[] {});
        methodsThatShouldExist_jdk7.put("getName", new Class[] {});
        methodsThatShouldExist_jdk7.put("asSubclass", new Class[] {java.lang.Class.class});
        methodsThatShouldExist_jdk7.put("cast", new Class[] {java.lang.Object.class});
        methodsThatShouldExist_jdk7.put("desiredAssertionStatus", new Class[] {});
        methodsThatShouldExist_jdk7.put("getAnnotation", new Class[] {java.lang.Class.class});
        methodsThatShouldExist_jdk7.put("getAnnotations", new Class[] {});
        methodsThatShouldExist_jdk7.put("getCanonicalName", new Class[] {});
        methodsThatShouldExist_jdk7.put("getClassLoader", new Class[] {});
        methodsThatShouldExist_jdk7.put("getClasses", new Class[] {});
        methodsThatShouldExist_jdk7.put("getConstructor", new Class[] {new java.lang.Class[0].getClass()});
        methodsThatShouldExist_jdk7.put("getConstructors", new Class[] {});
        methodsThatShouldExist_jdk7.put("getDeclaredAnnotations", new Class[] {});
        methodsThatShouldExist_jdk7.put("getDeclaredClasses", new Class[] {});
        methodsThatShouldExist_jdk7.put("getDeclaredConstructor", new Class[] {new java.lang.Class[0].getClass()});
        methodsThatShouldExist_jdk7.put("getDeclaredConstructors", new Class[] {});
        methodsThatShouldExist_jdk7.put("getDeclaredField", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk7.put("getDeclaredFields", new Class[] {});
        methodsThatShouldExist_jdk7.put("getDeclaredMethod", new Class[] {java.lang.String.class, new java.lang.Class[0].getClass()});
        methodsThatShouldExist_jdk7.put("getDeclaredMethods", new Class[] {});
        methodsThatShouldExist_jdk7.put("getDeclaringClass", new Class[] {});
        methodsThatShouldExist_jdk7.put("getEnclosingClass", new Class[] {});
        methodsThatShouldExist_jdk7.put("getEnclosingConstructor", new Class[] {});
        methodsThatShouldExist_jdk7.put("getEnclosingMethod", new Class[] {});
        methodsThatShouldExist_jdk7.put("getEnumConstants", new Class[] {});
        methodsThatShouldExist_jdk7.put("getField", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk7.put("getFields", new Class[] {});
        methodsThatShouldExist_jdk7.put("getGenericInterfaces", new Class[] {});
        methodsThatShouldExist_jdk7.put("getGenericSuperclass", new Class[] {});
        methodsThatShouldExist_jdk7.put("getInterfaces", new Class[] {});
        methodsThatShouldExist_jdk7.put("getMethod", new Class[] {java.lang.String.class, new java.lang.Class[0].getClass()});
        methodsThatShouldExist_jdk7.put("getMethods", new Class[] {});
        methodsThatShouldExist_jdk7.put("getPackage", new Class[] {});
        methodsThatShouldExist_jdk7.put("getProtectionDomain", new Class[] {});
        methodsThatShouldExist_jdk7.put("getResource", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk7.put("getResourceAsStream", new Class[] {java.lang.String.class});
        methodsThatShouldExist_jdk7.put("getSigners", new Class[] {});
        methodsThatShouldExist_jdk7.put("getSimpleName", new Class[] {});
        methodsThatShouldExist_jdk7.put("getTypeParameters", new Class[] {});
        methodsThatShouldExist_jdk7.put("isAnnotation", new Class[] {});
        methodsThatShouldExist_jdk7.put("isAnnotationPresent", new Class[] {java.lang.Class.class});
        methodsThatShouldExist_jdk7.put("isAnonymousClass", new Class[] {});
        methodsThatShouldExist_jdk7.put("isEnum", new Class[] {});
        methodsThatShouldExist_jdk7.put("isLocalClass", new Class[] {});
        methodsThatShouldExist_jdk7.put("isMemberClass", new Class[] {});
        methodsThatShouldExist_jdk7.put("isSynthetic", new Class[] {});
        methodsThatShouldExist_jdk7.put("newInstance", new Class[] {});
        methodsThatShouldExist_jdk7.put("wait", new Class[] {long.class, int.class});
        methodsThatShouldExist_jdk7.put("wait", new Class[] {long.class});
        methodsThatShouldExist_jdk7.put("wait", new Class[] {});
        methodsThatShouldExist_jdk7.put("equals", new Class[] {java.lang.Object.class});
        methodsThatShouldExist_jdk7.put("hashCode", new Class[] {});
        methodsThatShouldExist_jdk7.put("getClass", new Class[] {});
        methodsThatShouldExist_jdk7.put("notify", new Class[] {});
        methodsThatShouldExist_jdk7.put("notifyAll", new Class[] {});

        // get the right map containing method signatures
        Map<String, Class[]> methodsThatShouldExist = getJavaVersion() < 7 ? methodsThatShouldExist_jdk6 : methodsThatShouldExist_jdk7;

        // get a runtime class of an object "o"
        final Class c = Class.class;

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

