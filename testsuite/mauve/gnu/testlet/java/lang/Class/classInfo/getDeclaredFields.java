// Test for method java.lang.Class.getClass().getDeclaredFields()

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
 * Test for method java.lang.Class.getClass().getDeclaredFields()
 */
public class getDeclaredFields implements Testlet
{

    /**
     * Runs the test using the specified harness.
     *
     * @param harness  the test harness (<code>null</code> not permitted).
     */
    public void test(TestHarness harness)
    {
        // map of declared fields which should exists
        Map<String, String> testedDeclaredFields = null;

        // map of declared fields for (Open)JDK6
        Map<String, String> testedDeclaredFields_jdk6 = new HashMap<String, String>();

        // map for fields declared in (Open)JDK6
        testedDeclaredFields_jdk6.put("private static final int java.lang.Class.ANNOTATION", "ANNOTATION");
        testedDeclaredFields_jdk6.put("private static final int java.lang.Class.ENUM", "ENUM");
        testedDeclaredFields_jdk6.put("private static final int java.lang.Class.SYNTHETIC", "SYNTHETIC");
        testedDeclaredFields_jdk6.put("private transient volatile java.lang.reflect.Constructor java.lang.Class.cachedConstructor", "cachedConstructor");
        testedDeclaredFields_jdk6.put("private transient volatile java.lang.Class java.lang.Class.newInstanceCallerCache", "newInstanceCallerCache");
        testedDeclaredFields_jdk6.put("private transient java.lang.String java.lang.Class.name", "name");
        testedDeclaredFields_jdk6.put("private static java.security.ProtectionDomain java.lang.Class.allPermDomain", "allPermDomain");
        testedDeclaredFields_jdk6.put("private static boolean java.lang.Class.useCaches", "useCaches");
        testedDeclaredFields_jdk6.put("private transient volatile java.lang.ref.SoftReference java.lang.Class.declaredFields", "declaredFields");
        testedDeclaredFields_jdk6.put("private transient volatile java.lang.ref.SoftReference java.lang.Class.publicFields", "publicFields");
        testedDeclaredFields_jdk6.put("private transient volatile java.lang.ref.SoftReference java.lang.Class.declaredMethods", "declaredMethods");
        testedDeclaredFields_jdk6.put("private transient volatile java.lang.ref.SoftReference java.lang.Class.publicMethods", "publicMethods");
        testedDeclaredFields_jdk6.put("private transient volatile java.lang.ref.SoftReference java.lang.Class.declaredConstructors", "declaredConstructors");
        testedDeclaredFields_jdk6.put("private transient volatile java.lang.ref.SoftReference java.lang.Class.publicConstructors", "publicConstructors");
        testedDeclaredFields_jdk6.put("private transient volatile java.lang.ref.SoftReference java.lang.Class.declaredPublicFields", "declaredPublicFields");
        testedDeclaredFields_jdk6.put("private transient volatile java.lang.ref.SoftReference java.lang.Class.declaredPublicMethods", "declaredPublicMethods");
        testedDeclaredFields_jdk6.put("private transient volatile int java.lang.Class.classRedefinedCount", "classRedefinedCount");
        testedDeclaredFields_jdk6.put("private transient volatile int java.lang.Class.lastRedefinedCount", "lastRedefinedCount");
        testedDeclaredFields_jdk6.put("private transient sun.reflect.generics.repository.ClassRepository java.lang.Class.genericInfo", "genericInfo");
        testedDeclaredFields_jdk6.put("private static final long java.lang.Class.serialVersionUID", "serialVersionUID");
        testedDeclaredFields_jdk6.put("private static final java.io.ObjectStreamField[] java.lang.Class.serialPersistentFields", "serialPersistentFields");
        testedDeclaredFields_jdk6.put("private static sun.reflect.ReflectionFactory java.lang.Class.reflectionFactory", "reflectionFactory");
        testedDeclaredFields_jdk6.put("private static boolean java.lang.Class.initted", "initted");
        testedDeclaredFields_jdk6.put("private transient volatile java.lang.Object[] java.lang.Class.enumConstants", "enumConstants");
        testedDeclaredFields_jdk6.put("private transient volatile java.util.Map java.lang.Class.enumConstantDirectory", "enumConstantDirectory");
        testedDeclaredFields_jdk6.put("private transient java.util.Map java.lang.Class.annotations", "annotations");
        testedDeclaredFields_jdk6.put("private transient java.util.Map java.lang.Class.declaredAnnotations", "declaredAnnotations");
        testedDeclaredFields_jdk6.put("private sun.reflect.annotation.AnnotationType java.lang.Class.annotationType", "annotationType");
        testedDeclaredFields_jdk6.put("private static java.lang.annotation.Annotation[] java.lang.Class.EMPTY_ANNOTATIONS_ARRAY", "EMPTY_ANNOTATIONS_ARRAY");

        // map of declared fields for (Open)JDK7
        Map<String, String> testedDeclaredFields_jdk7 = new HashMap<String, String>();

        // map for fields declared in (Open)JDK7
        testedDeclaredFields_jdk7.put("private static final int java.lang.Class.ANNOTATION", "ANNOTATION");
        testedDeclaredFields_jdk7.put("private static final int java.lang.Class.ENUM", "ENUM");
        testedDeclaredFields_jdk7.put("private static final int java.lang.Class.SYNTHETIC", "SYNTHETIC");
        testedDeclaredFields_jdk7.put("private transient volatile java.lang.reflect.Constructor java.lang.Class.cachedConstructor", "cachedConstructor");
        testedDeclaredFields_jdk7.put("private transient volatile java.lang.Class java.lang.Class.newInstanceCallerCache", "newInstanceCallerCache");
        testedDeclaredFields_jdk7.put("private transient java.lang.String java.lang.Class.name", "name");
        testedDeclaredFields_jdk7.put("private static java.security.ProtectionDomain java.lang.Class.allPermDomain", "allPermDomain");
        testedDeclaredFields_jdk7.put("private static boolean java.lang.Class.useCaches", "useCaches");
        testedDeclaredFields_jdk7.put("private transient volatile java.lang.ref.SoftReference java.lang.Class.declaredFields", "declaredFields");
        testedDeclaredFields_jdk7.put("private transient volatile java.lang.ref.SoftReference java.lang.Class.publicFields", "publicFields");
        testedDeclaredFields_jdk7.put("private transient volatile java.lang.ref.SoftReference java.lang.Class.declaredMethods", "declaredMethods");
        testedDeclaredFields_jdk7.put("private transient volatile java.lang.ref.SoftReference java.lang.Class.publicMethods", "publicMethods");
        testedDeclaredFields_jdk7.put("private transient volatile java.lang.ref.SoftReference java.lang.Class.declaredConstructors", "declaredConstructors");
        testedDeclaredFields_jdk7.put("private transient volatile java.lang.ref.SoftReference java.lang.Class.publicConstructors", "publicConstructors");
        testedDeclaredFields_jdk7.put("private transient volatile java.lang.ref.SoftReference java.lang.Class.declaredPublicFields", "declaredPublicFields");
        testedDeclaredFields_jdk7.put("private transient volatile java.lang.ref.SoftReference java.lang.Class.declaredPublicMethods", "declaredPublicMethods");
        testedDeclaredFields_jdk7.put("private transient volatile int java.lang.Class.classRedefinedCount", "classRedefinedCount");
        testedDeclaredFields_jdk7.put("private transient volatile int java.lang.Class.lastRedefinedCount", "lastRedefinedCount");
        testedDeclaredFields_jdk7.put("private transient sun.reflect.generics.repository.ClassRepository java.lang.Class.genericInfo", "genericInfo");
        testedDeclaredFields_jdk7.put("private static final long java.lang.Class.serialVersionUID", "serialVersionUID");
        testedDeclaredFields_jdk7.put("private static final java.io.ObjectStreamField[] java.lang.Class.serialPersistentFields", "serialPersistentFields");
        testedDeclaredFields_jdk7.put("private static sun.reflect.ReflectionFactory java.lang.Class.reflectionFactory", "reflectionFactory");
        testedDeclaredFields_jdk7.put("private static boolean java.lang.Class.initted", "initted");
        testedDeclaredFields_jdk7.put("private transient volatile java.lang.Object[] java.lang.Class.enumConstants", "enumConstants");
        testedDeclaredFields_jdk7.put("private transient volatile java.util.Map java.lang.Class.enumConstantDirectory", "enumConstantDirectory");
        testedDeclaredFields_jdk7.put("private transient java.util.Map java.lang.Class.annotations", "annotations");
        testedDeclaredFields_jdk7.put("private transient java.util.Map java.lang.Class.declaredAnnotations", "declaredAnnotations");
        testedDeclaredFields_jdk7.put("private sun.reflect.annotation.AnnotationType java.lang.Class.annotationType", "annotationType");
        testedDeclaredFields_jdk7.put("transient java.lang.ClassValue$ClassValueMap java.lang.Class.classValueMap", "classValueMap");

        // get a runtime class of an object "o"
        final Class c = Class.class;

        // get the right map containing declared field signatures
        testedDeclaredFields = getJavaVersion() < 7 ? testedDeclaredFields_jdk6 : testedDeclaredFields_jdk7;

        // get all declared fields for this class
        java.lang.reflect.Field[] declaredFields = c.getDeclaredFields();

        // expected number of declared fields
        final int expectedNumberOfDeclaredFields = testedDeclaredFields.size();

        // basic check for a number of declared fields
        harness.check(declaredFields.length, expectedNumberOfDeclaredFields);

        // check if all fields exist
        for (java.lang.reflect.Field declaredField: declaredFields) {
            String fieldName = declaredField.getName();
            String fieldString = declaredField.toString();
            harness.check(testedDeclaredFields.containsKey(fieldString));
            harness.check(testedDeclaredFields.get(fieldString), fieldName);
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

