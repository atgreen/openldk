// Test for method java.lang.Package.getClass().getDeclaredMethods()

// Copyright (C) 2012, 2013, 2014, 2015, 2016 Pavel Tisnovsky <ptisnovs@redhat.com>

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

package gnu.testlet.java.lang.Package.classInfo;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.lang.Package;
import java.util.Map;
import java.util.HashMap;



/**
 * Test for method java.lang.Package.getClass().getDeclaredMethods()
 */
public class getDeclaredMethods implements Testlet
{

    /**
     * Runs the test using the specified harness.
     *
     * @param harness  the test harness (<code>null</code> not permitted).
     */
    public void test(TestHarness harness)
    {
        // map of declared methods which should exists
        Map<String, String> testedDeclaredMethods = null;

        // map of declared methods for (Open)JDK6
        Map<String, String> testedDeclaredMethods_jdk6 = new HashMap<String, String>();

        // map of declared methods for (Open)JDK7
        Map<String, String> testedDeclaredMethods_jdk7 = new HashMap<String, String>();

        // map for methods declared in (Open)JDK6
        testedDeclaredMethods_jdk6.put("public java.lang.String java.lang.Package.toString()", "toString");
        testedDeclaredMethods_jdk6.put("public int java.lang.Package.hashCode()", "hashCode");
        testedDeclaredMethods_jdk6.put("public java.lang.String java.lang.Package.getName()", "getName");
        testedDeclaredMethods_jdk6.put("public static java.lang.Package[] java.lang.Package.getPackages()", "getPackages");
        testedDeclaredMethods_jdk6.put("static java.util.Map java.lang.Package.access$000()", "access$000");
        testedDeclaredMethods_jdk6.put("static java.lang.Package java.lang.Package.getSystemPackage(java.lang.String)", "getSystemPackage");
        testedDeclaredMethods_jdk6.put("static java.lang.Package[] java.lang.Package.getSystemPackages()", "getSystemPackages");
        testedDeclaredMethods_jdk6.put("static java.util.Map java.lang.Package.access$200()", "access$200");
        testedDeclaredMethods_jdk6.put("public static java.lang.Package java.lang.Package.getPackage(java.lang.String)", "getPackage");
        testedDeclaredMethods_jdk6.put("static java.lang.Package java.lang.Package.getPackage(java.lang.Class)", "getPackage");
        testedDeclaredMethods_jdk6.put("public java.lang.annotation.Annotation java.lang.Package.getAnnotation(java.lang.Class)", "getAnnotation");
        testedDeclaredMethods_jdk6.put("public boolean java.lang.Package.isAnnotationPresent(java.lang.Class)", "isAnnotationPresent");
        testedDeclaredMethods_jdk6.put("public java.lang.annotation.Annotation[] java.lang.Package.getAnnotations()", "getAnnotations");
        testedDeclaredMethods_jdk6.put("public java.lang.annotation.Annotation[] java.lang.Package.getDeclaredAnnotations()", "getDeclaredAnnotations");
        testedDeclaredMethods_jdk6.put("static java.util.jar.Manifest java.lang.Package.access$100(java.lang.String)", "access$100");
        testedDeclaredMethods_jdk6.put("static java.util.Map java.lang.Package.access$400()", "access$400");
        testedDeclaredMethods_jdk6.put("public boolean java.lang.Package.isSealed()", "isSealed");
        testedDeclaredMethods_jdk6.put("public boolean java.lang.Package.isSealed(java.net.URL)", "isSealed");
        testedDeclaredMethods_jdk6.put("public java.lang.String java.lang.Package.getSpecificationTitle()", "getSpecificationTitle");
        testedDeclaredMethods_jdk6.put("public java.lang.String java.lang.Package.getSpecificationVersion()", "getSpecificationVersion");
        testedDeclaredMethods_jdk6.put("public java.lang.String java.lang.Package.getSpecificationVendor()", "getSpecificationVendor");
        testedDeclaredMethods_jdk6.put("public java.lang.String java.lang.Package.getImplementationTitle()", "getImplementationTitle");
        testedDeclaredMethods_jdk6.put("public java.lang.String java.lang.Package.getImplementationVersion()", "getImplementationVersion");
        testedDeclaredMethods_jdk6.put("public java.lang.String java.lang.Package.getImplementationVendor()", "getImplementationVendor");
        testedDeclaredMethods_jdk6.put("public boolean java.lang.Package.isCompatibleWith(java.lang.String) throws java.lang.NumberFormatException", "isCompatibleWith");
        testedDeclaredMethods_jdk6.put("private java.lang.Class java.lang.Package.getPackageInfo()", "getPackageInfo");
        testedDeclaredMethods_jdk6.put("private static java.lang.Package java.lang.Package.defineSystemPackage(java.lang.String,java.lang.String)", "defineSystemPackage");
        testedDeclaredMethods_jdk6.put("private static java.util.jar.Manifest java.lang.Package.loadManifest(java.lang.String)", "loadManifest");
        testedDeclaredMethods_jdk6.put("private static java.lang.String java.lang.Package.getSystemPackage0(java.lang.String)", "getSystemPackage0");
        testedDeclaredMethods_jdk6.put("private static java.lang.String[] java.lang.Package.getSystemPackages0()", "getSystemPackages0");

        // map for methods declared in (Open)JDK7
        testedDeclaredMethods_jdk7.put("public java.lang.String java.lang.Package.toString()", "toString");
        testedDeclaredMethods_jdk7.put("public int java.lang.Package.hashCode()", "hashCode");
        testedDeclaredMethods_jdk7.put("public java.lang.String java.lang.Package.getName()", "getName");
        testedDeclaredMethods_jdk7.put("static java.util.jar.Manifest java.lang.Package.access$100(java.lang.String)", "access$100");
        testedDeclaredMethods_jdk7.put("public java.lang.annotation.Annotation java.lang.Package.getAnnotation(java.lang.Class)", "getAnnotation");
        testedDeclaredMethods_jdk7.put("public java.lang.annotation.Annotation[] java.lang.Package.getAnnotations()", "getAnnotations");
        testedDeclaredMethods_jdk7.put("public java.lang.annotation.Annotation[] java.lang.Package.getDeclaredAnnotations()", "getDeclaredAnnotations");
        testedDeclaredMethods_jdk7.put("static java.lang.Package java.lang.Package.getPackage(java.lang.Class)", "getPackage");
        testedDeclaredMethods_jdk7.put("public static java.lang.Package java.lang.Package.getPackage(java.lang.String)", "getPackage");
        testedDeclaredMethods_jdk7.put("public boolean java.lang.Package.isAnnotationPresent(java.lang.Class)", "isAnnotationPresent");
        testedDeclaredMethods_jdk7.put("static java.util.Map java.lang.Package.access$000()", "access$000");
        testedDeclaredMethods_jdk7.put("static java.util.Map java.lang.Package.access$200()", "access$200");
        testedDeclaredMethods_jdk7.put("public static java.lang.Package[] java.lang.Package.getPackages()", "getPackages");
        testedDeclaredMethods_jdk7.put("static java.lang.Package java.lang.Package.getSystemPackage(java.lang.String)", "getSystemPackage");
        testedDeclaredMethods_jdk7.put("static java.lang.Package[] java.lang.Package.getSystemPackages()", "getSystemPackages");
        testedDeclaredMethods_jdk7.put("static java.util.Map java.lang.Package.access$400()", "access$400");
        testedDeclaredMethods_jdk7.put("public boolean java.lang.Package.isSealed()", "isSealed");
        testedDeclaredMethods_jdk7.put("public boolean java.lang.Package.isSealed(java.net.URL)", "isSealed");
        testedDeclaredMethods_jdk7.put("public java.lang.String java.lang.Package.getImplementationTitle()", "getImplementationTitle");
        testedDeclaredMethods_jdk7.put("private static java.lang.Package java.lang.Package.defineSystemPackage(java.lang.String,java.lang.String)", "defineSystemPackage");
        testedDeclaredMethods_jdk7.put("public java.lang.String java.lang.Package.getImplementationVendor()", "getImplementationVendor");
        testedDeclaredMethods_jdk7.put("public java.lang.String java.lang.Package.getImplementationVersion()", "getImplementationVersion");
        testedDeclaredMethods_jdk7.put("private java.lang.Class java.lang.Package.getPackageInfo()", "getPackageInfo");
        testedDeclaredMethods_jdk7.put("public java.lang.String java.lang.Package.getSpecificationTitle()", "getSpecificationTitle");
        testedDeclaredMethods_jdk7.put("public java.lang.String java.lang.Package.getSpecificationVendor()", "getSpecificationVendor");
        testedDeclaredMethods_jdk7.put("public java.lang.String java.lang.Package.getSpecificationVersion()", "getSpecificationVersion");
        testedDeclaredMethods_jdk7.put("private static java.lang.String java.lang.Package.getSystemPackage0(java.lang.String)", "getSystemPackage0");
        testedDeclaredMethods_jdk7.put("private static java.lang.String[] java.lang.Package.getSystemPackages0()", "getSystemPackages0");
        testedDeclaredMethods_jdk7.put("public boolean java.lang.Package.isCompatibleWith(java.lang.String) throws java.lang.NumberFormatException", "isCompatibleWith");
        testedDeclaredMethods_jdk7.put("private static java.util.jar.Manifest java.lang.Package.loadManifest(java.lang.String)", "loadManifest");

        // create instance of a class Package
        final Object o = Package.getPackage("java.lang");

        // get a runtime class of an object "o"
        final Class c = o.getClass();

        // get the right map containing method signatures
        testedDeclaredMethods = getJavaVersion() < 7 ? testedDeclaredMethods_jdk6 : testedDeclaredMethods_jdk7;

        // get all declared methods for this class
        java.lang.reflect.Method[] declaredMethods = c.getDeclaredMethods();

        // expected number of declared methods
        final int expectedNumberOfDeclaredMethods = testedDeclaredMethods.size();

        // basic check for a number of declared methods
        harness.check(declaredMethods.length, expectedNumberOfDeclaredMethods);

        // check if all declared methods exist
        for (java.lang.reflect.Method declaredMethod : declaredMethods) {
            // method name should consists of package name + class name
            String methodName = declaredMethod.getName();
            // modifiers + package + method name + parameter types
            String methodString = declaredMethod.toString().replaceAll(" native ", " ");
            harness.check(testedDeclaredMethods.containsKey(methodString));
            harness.check(testedDeclaredMethods.get(methodString), methodName);
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

