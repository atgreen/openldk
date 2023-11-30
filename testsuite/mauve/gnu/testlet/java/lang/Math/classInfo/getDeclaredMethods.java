// Test for method java.lang.Math.getClass().getDeclaredMethods()

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

package gnu.testlet.java.lang.Math.classInfo;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.lang.Math;
import java.util.Map;
import java.util.HashMap;



/**
 * Test for method java.lang.Math.getClass().getDeclaredMethods()
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
        testedDeclaredMethods_jdk6.put("public static double java.lang.Math.abs(double)", "abs");
        testedDeclaredMethods_jdk6.put("public static float java.lang.Math.abs(float)", "abs");
        testedDeclaredMethods_jdk6.put("public static long java.lang.Math.abs(long)", "abs");
        testedDeclaredMethods_jdk6.put("public static int java.lang.Math.abs(int)", "abs");
        testedDeclaredMethods_jdk6.put("public static double java.lang.Math.sin(double)", "sin");
        testedDeclaredMethods_jdk6.put("public static double java.lang.Math.cos(double)", "cos");
        testedDeclaredMethods_jdk6.put("public static double java.lang.Math.tan(double)", "tan");
        testedDeclaredMethods_jdk6.put("public static double java.lang.Math.atan2(double,double)", "atan2");
        testedDeclaredMethods_jdk6.put("public static double java.lang.Math.sqrt(double)", "sqrt");
        testedDeclaredMethods_jdk6.put("public static double java.lang.Math.log(double)", "log");
        testedDeclaredMethods_jdk6.put("public static double java.lang.Math.log10(double)", "log10");
        testedDeclaredMethods_jdk6.put("public static double java.lang.Math.pow(double,double)", "pow");
        testedDeclaredMethods_jdk6.put("public static double java.lang.Math.exp(double)", "exp");
        testedDeclaredMethods_jdk6.put("public static double java.lang.Math.min(double,double)", "min");
        testedDeclaredMethods_jdk6.put("public static float java.lang.Math.min(float,float)", "min");
        testedDeclaredMethods_jdk6.put("public static long java.lang.Math.min(long,long)", "min");
        testedDeclaredMethods_jdk6.put("public static int java.lang.Math.min(int,int)", "min");
        testedDeclaredMethods_jdk6.put("public static long java.lang.Math.max(long,long)", "max");
        testedDeclaredMethods_jdk6.put("public static int java.lang.Math.max(int,int)", "max");
        testedDeclaredMethods_jdk6.put("public static double java.lang.Math.max(double,double)", "max");
        testedDeclaredMethods_jdk6.put("public static float java.lang.Math.max(float,float)", "max");
        testedDeclaredMethods_jdk6.put("public static double java.lang.Math.scalb(double,int)", "scalb");
        testedDeclaredMethods_jdk6.put("public static float java.lang.Math.scalb(float,int)", "scalb");
        testedDeclaredMethods_jdk6.put("public static int java.lang.Math.getExponent(double)", "getExponent");
        testedDeclaredMethods_jdk6.put("public static int java.lang.Math.getExponent(float)", "getExponent");
        testedDeclaredMethods_jdk6.put("public static double java.lang.Math.signum(double)", "signum");
        testedDeclaredMethods_jdk6.put("public static float java.lang.Math.signum(float)", "signum");
        testedDeclaredMethods_jdk6.put("public static double java.lang.Math.asin(double)", "asin");
        testedDeclaredMethods_jdk6.put("public static double java.lang.Math.acos(double)", "acos");
        testedDeclaredMethods_jdk6.put("public static double java.lang.Math.atan(double)", "atan");
        testedDeclaredMethods_jdk6.put("public static double java.lang.Math.toRadians(double)", "toRadians");
        testedDeclaredMethods_jdk6.put("public static double java.lang.Math.toDegrees(double)", "toDegrees");
        testedDeclaredMethods_jdk6.put("public static double java.lang.Math.cbrt(double)", "cbrt");
        testedDeclaredMethods_jdk6.put("public static double java.lang.Math.IEEEremainder(double,double)", "IEEEremainder");
        testedDeclaredMethods_jdk6.put("public static double java.lang.Math.ceil(double)", "ceil");
        testedDeclaredMethods_jdk6.put("public static double java.lang.Math.floor(double)", "floor");
        testedDeclaredMethods_jdk6.put("public static double java.lang.Math.rint(double)", "rint");
        testedDeclaredMethods_jdk6.put("public static long java.lang.Math.round(double)", "round");
        testedDeclaredMethods_jdk6.put("public static int java.lang.Math.round(float)", "round");
        testedDeclaredMethods_jdk6.put("private static synchronized void java.lang.Math.initRNG()", "initRNG");
        testedDeclaredMethods_jdk6.put("public static double java.lang.Math.random()", "random");
        testedDeclaredMethods_jdk6.put("public static float java.lang.Math.ulp(float)", "ulp");
        testedDeclaredMethods_jdk6.put("public static double java.lang.Math.ulp(double)", "ulp");
        testedDeclaredMethods_jdk6.put("public static double java.lang.Math.sinh(double)", "sinh");
        testedDeclaredMethods_jdk6.put("public static double java.lang.Math.cosh(double)", "cosh");
        testedDeclaredMethods_jdk6.put("public static double java.lang.Math.tanh(double)", "tanh");
        testedDeclaredMethods_jdk6.put("public static double java.lang.Math.hypot(double,double)", "hypot");
        testedDeclaredMethods_jdk6.put("public static double java.lang.Math.expm1(double)", "expm1");
        testedDeclaredMethods_jdk6.put("public static double java.lang.Math.log1p(double)", "log1p");
        testedDeclaredMethods_jdk6.put("public static double java.lang.Math.copySign(double,double)", "copySign");
        testedDeclaredMethods_jdk6.put("public static float java.lang.Math.copySign(float,float)", "copySign");
        testedDeclaredMethods_jdk6.put("public static float java.lang.Math.nextAfter(float,double)", "nextAfter");
        testedDeclaredMethods_jdk6.put("public static double java.lang.Math.nextAfter(double,double)", "nextAfter");
        testedDeclaredMethods_jdk6.put("public static float java.lang.Math.nextUp(float)", "nextUp");
        testedDeclaredMethods_jdk6.put("public static double java.lang.Math.nextUp(double)", "nextUp");

        // map for methods declared in (Open)JDK7
        testedDeclaredMethods_jdk7.put("public static double java.lang.Math.abs(double)", "abs");
        testedDeclaredMethods_jdk7.put("public static float java.lang.Math.abs(float)", "abs");
        testedDeclaredMethods_jdk7.put("public static long java.lang.Math.abs(long)", "abs");
        testedDeclaredMethods_jdk7.put("public static int java.lang.Math.abs(int)", "abs");
        testedDeclaredMethods_jdk7.put("public static double java.lang.Math.sin(double)", "sin");
        testedDeclaredMethods_jdk7.put("public static double java.lang.Math.cos(double)", "cos");
        testedDeclaredMethods_jdk7.put("public static double java.lang.Math.tan(double)", "tan");
        testedDeclaredMethods_jdk7.put("public static double java.lang.Math.atan2(double,double)", "atan2");
        testedDeclaredMethods_jdk7.put("public static double java.lang.Math.sqrt(double)", "sqrt");
        testedDeclaredMethods_jdk7.put("public static double java.lang.Math.log(double)", "log");
        testedDeclaredMethods_jdk7.put("public static double java.lang.Math.log10(double)", "log10");
        testedDeclaredMethods_jdk7.put("public static double java.lang.Math.pow(double,double)", "pow");
        testedDeclaredMethods_jdk7.put("public static double java.lang.Math.exp(double)", "exp");
        testedDeclaredMethods_jdk7.put("public static double java.lang.Math.min(double,double)", "min");
        testedDeclaredMethods_jdk7.put("public static float java.lang.Math.min(float,float)", "min");
        testedDeclaredMethods_jdk7.put("public static long java.lang.Math.min(long,long)", "min");
        testedDeclaredMethods_jdk7.put("public static int java.lang.Math.min(int,int)", "min");
        testedDeclaredMethods_jdk7.put("public static long java.lang.Math.max(long,long)", "max");
        testedDeclaredMethods_jdk7.put("public static int java.lang.Math.max(int,int)", "max");
        testedDeclaredMethods_jdk7.put("public static double java.lang.Math.max(double,double)", "max");
        testedDeclaredMethods_jdk7.put("public static float java.lang.Math.max(float,float)", "max");
        testedDeclaredMethods_jdk7.put("public static double java.lang.Math.scalb(double,int)", "scalb");
        testedDeclaredMethods_jdk7.put("public static float java.lang.Math.scalb(float,int)", "scalb");
        testedDeclaredMethods_jdk7.put("public static int java.lang.Math.getExponent(double)", "getExponent");
        testedDeclaredMethods_jdk7.put("public static int java.lang.Math.getExponent(float)", "getExponent");
        testedDeclaredMethods_jdk7.put("public static float java.lang.Math.signum(float)", "signum");
        testedDeclaredMethods_jdk7.put("public static double java.lang.Math.signum(double)", "signum");
        testedDeclaredMethods_jdk7.put("public static double java.lang.Math.IEEEremainder(double,double)", "IEEEremainder");
        testedDeclaredMethods_jdk7.put("public static double java.lang.Math.acos(double)", "acos");
        testedDeclaredMethods_jdk7.put("public static double java.lang.Math.asin(double)", "asin");
        testedDeclaredMethods_jdk7.put("public static double java.lang.Math.atan(double)", "atan");
        testedDeclaredMethods_jdk7.put("public static double java.lang.Math.cbrt(double)", "cbrt");
        testedDeclaredMethods_jdk7.put("public static double java.lang.Math.ceil(double)", "ceil");
        testedDeclaredMethods_jdk7.put("public static float java.lang.Math.copySign(float,float)", "copySign");
        testedDeclaredMethods_jdk7.put("public static double java.lang.Math.copySign(double,double)", "copySign");
        testedDeclaredMethods_jdk7.put("public static double java.lang.Math.cosh(double)", "cosh");
        testedDeclaredMethods_jdk7.put("public static double java.lang.Math.expm1(double)", "expm1");
        testedDeclaredMethods_jdk7.put("public static double java.lang.Math.floor(double)", "floor");
        testedDeclaredMethods_jdk7.put("public static double java.lang.Math.hypot(double,double)", "hypot");
        testedDeclaredMethods_jdk7.put("private static synchronized java.util.Random java.lang.Math.initRNG()", "initRNG");
        testedDeclaredMethods_jdk7.put("public static double java.lang.Math.log1p(double)", "log1p");
        testedDeclaredMethods_jdk7.put("public static float java.lang.Math.nextAfter(float,double)", "nextAfter");
        testedDeclaredMethods_jdk7.put("public static double java.lang.Math.nextAfter(double,double)", "nextAfter");
        testedDeclaredMethods_jdk7.put("public static double java.lang.Math.nextUp(double)", "nextUp");
        testedDeclaredMethods_jdk7.put("public static float java.lang.Math.nextUp(float)", "nextUp");
        testedDeclaredMethods_jdk7.put("public static double java.lang.Math.random()", "random");
        testedDeclaredMethods_jdk7.put("public static double java.lang.Math.rint(double)", "rint");
        testedDeclaredMethods_jdk7.put("public static int java.lang.Math.round(float)", "round");
        testedDeclaredMethods_jdk7.put("public static long java.lang.Math.round(double)", "round");
        testedDeclaredMethods_jdk7.put("public static double java.lang.Math.sinh(double)", "sinh");
        testedDeclaredMethods_jdk7.put("public static double java.lang.Math.tanh(double)", "tanh");
        testedDeclaredMethods_jdk7.put("public static double java.lang.Math.toDegrees(double)", "toDegrees");
        testedDeclaredMethods_jdk7.put("public static double java.lang.Math.toRadians(double)", "toRadians");
        testedDeclaredMethods_jdk7.put("public static double java.lang.Math.ulp(double)", "ulp");
        testedDeclaredMethods_jdk7.put("public static float java.lang.Math.ulp(float)", "ulp");

        // get a runtime class of an Math
        final Class c = Math.class;

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

