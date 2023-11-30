// Test for method java.lang.Math.getClass().getMethods()

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
 * Test for method java.lang.Math.getClass().getMethods()
 */
public class getMethods implements Testlet
{

    /**
     * Runs the test using the specified harness.
     *
     * @param harness  the test harness (<code>null</code> not permitted).
     */
    public void test(TestHarness harness)
    {
        // map of methods which should exists
        Map<String, String> testedMethods = null;

        // map of methods for (Open)JDK6
        Map<String, String> testedMethods_jdk6 = new HashMap<String, String>();

        // map of methods for (Open)JDK7
        Map<String, String> testedMethods_jdk7 = new HashMap<String, String>();

        // map for methods declared in (Open)JDK6
        testedMethods_jdk6.put("public static double java.lang.Math.abs(double)", "abs");
        testedMethods_jdk6.put("public static float java.lang.Math.abs(float)", "abs");
        testedMethods_jdk6.put("public static long java.lang.Math.abs(long)", "abs");
        testedMethods_jdk6.put("public static int java.lang.Math.abs(int)", "abs");
        testedMethods_jdk6.put("public static double java.lang.Math.sin(double)", "sin");
        testedMethods_jdk6.put("public static double java.lang.Math.cos(double)", "cos");
        testedMethods_jdk6.put("public static double java.lang.Math.tan(double)", "tan");
        testedMethods_jdk6.put("public static double java.lang.Math.atan2(double,double)", "atan2");
        testedMethods_jdk6.put("public static double java.lang.Math.sqrt(double)", "sqrt");
        testedMethods_jdk6.put("public static double java.lang.Math.log(double)", "log");
        testedMethods_jdk6.put("public static double java.lang.Math.log10(double)", "log10");
        testedMethods_jdk6.put("public static double java.lang.Math.pow(double,double)", "pow");
        testedMethods_jdk6.put("public static double java.lang.Math.exp(double)", "exp");
        testedMethods_jdk6.put("public static double java.lang.Math.min(double,double)", "min");
        testedMethods_jdk6.put("public static float java.lang.Math.min(float,float)", "min");
        testedMethods_jdk6.put("public static long java.lang.Math.min(long,long)", "min");
        testedMethods_jdk6.put("public static int java.lang.Math.min(int,int)", "min");
        testedMethods_jdk6.put("public static long java.lang.Math.max(long,long)", "max");
        testedMethods_jdk6.put("public static int java.lang.Math.max(int,int)", "max");
        testedMethods_jdk6.put("public static double java.lang.Math.max(double,double)", "max");
        testedMethods_jdk6.put("public static float java.lang.Math.max(float,float)", "max");
        testedMethods_jdk6.put("public static double java.lang.Math.scalb(double,int)", "scalb");
        testedMethods_jdk6.put("public static float java.lang.Math.scalb(float,int)", "scalb");
        testedMethods_jdk6.put("public static int java.lang.Math.getExponent(double)", "getExponent");
        testedMethods_jdk6.put("public static int java.lang.Math.getExponent(float)", "getExponent");
        testedMethods_jdk6.put("public static double java.lang.Math.signum(double)", "signum");
        testedMethods_jdk6.put("public static float java.lang.Math.signum(float)", "signum");
        testedMethods_jdk6.put("public static double java.lang.Math.asin(double)", "asin");
        testedMethods_jdk6.put("public static double java.lang.Math.acos(double)", "acos");
        testedMethods_jdk6.put("public static double java.lang.Math.atan(double)", "atan");
        testedMethods_jdk6.put("public static double java.lang.Math.toRadians(double)", "toRadians");
        testedMethods_jdk6.put("public static double java.lang.Math.toDegrees(double)", "toDegrees");
        testedMethods_jdk6.put("public static double java.lang.Math.cbrt(double)", "cbrt");
        testedMethods_jdk6.put("public static double java.lang.Math.IEEEremainder(double,double)", "IEEEremainder");
        testedMethods_jdk6.put("public static double java.lang.Math.ceil(double)", "ceil");
        testedMethods_jdk6.put("public static double java.lang.Math.floor(double)", "floor");
        testedMethods_jdk6.put("public static double java.lang.Math.rint(double)", "rint");
        testedMethods_jdk6.put("public static long java.lang.Math.round(double)", "round");
        testedMethods_jdk6.put("public static int java.lang.Math.round(float)", "round");
        testedMethods_jdk6.put("public static double java.lang.Math.random()", "random");
        testedMethods_jdk6.put("public static float java.lang.Math.ulp(float)", "ulp");
        testedMethods_jdk6.put("public static double java.lang.Math.ulp(double)", "ulp");
        testedMethods_jdk6.put("public static double java.lang.Math.sinh(double)", "sinh");
        testedMethods_jdk6.put("public static double java.lang.Math.cosh(double)", "cosh");
        testedMethods_jdk6.put("public static double java.lang.Math.tanh(double)", "tanh");
        testedMethods_jdk6.put("public static double java.lang.Math.hypot(double,double)", "hypot");
        testedMethods_jdk6.put("public static double java.lang.Math.expm1(double)", "expm1");
        testedMethods_jdk6.put("public static double java.lang.Math.log1p(double)", "log1p");
        testedMethods_jdk6.put("public static double java.lang.Math.copySign(double,double)", "copySign");
        testedMethods_jdk6.put("public static float java.lang.Math.copySign(float,float)", "copySign");
        testedMethods_jdk6.put("public static float java.lang.Math.nextAfter(float,double)", "nextAfter");
        testedMethods_jdk6.put("public static double java.lang.Math.nextAfter(double,double)", "nextAfter");
        testedMethods_jdk6.put("public static float java.lang.Math.nextUp(float)", "nextUp");
        testedMethods_jdk6.put("public static double java.lang.Math.nextUp(double)", "nextUp");
        testedMethods_jdk6.put("public final void java.lang.Object.wait(long,int) throws java.lang.InterruptedException", "wait");
        testedMethods_jdk6.put("public final void java.lang.Object.wait(long) throws java.lang.InterruptedException", "wait");
        testedMethods_jdk6.put("public final void java.lang.Object.wait() throws java.lang.InterruptedException", "wait");
        testedMethods_jdk6.put("public boolean java.lang.Object.equals(java.lang.Object)", "equals");
        testedMethods_jdk6.put("public java.lang.String java.lang.Object.toString()", "toString");
        testedMethods_jdk6.put("public int java.lang.Object.hashCode()", "hashCode");
        testedMethods_jdk6.put("public final java.lang.Class java.lang.Object.getClass()", "getClass");
        testedMethods_jdk6.put("public final void java.lang.Object.notify()", "notify");
        testedMethods_jdk6.put("public final void java.lang.Object.notifyAll()", "notifyAll");

        // map for methods declared in (Open)JDK7
        testedMethods_jdk7.put("public static double java.lang.Math.abs(double)", "abs");
        testedMethods_jdk7.put("public static float java.lang.Math.abs(float)", "abs");
        testedMethods_jdk7.put("public static long java.lang.Math.abs(long)", "abs");
        testedMethods_jdk7.put("public static int java.lang.Math.abs(int)", "abs");
        testedMethods_jdk7.put("public static double java.lang.Math.sin(double)", "sin");
        testedMethods_jdk7.put("public static double java.lang.Math.cos(double)", "cos");
        testedMethods_jdk7.put("public static double java.lang.Math.tan(double)", "tan");
        testedMethods_jdk7.put("public static double java.lang.Math.atan2(double,double)", "atan2");
        testedMethods_jdk7.put("public static double java.lang.Math.sqrt(double)", "sqrt");
        testedMethods_jdk7.put("public static double java.lang.Math.log(double)", "log");
        testedMethods_jdk7.put("public static double java.lang.Math.log10(double)", "log10");
        testedMethods_jdk7.put("public static double java.lang.Math.pow(double,double)", "pow");
        testedMethods_jdk7.put("public static double java.lang.Math.exp(double)", "exp");
        testedMethods_jdk7.put("public static double java.lang.Math.min(double,double)", "min");
        testedMethods_jdk7.put("public static float java.lang.Math.min(float,float)", "min");
        testedMethods_jdk7.put("public static long java.lang.Math.min(long,long)", "min");
        testedMethods_jdk7.put("public static int java.lang.Math.min(int,int)", "min");
        testedMethods_jdk7.put("public static long java.lang.Math.max(long,long)", "max");
        testedMethods_jdk7.put("public static int java.lang.Math.max(int,int)", "max");
        testedMethods_jdk7.put("public static double java.lang.Math.max(double,double)", "max");
        testedMethods_jdk7.put("public static float java.lang.Math.max(float,float)", "max");
        testedMethods_jdk7.put("public static double java.lang.Math.scalb(double,int)", "scalb");
        testedMethods_jdk7.put("public static float java.lang.Math.scalb(float,int)", "scalb");
        testedMethods_jdk7.put("public static int java.lang.Math.getExponent(double)", "getExponent");
        testedMethods_jdk7.put("public static int java.lang.Math.getExponent(float)", "getExponent");
        testedMethods_jdk7.put("public static float java.lang.Math.signum(float)", "signum");
        testedMethods_jdk7.put("public static double java.lang.Math.signum(double)", "signum");
        testedMethods_jdk7.put("public static double java.lang.Math.IEEEremainder(double,double)", "IEEEremainder");
        testedMethods_jdk7.put("public static double java.lang.Math.acos(double)", "acos");
        testedMethods_jdk7.put("public static double java.lang.Math.asin(double)", "asin");
        testedMethods_jdk7.put("public static double java.lang.Math.atan(double)", "atan");
        testedMethods_jdk7.put("public static double java.lang.Math.cbrt(double)", "cbrt");
        testedMethods_jdk7.put("public static double java.lang.Math.ceil(double)", "ceil");
        testedMethods_jdk7.put("public static float java.lang.Math.copySign(float,float)", "copySign");
        testedMethods_jdk7.put("public static double java.lang.Math.copySign(double,double)", "copySign");
        testedMethods_jdk7.put("public static double java.lang.Math.cosh(double)", "cosh");
        testedMethods_jdk7.put("public static double java.lang.Math.expm1(double)", "expm1");
        testedMethods_jdk7.put("public static double java.lang.Math.floor(double)", "floor");
        testedMethods_jdk7.put("public static double java.lang.Math.hypot(double,double)", "hypot");
        testedMethods_jdk7.put("public static double java.lang.Math.log1p(double)", "log1p");
        testedMethods_jdk7.put("public static float java.lang.Math.nextAfter(float,double)", "nextAfter");
        testedMethods_jdk7.put("public static double java.lang.Math.nextAfter(double,double)", "nextAfter");
        testedMethods_jdk7.put("public static double java.lang.Math.nextUp(double)", "nextUp");
        testedMethods_jdk7.put("public static float java.lang.Math.nextUp(float)", "nextUp");
        testedMethods_jdk7.put("public static double java.lang.Math.random()", "random");
        testedMethods_jdk7.put("public static double java.lang.Math.rint(double)", "rint");
        testedMethods_jdk7.put("public static int java.lang.Math.round(float)", "round");
        testedMethods_jdk7.put("public static long java.lang.Math.round(double)", "round");
        testedMethods_jdk7.put("public static double java.lang.Math.sinh(double)", "sinh");
        testedMethods_jdk7.put("public static double java.lang.Math.tanh(double)", "tanh");
        testedMethods_jdk7.put("public static double java.lang.Math.toDegrees(double)", "toDegrees");
        testedMethods_jdk7.put("public static double java.lang.Math.toRadians(double)", "toRadians");
        testedMethods_jdk7.put("public static double java.lang.Math.ulp(double)", "ulp");
        testedMethods_jdk7.put("public static float java.lang.Math.ulp(float)", "ulp");
        testedMethods_jdk7.put("public final void java.lang.Object.wait(long,int) throws java.lang.InterruptedException", "wait");
        testedMethods_jdk7.put("public final void java.lang.Object.wait(long) throws java.lang.InterruptedException", "wait");
        testedMethods_jdk7.put("public final void java.lang.Object.wait() throws java.lang.InterruptedException", "wait");
        testedMethods_jdk7.put("public boolean java.lang.Object.equals(java.lang.Object)", "equals");
        testedMethods_jdk7.put("public java.lang.String java.lang.Object.toString()", "toString");
        testedMethods_jdk7.put("public int java.lang.Object.hashCode()", "hashCode");
        testedMethods_jdk7.put("public final java.lang.Class java.lang.Object.getClass()", "getClass");
        testedMethods_jdk7.put("public final void java.lang.Object.notify()", "notify");
        testedMethods_jdk7.put("public final void java.lang.Object.notifyAll()", "notifyAll");

        // Math.class
        final Class c = Math.class;

        // get the right map containing method signatures
        testedMethods = getJavaVersion() < 7 ? testedMethods_jdk6 : testedMethods_jdk7;

        // get all methods for this class
        java.lang.reflect.Method[] methods = c.getMethods();

        // expected number of methods
        final int expectedNumberOfMethods = testedMethods.size();

        // basic check for a number of methods
        harness.check(methods.length, expectedNumberOfMethods);

        // check if all methods exist
        for (java.lang.reflect.Method method : methods) {
            // method name should consists of package name + class name
            String methodName = method.getName();
            // modifiers + package + method name + parameter types
            String methodString = method.toString().replaceAll(" native ", " ");
            harness.check(testedMethods.containsKey(methodString));
            harness.check(testedMethods.get(methodString), methodName);
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

