// Test for method java.lang.Math.getClass().getMethod()

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
 * Test for method java.lang.Math.getClass().getMethod()
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
        methodsThatShouldExist_jdk6.put("abs", new Class[] {double.class});
        methodsThatShouldExist_jdk6.put("abs", new Class[] {float.class});
        methodsThatShouldExist_jdk6.put("abs", new Class[] {long.class});
        methodsThatShouldExist_jdk6.put("abs", new Class[] {int.class});
        methodsThatShouldExist_jdk6.put("sin", new Class[] {double.class});
        methodsThatShouldExist_jdk6.put("cos", new Class[] {double.class});
        methodsThatShouldExist_jdk6.put("tan", new Class[] {double.class});
        methodsThatShouldExist_jdk6.put("atan2", new Class[] {double.class, double.class});
        methodsThatShouldExist_jdk6.put("sqrt", new Class[] {double.class});
        methodsThatShouldExist_jdk6.put("log", new Class[] {double.class});
        methodsThatShouldExist_jdk6.put("log10", new Class[] {double.class});
        methodsThatShouldExist_jdk6.put("pow", new Class[] {double.class, double.class});
        methodsThatShouldExist_jdk6.put("exp", new Class[] {double.class});
        methodsThatShouldExist_jdk6.put("min", new Class[] {double.class, double.class});
        methodsThatShouldExist_jdk6.put("min", new Class[] {float.class, float.class});
        methodsThatShouldExist_jdk6.put("min", new Class[] {long.class, long.class});
        methodsThatShouldExist_jdk6.put("min", new Class[] {int.class, int.class});
        methodsThatShouldExist_jdk6.put("max", new Class[] {long.class, long.class});
        methodsThatShouldExist_jdk6.put("max", new Class[] {int.class, int.class});
        methodsThatShouldExist_jdk6.put("max", new Class[] {double.class, double.class});
        methodsThatShouldExist_jdk6.put("max", new Class[] {float.class, float.class});
        methodsThatShouldExist_jdk6.put("scalb", new Class[] {double.class, int.class});
        methodsThatShouldExist_jdk6.put("scalb", new Class[] {float.class, int.class});
        methodsThatShouldExist_jdk6.put("getExponent", new Class[] {double.class});
        methodsThatShouldExist_jdk6.put("getExponent", new Class[] {float.class});
        methodsThatShouldExist_jdk6.put("signum", new Class[] {double.class});
        methodsThatShouldExist_jdk6.put("signum", new Class[] {float.class});
        methodsThatShouldExist_jdk6.put("asin", new Class[] {double.class});
        methodsThatShouldExist_jdk6.put("acos", new Class[] {double.class});
        methodsThatShouldExist_jdk6.put("atan", new Class[] {double.class});
        methodsThatShouldExist_jdk6.put("toRadians", new Class[] {double.class});
        methodsThatShouldExist_jdk6.put("toDegrees", new Class[] {double.class});
        methodsThatShouldExist_jdk6.put("cbrt", new Class[] {double.class});
        methodsThatShouldExist_jdk6.put("IEEEremainder", new Class[] {double.class, double.class});
        methodsThatShouldExist_jdk6.put("ceil", new Class[] {double.class});
        methodsThatShouldExist_jdk6.put("floor", new Class[] {double.class});
        methodsThatShouldExist_jdk6.put("rint", new Class[] {double.class});
        methodsThatShouldExist_jdk6.put("round", new Class[] {double.class});
        methodsThatShouldExist_jdk6.put("round", new Class[] {float.class});
        methodsThatShouldExist_jdk6.put("random", new Class[] {});
        methodsThatShouldExist_jdk6.put("ulp", new Class[] {float.class});
        methodsThatShouldExist_jdk6.put("ulp", new Class[] {double.class});
        methodsThatShouldExist_jdk6.put("sinh", new Class[] {double.class});
        methodsThatShouldExist_jdk6.put("cosh", new Class[] {double.class});
        methodsThatShouldExist_jdk6.put("tanh", new Class[] {double.class});
        methodsThatShouldExist_jdk6.put("hypot", new Class[] {double.class, double.class});
        methodsThatShouldExist_jdk6.put("expm1", new Class[] {double.class});
        methodsThatShouldExist_jdk6.put("log1p", new Class[] {double.class});
        methodsThatShouldExist_jdk6.put("copySign", new Class[] {double.class, double.class});
        methodsThatShouldExist_jdk6.put("copySign", new Class[] {float.class, float.class});
        methodsThatShouldExist_jdk6.put("nextAfter", new Class[] {float.class, double.class});
        methodsThatShouldExist_jdk6.put("nextAfter", new Class[] {double.class, double.class});
        methodsThatShouldExist_jdk6.put("nextUp", new Class[] {float.class});
        methodsThatShouldExist_jdk6.put("nextUp", new Class[] {double.class});
        methodsThatShouldExist_jdk6.put("wait", new Class[] {long.class, int.class});
        methodsThatShouldExist_jdk6.put("wait", new Class[] {long.class});
        methodsThatShouldExist_jdk6.put("wait", new Class[] {});
        methodsThatShouldExist_jdk6.put("equals", new Class[] {java.lang.Object.class});
        methodsThatShouldExist_jdk6.put("toString", new Class[] {});
        methodsThatShouldExist_jdk6.put("hashCode", new Class[] {});
        methodsThatShouldExist_jdk6.put("getClass", new Class[] {});
        methodsThatShouldExist_jdk6.put("notify", new Class[] {});
        methodsThatShouldExist_jdk6.put("notifyAll", new Class[] {});

        Map<String, Class[]> methodsThatShouldExist_jdk7 = new HashMap<String, Class[]>();
        methodsThatShouldExist_jdk7.put("abs", new Class[] {double.class});
        methodsThatShouldExist_jdk7.put("abs", new Class[] {float.class});
        methodsThatShouldExist_jdk7.put("abs", new Class[] {long.class});
        methodsThatShouldExist_jdk7.put("abs", new Class[] {int.class});
        methodsThatShouldExist_jdk7.put("sin", new Class[] {double.class});
        methodsThatShouldExist_jdk7.put("cos", new Class[] {double.class});
        methodsThatShouldExist_jdk7.put("tan", new Class[] {double.class});
        methodsThatShouldExist_jdk7.put("atan2", new Class[] {double.class, double.class});
        methodsThatShouldExist_jdk7.put("sqrt", new Class[] {double.class});
        methodsThatShouldExist_jdk7.put("log", new Class[] {double.class});
        methodsThatShouldExist_jdk7.put("log10", new Class[] {double.class});
        methodsThatShouldExist_jdk7.put("pow", new Class[] {double.class, double.class});
        methodsThatShouldExist_jdk7.put("exp", new Class[] {double.class});
        methodsThatShouldExist_jdk7.put("min", new Class[] {double.class, double.class});
        methodsThatShouldExist_jdk7.put("min", new Class[] {float.class, float.class});
        methodsThatShouldExist_jdk7.put("min", new Class[] {long.class, long.class});
        methodsThatShouldExist_jdk7.put("min", new Class[] {int.class, int.class});
        methodsThatShouldExist_jdk7.put("max", new Class[] {long.class, long.class});
        methodsThatShouldExist_jdk7.put("max", new Class[] {int.class, int.class});
        methodsThatShouldExist_jdk7.put("max", new Class[] {double.class, double.class});
        methodsThatShouldExist_jdk7.put("max", new Class[] {float.class, float.class});
        methodsThatShouldExist_jdk7.put("scalb", new Class[] {double.class, int.class});
        methodsThatShouldExist_jdk7.put("scalb", new Class[] {float.class, int.class});
        methodsThatShouldExist_jdk7.put("getExponent", new Class[] {double.class});
        methodsThatShouldExist_jdk7.put("getExponent", new Class[] {float.class});
        methodsThatShouldExist_jdk7.put("signum", new Class[] {float.class});
        methodsThatShouldExist_jdk7.put("signum", new Class[] {double.class});
        methodsThatShouldExist_jdk7.put("IEEEremainder", new Class[] {double.class, double.class});
        methodsThatShouldExist_jdk7.put("acos", new Class[] {double.class});
        methodsThatShouldExist_jdk7.put("asin", new Class[] {double.class});
        methodsThatShouldExist_jdk7.put("atan", new Class[] {double.class});
        methodsThatShouldExist_jdk7.put("cbrt", new Class[] {double.class});
        methodsThatShouldExist_jdk7.put("ceil", new Class[] {double.class});
        methodsThatShouldExist_jdk7.put("copySign", new Class[] {float.class, float.class});
        methodsThatShouldExist_jdk7.put("copySign", new Class[] {double.class, double.class});
        methodsThatShouldExist_jdk7.put("cosh", new Class[] {double.class});
        methodsThatShouldExist_jdk7.put("expm1", new Class[] {double.class});
        methodsThatShouldExist_jdk7.put("floor", new Class[] {double.class});
        methodsThatShouldExist_jdk7.put("hypot", new Class[] {double.class, double.class});
        methodsThatShouldExist_jdk7.put("log1p", new Class[] {double.class});
        methodsThatShouldExist_jdk7.put("nextAfter", new Class[] {float.class, double.class});
        methodsThatShouldExist_jdk7.put("nextAfter", new Class[] {double.class, double.class});
        methodsThatShouldExist_jdk7.put("nextUp", new Class[] {double.class});
        methodsThatShouldExist_jdk7.put("nextUp", new Class[] {float.class});
        methodsThatShouldExist_jdk7.put("random", new Class[] {});
        methodsThatShouldExist_jdk7.put("rint", new Class[] {double.class});
        methodsThatShouldExist_jdk7.put("round", new Class[] {float.class});
        methodsThatShouldExist_jdk7.put("round", new Class[] {double.class});
        methodsThatShouldExist_jdk7.put("sinh", new Class[] {double.class});
        methodsThatShouldExist_jdk7.put("tanh", new Class[] {double.class});
        methodsThatShouldExist_jdk7.put("toDegrees", new Class[] {double.class});
        methodsThatShouldExist_jdk7.put("toRadians", new Class[] {double.class});
        methodsThatShouldExist_jdk7.put("ulp", new Class[] {double.class});
        methodsThatShouldExist_jdk7.put("ulp", new Class[] {float.class});
        methodsThatShouldExist_jdk7.put("wait", new Class[] {long.class, int.class});
        methodsThatShouldExist_jdk7.put("wait", new Class[] {long.class});
        methodsThatShouldExist_jdk7.put("wait", new Class[] {});
        methodsThatShouldExist_jdk7.put("equals", new Class[] {java.lang.Object.class});
        methodsThatShouldExist_jdk7.put("toString", new Class[] {});
        methodsThatShouldExist_jdk7.put("hashCode", new Class[] {});
        methodsThatShouldExist_jdk7.put("getClass", new Class[] {});
        methodsThatShouldExist_jdk7.put("notify", new Class[] {});
        methodsThatShouldExist_jdk7.put("notifyAll", new Class[] {});

        // get the right map containing method signatures
        Map<String, Class[]> methodsThatShouldExist = getJavaVersion() < 7 ? methodsThatShouldExist_jdk6 : methodsThatShouldExist_jdk7;

        // Math.class
        final Class c = Math.class;

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

