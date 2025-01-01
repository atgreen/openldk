// Test for instanceof operator applied to a class java.util.IllegalFormatException

// Copyright (C) 2012 Pavel Tisnovsky <ptisnovs@redhat.com>

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

package gnu.testlet.java.util.IllegalFormatException.classInfo;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.IllegalFormatException;
import java.util.DuplicateFormatFlagsException;
import java.util.FormatFlagsConversionMismatchException;
import java.util.IllegalFormatCodePointException;
import java.lang.IllegalArgumentException;
import java.lang.RuntimeException;
import java.lang.Exception;
import java.lang.Throwable;
import java.lang.Object;



/**
 * Test for instanceof operator applied to a class java.util.IllegalFormatException
 */
public class InstanceOf implements Testlet
{

    /**
     * Runs the test using the specified harness.
     *
     * @param harness  the test harness (<code>null</code> not permitted).
     */
    public void test(TestHarness harness)
    {
        check(harness, new DuplicateFormatFlagsException("string"));
        check(harness, new FormatFlagsConversionMismatchException("string", 'c'));
        check(harness, new IllegalFormatCodePointException(42));
    }

    public void check(TestHarness harness, Throwable t)
    {
        // basic check of instanceof operator
        harness.check(t instanceof IllegalFormatException);

        // check operator instanceof against all superclasses
        harness.check(t instanceof IllegalArgumentException);
        harness.check(t instanceof RuntimeException);
        harness.check(t instanceof Exception);
        harness.check(t instanceof Throwable);
        harness.check(t instanceof Object);
    }
}

