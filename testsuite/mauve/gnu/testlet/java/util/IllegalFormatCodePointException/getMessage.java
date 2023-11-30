// Test of a method java.util.IllegalFormatCodePointException.getMessage()

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

package gnu.testlet.java.util.IllegalFormatCodePointException;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.IllegalFormatCodePointException;



/**
 * Test of a method java.util.IllegalFormatCodePointException.getMessage()
 */
public class getMessage implements Testlet
{

    /**
     * Runs the test using the specified harness.
     *
     * @param harness  the test harness (<code>null</code> not permitted).
     */
    public void test(TestHarness harness)
    {
        IllegalFormatCodePointException object1 = new IllegalFormatCodePointException(0);
        harness.check(object1 != null);
        harness.check(object1.getMessage() != null);
        harness.check(object1.getMessage().contains("Code point = "));
        harness.check(object1.getMessage().contains("0x0"));

        IllegalFormatCodePointException object2 = new IllegalFormatCodePointException(0x2a);
        harness.check(object2 != null);
        harness.check(object2.getMessage() != null);
        harness.check(object2.getMessage().contains("Code point = "));
        harness.check(object2.getMessage().contains("0x2a"));

        IllegalFormatCodePointException object3 = new IllegalFormatCodePointException(Integer.MAX_VALUE);
        harness.check(object3 != null);
        harness.check(object3.getMessage() != null);
        harness.check(object3.getMessage().contains("Code point = "));
        harness.check(object3.getMessage().contains("0x7fffffff"));

        IllegalFormatCodePointException object4 = new IllegalFormatCodePointException(Integer.MIN_VALUE);
        harness.check(object4 != null);
        harness.check(object4.getMessage() != null);
        harness.check(object4.getMessage().contains("Code point = "));
        harness.check(object4.getMessage().contains("0x80000000"));
    }
}


