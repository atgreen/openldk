// Test of a method java.util.IllegalFormatConversionException.getMessage()

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

package gnu.testlet.java.util.IllegalFormatConversionException;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.IllegalFormatConversionException;



/**
 * Test of a method java.util.IllegalFormatConversionException.getMessage()
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
        IllegalFormatConversionException object1 = new IllegalFormatConversionException('c', Integer.class);
        harness.check(object1 != null);
        harness.check(object1.getMessage() != null);
        harness.check(object1.getMessage().contains("Integer"));

        IllegalFormatConversionException object2 = new IllegalFormatConversionException(' ', Number.class);
        harness.check(object2 != null);
        harness.check(object2.getMessage() != null);
        harness.check(object2.getMessage().contains("Number"));

        IllegalFormatConversionException object3 = new IllegalFormatConversionException('@', Object.class);
        harness.check(object3 != null);
        harness.check(object3.getMessage() != null);
        harness.check(object3.getMessage().contains("Object"));
        harness.check(object3.getMessage().contains("@"));

        IllegalFormatConversionException object4 = new IllegalFormatConversionException('\u1234', Object.class);
        harness.check(object4 != null);
        harness.check(object4.getMessage() != null);
        harness.check(object4.getMessage().contains("Object"));
        harness.check(object4.getMessage().contains("\u1234"));
    }
}

