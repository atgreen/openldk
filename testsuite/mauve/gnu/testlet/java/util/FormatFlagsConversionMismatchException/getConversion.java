// Test of a method java.util.FormatFlagsConversionMismatchException.getConversion()

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

package gnu.testlet.java.util.FormatFlagsConversionMismatchException;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.FormatFlagsConversionMismatchException;



/**
 * Test of a method java.util.FormatFlagsConversionMismatchException.getConversion()
 */
public class getConversion implements Testlet
{

    /**
     * Runs the test using the specified harness.
     *
     * @param harness  the test harness (<code>null</code> not permitted).
     */
    public void test(TestHarness harness)
    {
        FormatFlagsConversionMismatchException object1 = new FormatFlagsConversionMismatchException("", 'c');
        harness.check(object1 != null);
        harness.check(object1.getConversion() == 'c');

        FormatFlagsConversionMismatchException object2 = new FormatFlagsConversionMismatchException("nothing happens", 'c');
        harness.check(object2 != null);
        harness.check(object2.getConversion() == 'c');

        FormatFlagsConversionMismatchException object3 = new FormatFlagsConversionMismatchException("nothing happens", ' ');
        harness.check(object3 != null);
        harness.check(object3.getConversion() == ' ');

        FormatFlagsConversionMismatchException object4 = new FormatFlagsConversionMismatchException("nothing happens", '\u1234');
        harness.check(object4 != null);
        harness.check(object4.getConversion() == '\u1234');
    }
}


