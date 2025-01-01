// Test for the following requirement:
// Every implementation of the Java platform is required to support the following standard MessageDigest algorithms:
// MD5
// SHA-1
// SHA-256

// Copyright (C) 2013 Pavel Tisnovsky <ptisnovs@redhat.com>

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

package gnu.testlet.java.security.MessageDigest;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;



/**
 * Test for the following requirement:
 * Every implementation of the Java platform is required to support the following standard MessageDigest algorithms:
 * MD5
 * SHA-1
 * SHA-256
 */
public class BasicMessageDigestAlgorithms implements Testlet {

    /**
     * Runs the test using the specified harness.
     *
     * @param harness  the test harness (<code>null</code> not permitted).
     */
    public void test(TestHarness harness) {
        String[] algorithmsNames = new String[]
            { "MD5", "SHA-1", "SHA-256" };
        for (String algorithmName : algorithmsNames) {
            try {
                // try to get an instance from any provider for the specified algorithm
                MessageDigest md = MessageDigest.getInstance(algorithmName);
                // just to be sure...
                harness.check(md != null);
            }
            catch (NoSuchAlgorithmException e) {
                // algorithm implementation can't be found
                harness.fail("Fail for algorithm: " + algorithmName);
                harness.debug(e);
            }
        }
    }

}

