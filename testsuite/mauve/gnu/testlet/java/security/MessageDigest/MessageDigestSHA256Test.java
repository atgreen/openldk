// Test of SHA256 digest algorithm.

// Copyright (C) 2013, 2014 Pavel Tisnovsky <ptisnovs@redhat.com>

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
 * Test of SHA256 digest algorithm.
 */
public class MessageDigestSHA256Test implements Testlet {

    /**
     * Hash for the text "".
     *
     * This hash was generated using SHA-256 algorithm.
     */
    private static final byte[] EXPECTED_HASH_1 = new byte[] {
        (byte)0xe3, (byte)0xb0, (byte)0xc4, (byte)0x42, (byte)0x98, (byte)0xfc, (byte)0x1c, (byte)0x14, 
        (byte)0x9a, (byte)0xfb, (byte)0xf4, (byte)0xc8, (byte)0x99, (byte)0x6f, (byte)0xb9, (byte)0x24, 
        (byte)0x27, (byte)0xae, (byte)0x41, (byte)0xe4, (byte)0x64, (byte)0x9b, (byte)0x93, (byte)0x4c, 
        (byte)0xa4, (byte)0x95, (byte)0x99, (byte)0x1b, (byte)0x78, (byte)0x52, (byte)0xb8, (byte)0x55, 
    };

    /**
     * Hash for the text "\0".
     *
     * This hash was generated using SHA-256 algorithm.
     */
    private static final byte[] EXPECTED_HASH_2 = new byte[] {
        (byte)0x6e, (byte)0x34, (byte)0x0b, (byte)0x9c, (byte)0xff, (byte)0xb3, (byte)0x7a, (byte)0x98, 
        (byte)0x9c, (byte)0xa5, (byte)0x44, (byte)0xe6, (byte)0xbb, (byte)0x78, (byte)0x0a, (byte)0x2c, 
        (byte)0x78, (byte)0x90, (byte)0x1d, (byte)0x3f, (byte)0xb3, (byte)0x37, (byte)0x38, (byte)0x76, 
        (byte)0x85, (byte)0x11, (byte)0xa3, (byte)0x06, (byte)0x17, (byte)0xaf, (byte)0xa0, (byte)0x1d, 
    };

    /**
     * Hash for the text " ".
     *
     * This hash was generated using SHA-256 algorithm.
     */
    private static final byte[] EXPECTED_HASH_3 = new byte[] {
        (byte)0x36, (byte)0xa9, (byte)0xe7, (byte)0xf1, (byte)0xc9, (byte)0x5b, (byte)0x82, (byte)0xff, 
        (byte)0xb9, (byte)0x97, (byte)0x43, (byte)0xe0, (byte)0xc5, (byte)0xc4, (byte)0xce, (byte)0x95, 
        (byte)0xd8, (byte)0x3c, (byte)0x9a, (byte)0x43, (byte)0x0a, (byte)0xac, (byte)0x59, (byte)0xf8, 
        (byte)0x4e, (byte)0xf3, (byte)0xcb, (byte)0xfa, (byte)0xb6, (byte)0x14, (byte)0x50, (byte)0x68, 
    };

    /**
     * Hash for the text "a".
     *
     * This hash was generated using SHA-256 algorithm.
     */
    private static final byte[] EXPECTED_HASH_4 = new byte[] {
        (byte)0xca, (byte)0x97, (byte)0x81, (byte)0x12, (byte)0xca, (byte)0x1b, (byte)0xbd, (byte)0xca, 
        (byte)0xfa, (byte)0xc2, (byte)0x31, (byte)0xb3, (byte)0x9a, (byte)0x23, (byte)0xdc, (byte)0x4d, 
        (byte)0xa7, (byte)0x86, (byte)0xef, (byte)0xf8, (byte)0x14, (byte)0x7c, (byte)0x4e, (byte)0x72, 
        (byte)0xb9, (byte)0x80, (byte)0x77, (byte)0x85, (byte)0xaf, (byte)0xee, (byte)0x48, (byte)0xbb, 
    };

    /**
     * Hash for the text "text".
     *
     * This hash was generated using SHA-256 algorithm.
     */
    private static final byte[] EXPECTED_HASH_5 = new byte[] {
        (byte)0x98, (byte)0x2d, (byte)0x9e, (byte)0x3e, (byte)0xb9, (byte)0x96, (byte)0xf5, (byte)0x59, 
        (byte)0xe6, (byte)0x33, (byte)0xf4, (byte)0xd1, (byte)0x94, (byte)0xde, (byte)0xf3, (byte)0x76, 
        (byte)0x1d, (byte)0x90, (byte)0x9f, (byte)0x5a, (byte)0x3b, (byte)0x64, (byte)0x7d, (byte)0x1a, 
        (byte)0x85, (byte)0x1f, (byte)0xea, (byte)0xd6, (byte)0x7c, (byte)0x32, (byte)0xc9, (byte)0xd1, 
    };

    /**
     * Hash for the text "Hello world!".
     *
     * This hash was generated using SHA-256 algorithm.
     */
    private static final byte[] EXPECTED_HASH_6 = new byte[] {
        (byte)0xc0, (byte)0x53, (byte)0x5e, (byte)0x4b, (byte)0xe2, (byte)0xb7, (byte)0x9f, (byte)0xfd, 
        (byte)0x93, (byte)0x29, (byte)0x13, (byte)0x05, (byte)0x43, (byte)0x6b, (byte)0xf8, (byte)0x89, 
        (byte)0x31, (byte)0x4e, (byte)0x4a, (byte)0x3f, (byte)0xae, (byte)0xc0, (byte)0x5e, (byte)0xcf, 
        (byte)0xfc, (byte)0xbb, (byte)0x7d, (byte)0xf3, (byte)0x1a, (byte)0xd9, (byte)0xe5, (byte)0x1a, 
    };

    /**
     * Hash for the text "Even longer text...".
     *
     * This hash was generated using SHA-256 algorithm.
     */
    private static final byte[] EXPECTED_HASH_7 = new byte[] {
        (byte)0x15, (byte)0xe9, (byte)0xc6, (byte)0x0b, (byte)0x45, (byte)0x3d, (byte)0x0f, (byte)0x1f, 
        (byte)0x92, (byte)0x0b, (byte)0xb7, (byte)0x14, (byte)0xf4, (byte)0x61, (byte)0x05, (byte)0x8b, 
        (byte)0x14, (byte)0x19, (byte)0x1b, (byte)0xb7, (byte)0xfe, (byte)0xe9, (byte)0xd3, (byte)0x13, 
        (byte)0x11, (byte)0xc7, (byte)0x81, (byte)0xff, (byte)0x7f, (byte)0x4a, (byte)0xa6, (byte)0x98, 
    };

    /**
     * Hash for the text "Lorem ipsum dolor sit amet, consectetur adipisicing
     * elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
     * Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi
     * ut aliquip ex ea commodo consequat. Duis aute irure dolor in
     * reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla
     * pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa
     * qui officia deserunt mollit anim id est laborum.".
     *
     * This hash was generated using SHA-256 algorithm.
     */
    private static final byte[] EXPECTED_HASH_8 = new byte[] {
        (byte)0x2c, (byte)0x7c, (byte)0x3d, (byte)0x5f, (byte)0x24, (byte)0x4f, (byte)0x1a, (byte)0x40, 
        (byte)0x06, (byte)0x9a, (byte)0x32, (byte)0x22, (byte)0x42, (byte)0x15, (byte)0xe0, (byte)0xcf, 
        (byte)0x9b, (byte)0x42, (byte)0x48, (byte)0x5c, (byte)0x99, (byte)0xd8, (byte)0x0f, (byte)0x35, 
        (byte)0x7d, (byte)0x76, (byte)0xf0, (byte)0x06, (byte)0x35, (byte)0x9c, (byte)0x7a, (byte)0x18, 
    };

    /**
     * Generate hash for given text using the specified hash algorithm.
     */
    private static byte[] generateHash(TestHarness harness, String algorithmName, String text) throws NoSuchAlgorithmException {
        MessageDigest md = MessageDigest.getInstance(algorithmName);
        harness.check(md != null);
        md.update(text.getBytes());
        byte[] digest = md.digest();
        harness.check(digest != null);
        harness.check(digest.length, 32);
        return digest;
    }

    /**
     * Compare digest (hash) with the expected result.
     */
    private void dotest(TestHarness harness, String algorithmName, byte[] expectedHash, String text) throws NoSuchAlgorithmException {
        byte[] digest;
        digest = generateHash(harness, algorithmName, text);
        for (int i = 0; i < digest.length; i++) {
            if (digest[i] != expectedHash[i]) {
                harness.fail("Difference found at offset " + i);
            }
        }
    }

    private void run(TestHarness harness) throws NoSuchAlgorithmException {
        dotest(harness, "SHA-256", EXPECTED_HASH_1, "");
        dotest(harness, "SHA-256", EXPECTED_HASH_2, "\0");
        dotest(harness, "SHA-256", EXPECTED_HASH_3, " ");
        dotest(harness, "SHA-256", EXPECTED_HASH_4, "a");
        dotest(harness, "SHA-256", EXPECTED_HASH_5, "text");
        dotest(harness, "SHA-256", EXPECTED_HASH_6, "Hello world!");
        dotest(harness, "SHA-256", EXPECTED_HASH_7, "Even longer text...");
        dotest(harness, "SHA-256", EXPECTED_HASH_8, "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.");
    }

    /**
     * Runs the test using the specified harness.
     *
     * @param harness  the test harness (<code>null</code> not permitted).
     */
    public void test(TestHarness harness) {
        if (!isSHA256Available()) {
            return;
        }
        try {
            run(harness);
        }
        catch (NoSuchAlgorithmException e) {
            // algorithm implementation can't be found
            harness.fail("Fail for algorithm SHA256.");
            harness.debug(e);
        }
    }

    public boolean isSHA256Available() {
        try {
            MessageDigest.getInstance("SHA-256");
        }
        catch (NoSuchAlgorithmException e) {
            return false;
        }
        return true;
    }

    /**
     * @param args
     * @throws NoSuchAlgorithmException 
     */
    public static void main(String[] args) {
        try {
            new MessageDigestSHA256Test().run(null);
            System.out.println("OK");
        }
        catch (NoSuchAlgorithmException e) {
        }
    }
}

