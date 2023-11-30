// Test the basic usage of RC4 crypto algorithm.

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

package gnu.testlet.javax.crypto.Cipher;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.spec.InvalidKeySpecException;



import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;



/**
 * Test the basic usage of RC4 crypto algorithm.
 * 
 * @author Pavel Tisnovsky (ptisnovs@redhat.com)
 */
public class RC4CipherTest {

    /**
     * Text which should be crypted into byte stream stored in EXPECTED_CIPHER_TEXT_1.
     */
    static final private String ORIGINAL_TEXT_1 = "";

    /**
     * Text which should be crypted into byte stream stored in EXPECTED_CIPHER_TEXT_2.
     */
    static final private String ORIGINAL_TEXT_2 = "\0";

    /**
     * Text which should be crypted into byte stream stored in EXPECTED_CIPHER_TEXT_3.
     */
    static final private String ORIGINAL_TEXT_3 = " ";

    /**
     * Text which should be crypted into byte stream stored in EXPECTED_CIPHER_TEXT_4.
     */
    static final private String ORIGINAL_TEXT_4 = "a";

    /**
     * Text which should be crypted into byte stream stored in EXPECTED_CIPHER_TEXT_5.
     */
    static final private String ORIGINAL_TEXT_5 = "Hello World!";

    /**
     * Text which should be crypted into byte stream stored in EXPECTED_CIPHER_TEXT_6.
     */
    static final private String ORIGINAL_TEXT_6 = "The quick brown fox jumps over the lazy dog";

    /**
     * Text which should be crypted into byte stream stored in EXPECTED_CIPHER_TEXT_7.
     */
    static final private String ORIGINAL_TEXT_7 = "Lorem ipsum dolor sit amet, consectetur " +
                                                  "adipisicing elit, sed do eiusmod tempor " +
                                                  "incididunt ut labore et dolore magna aliqua. " +
                                                  "Ut enim ad minim veniam, quis nostrud " +
                                                  "exercitation ullamco laboris nisi ut aliquip " +
                                                  "ex ea commodo consequat. Duis aute irure " +
                                                  "dolor in reprehenderit in voluptate velit " +
                                                  "esse cillum dolore eu fugiat nulla pariatur. " +
                                                  "Excepteur sint occaecat cupidatat non proident, " +
                                                  "sunt in culpa qui officia deserunt mollit anim " +
                                                  "id est laborum.";

    /**
     * Byte stream for the original text ORIGINAL_TEXT_1.
     */
    static final private byte[] EXPECTED_CIPHER_TEXT_1 = new byte[] {
        // empty!
    };

    /**
     * Byte stream for the original text ORIGINAL_TEXT_2.
     */
    static final private byte[] EXPECTED_CIPHER_TEXT_2 = new byte[] {
        (byte)0x95, 
    };

    /**
     * Byte stream for the original text ORIGINAL_TEXT_3.
     */
    static final private byte[] EXPECTED_CIPHER_TEXT_3 = new byte[] {
        (byte)0xb5, 
    };

    /**
     * Byte stream for the original text ORIGINAL_TEXT_4.
     */
    static final private byte[] EXPECTED_CIPHER_TEXT_4 = new byte[] {
        (byte)0xf4, 
    };

    /**
     * Byte stream for the original text ORIGINAL_TEXT_5.
     */
    static final private byte[] EXPECTED_CIPHER_TEXT_5 = new byte[] {
        (byte)0xdd, (byte)0x7e, (byte)0x34, (byte)0x72, (byte)0x6f, (byte)0x5e, (byte)0x22, (byte)0x1d, 
        (byte)0x60, (byte)0x3e, (byte)0x52, (byte)0x1b, 
    };

    /**
     * Byte stream for the original text ORIGINAL_TEXT_6.
     */
    static final private byte[] EXPECTED_CIPHER_TEXT_6 = new byte[] {
        (byte)0xc1, (byte)0x73, (byte)0x3d, (byte)0x3e, (byte)0x71, (byte)0x0b, (byte)0x1c, (byte)0x11, 
        (byte)0x79, (byte)0x72, (byte)0x54, (byte)0x48, (byte)0x88, (byte)0x51, (byte)0xc7, (byte)0x8c, 
        (byte)0x49, (byte)0x87, (byte)0x97, (byte)0x90, (byte)0x7e, (byte)0x2b, (byte)0xeb, (byte)0x1f, 
        (byte)0x8e, (byte)0x7c, (byte)0x6f, (byte)0x63, (byte)0x25, (byte)0x16, (byte)0x0c, (byte)0xb8, 
        (byte)0xc2, (byte)0x27, (byte)0x19, (byte)0xd9, (byte)0x5f, (byte)0x0f, (byte)0x91, (byte)0x62, 
        (byte)0xa8, (byte)0x27, (byte)0x41, 
    };

    /**
     * Byte stream for the original text ORIGINAL_TEXT_7.
     */
    static final private byte[] EXPECTED_CIPHER_TEXT_7 = new byte[] {
        (byte)0xd9, (byte)0x74, (byte)0x2a, (byte)0x7b, (byte)0x6d, (byte)0x5e, (byte)0x1c, (byte)0x02, 
        (byte)0x61, (byte)0x27, (byte)0x5b, (byte)0x1a, (byte)0x83, (byte)0x49, (byte)0xc5, (byte)0xc3, 
        (byte)0x5d, (byte)0xc8, (byte)0x9c, (byte)0xd9, (byte)0x60, (byte)0x7e, (byte)0xe7, (byte)0x02, 
        (byte)0x98, (byte)0x28, (byte)0x2c, (byte)0x35, (byte)0x23, (byte)0x0b, (byte)0x42, (byte)0xbf, 
        (byte)0xcf, (byte)0x21, (byte)0x4d, (byte)0xd0, (byte)0x4a, (byte)0x00, (byte)0x9a, (byte)0x62, 
        (byte)0xad, (byte)0x2c, (byte)0x4f, (byte)0x0c, (byte)0xa2, (byte)0x19, (byte)0x4e, (byte)0x9c, 
        (byte)0x1e, (byte)0x13, (byte)0x05, (byte)0x0b, (byte)0x90, (byte)0x81, (byte)0xb3, (byte)0x3d, 
        (byte)0xda, (byte)0x54, (byte)0x2b, (byte)0xe5, (byte)0xe3, (byte)0x52, (byte)0xce, (byte)0xe3, 
        (byte)0x6f, (byte)0x45, (byte)0x07, (byte)0xbf, (byte)0x97, (byte)0x52, (byte)0x10, (byte)0xbf, 
        (byte)0xff, (byte)0xd9, (byte)0x6e, (byte)0x10, (byte)0x95, (byte)0xaa, (byte)0x3c, (byte)0x7e, 
        (byte)0xb4, (byte)0x80, (byte)0x51, (byte)0x4b, (byte)0xe3, (byte)0xa4, (byte)0xea, (byte)0x2a, 
        (byte)0xb7, (byte)0x11, (byte)0x5e, (byte)0xf6, (byte)0x98, (byte)0x03, (byte)0xe4, (byte)0xe0, 
        (byte)0x1f, (byte)0x90, (byte)0xe4, (byte)0x28, (byte)0xc5, (byte)0x00, (byte)0xdf, (byte)0xcd, 
        (byte)0x20, (byte)0x52, (byte)0x07, (byte)0xf3, (byte)0x0e, (byte)0x62, (byte)0x7b, (byte)0x56, 
        (byte)0xa1, (byte)0x68, (byte)0x88, (byte)0x35, (byte)0xd3, (byte)0xc2, (byte)0x2d, (byte)0x4c, 
        (byte)0x84, (byte)0x95, (byte)0xdd, (byte)0xfa, (byte)0xe6, (byte)0xa7, (byte)0xb0, (byte)0x70, 
        (byte)0x91, (byte)0x41, (byte)0x4e, (byte)0x2c, (byte)0x65, (byte)0x26, (byte)0x07, (byte)0xc9, 
        (byte)0x84, (byte)0xcc, (byte)0xbf, (byte)0xa7, (byte)0x85, (byte)0x29, (byte)0x95, (byte)0x88, 
        (byte)0x8b, (byte)0x5f, (byte)0x7e, (byte)0x18, (byte)0xdc, (byte)0x32, (byte)0xdf, (byte)0x40, 
        (byte)0xd4, (byte)0x41, (byte)0x38, (byte)0x34, (byte)0x94, (byte)0x1a, (byte)0x9e, (byte)0x80, 
        (byte)0xa2, (byte)0x54, (byte)0x86, (byte)0xe8, (byte)0x93, (byte)0xf4, (byte)0xf6, (byte)0x73, 
        (byte)0x8e, (byte)0xf1, (byte)0x11, (byte)0xe2, (byte)0x96, (byte)0x6a, (byte)0xb2, (byte)0x57, 
        (byte)0x29, (byte)0xa5, (byte)0x58, (byte)0xf7, (byte)0x23, (byte)0x59, (byte)0x63, (byte)0x28, 
        (byte)0x29, (byte)0x11, (byte)0xfe, (byte)0xdf, (byte)0xa5, (byte)0x15, (byte)0xd2, (byte)0xb2, 
        (byte)0xd5, (byte)0x00, (byte)0x1b, (byte)0xe4, (byte)0xee, (byte)0x8a, (byte)0xac, (byte)0x1d, 
        (byte)0x9b, (byte)0xa1, (byte)0xfe, (byte)0xa5, (byte)0x4d, (byte)0x62, (byte)0xdb, (byte)0xe8, 
        (byte)0x5a, (byte)0x7a, (byte)0xe7, (byte)0x85, (byte)0x4e, (byte)0x64, (byte)0x38, (byte)0x6a, 
        (byte)0x64, (byte)0xaa, (byte)0xd4, (byte)0x53, (byte)0x62, (byte)0x64, (byte)0x9c, (byte)0xbf, 
        (byte)0x7f, (byte)0x46, (byte)0x5e, (byte)0x65, (byte)0x78, (byte)0x76, (byte)0x0c, (byte)0xcc, 
        (byte)0x5d, (byte)0xfc, (byte)0x70, (byte)0x7c, (byte)0x2d, (byte)0x92, (byte)0x4d, (byte)0x51, 
        (byte)0xf8, (byte)0xfd, (byte)0xdf, (byte)0x2e, (byte)0xbe, (byte)0x01, (byte)0x51, (byte)0x92, 
        (byte)0xe5, (byte)0x80, (byte)0x11, (byte)0x74, (byte)0x15, (byte)0xa1, (byte)0x01, (byte)0xd1, 
        (byte)0x2b, (byte)0x46, (byte)0x44, (byte)0x0c, (byte)0xc5, (byte)0x28, (byte)0xe7, (byte)0xf0, 
        (byte)0xb4, (byte)0x46, (byte)0x82, (byte)0xcf, (byte)0x80, (byte)0x42, (byte)0xa1, (byte)0xeb, 
        (byte)0xa5, (byte)0x72, (byte)0x09, (byte)0xdc, (byte)0x08, (byte)0x80, (byte)0x25, (byte)0xfa, 
        (byte)0xe4, (byte)0x03, (byte)0x5a, (byte)0x4e, (byte)0xd3, (byte)0xe4, (byte)0x52, (byte)0xd1, 
        (byte)0x20, (byte)0x67, (byte)0xaa, (byte)0xcd, (byte)0xe7, (byte)0xb8, (byte)0x02, (byte)0xa8, 
        (byte)0x11, (byte)0xf7, (byte)0x13, (byte)0x42, (byte)0xf4, (byte)0xb4, (byte)0x69, (byte)0x0b, 
        (byte)0x37, (byte)0xe5, (byte)0xf9, (byte)0xda, (byte)0x44, (byte)0xed, (byte)0x2b, (byte)0x69, 
        (byte)0x3e, (byte)0x6a, (byte)0xe1, (byte)0xd3, (byte)0x51, (byte)0x21, (byte)0x49, (byte)0x0a, 
        (byte)0xd3, (byte)0xef, (byte)0xf6, (byte)0xe9, (byte)0x19, (byte)0xcc, (byte)0x7b, (byte)0xc7, 
        (byte)0x4b, (byte)0x5a, (byte)0xb6, (byte)0x99, (byte)0x3e, (byte)0x1d, (byte)0x3d, (byte)0xb8, 
        (byte)0x60, (byte)0x24, (byte)0x2a, (byte)0x6a, (byte)0x32, (byte)0x8a, (byte)0x2b, (byte)0xcd, 
        (byte)0x35, (byte)0xb6, (byte)0xbf, (byte)0x39, (byte)0x5d, (byte)0xe2, (byte)0xa0, (byte)0x92, 
        (byte)0x4a, (byte)0x8e, (byte)0x92, (byte)0x69, (byte)0x5c, (byte)0xa1, (byte)0x55, (byte)0xe6, 
        (byte)0xbf, (byte)0x0b, (byte)0x18, (byte)0x5b, (byte)0x22, (byte)0x33, (byte)0x6d, (byte)0x2d, 
        (byte)0x6f, (byte)0xf8, (byte)0xae, (byte)0x4e, (byte)0x58, (byte)0x6d, (byte)0xe7, (byte)0x37, 
        (byte)0x01, (byte)0x39, (byte)0xee, (byte)0x04, (byte)0x4b, (byte)0x8d, (byte)0x9c, (byte)0x77, 
        (byte)0xb3, (byte)0x4d, (byte)0x51, (byte)0xe8, (byte)0xc9, (byte)0x26, (byte)0x14, (byte)0x10, 
        (byte)0xf8, (byte)0x0f, (byte)0xb5, (byte)0xd2, (byte)0x04, (byte)0x30, (byte)0x04, (byte)0x25, 
        (byte)0x9d, (byte)0xa1, (byte)0x41, (byte)0xad, (byte)0x78, (byte)0xed, (byte)0xb2, (byte)0xfa, 
        (byte)0xba, (byte)0x63, (byte)0x0a, (byte)0x3c, (byte)0x80, (byte)0xa9, (byte)0xd1, (byte)0x5e, 
        (byte)0x03, (byte)0x0f, (byte)0x5b, (byte)0x78, (byte)0x65, (byte)0x3d, (byte)0x0b, (byte)0xf1, 
        (byte)0x8c, (byte)0x06, (byte)0x2d, (byte)0x22, (byte)0x52, (byte)0x70, (byte)0x3a, (byte)0xa7, 
        (byte)0x0f, (byte)0xe1, (byte)0x7a, (byte)0x0d, (byte)0x3f, (byte)0x75, (byte)0x25, (byte)0x16, 
        (byte)0xde, (byte)0xcb, (byte)0x08, (byte)0x9a, (byte)0x0c, (byte)0xb8, 
    };

    /**
     * RC4 key.
     */
    static final private byte[] RC4_KEY = new byte[] {
        (byte)0x2f, (byte)0xe5, (byte)0xb7, (byte)0x40, (byte)0x0c, (byte)0xd8, (byte)0xa5, (byte)0xef,
        (byte)0x87, (byte)0x23, (byte)0xb6, (byte)0x30, (byte)0x50, (byte)0xc3, (byte)0x4f, (byte)0xa7
    };

    /**
     * Generate secret key for RC4 crypto algorithm.
     * @return secret key for RC4 crypto algorithm.
     *
     * @throws NoSuchAlgorithmException
     * @throws InvalidKeyException
     * @throws InvalidKeySpecException
     */
    private static SecretKey generateSecretKey() throws NoSuchAlgorithmException, InvalidKeyException,
                    InvalidKeySpecException {
        // create a SecretKeySpec from key material
        return new SecretKeySpec(RC4_KEY, "RC4");
    }

    /**
     * Check if RC4 crypting and decrypting works as expected.
     *
     * @param harness  the test harness (<code>null</code> not permitted).
     * @param secretKey RC4 secret key
     * @param rc4 instance of class which implements RC4 crypto algorithm.
     * @param expectedCipherText expected byte stream.
     * @throws InvalidKeyException
     * @throws IllegalBlockSizeException
     * @throws BadPaddingException
     */
    private static void checkCipher(TestHarness harness, SecretKey secretKey, Cipher rc4, String originalText, byte[] expectedCipherText) {
        try
        {
            rc4.init(Cipher.ENCRYPT_MODE, secretKey);
            byte[] ciphertext;
            ciphertext = rc4.doFinal(originalText.getBytes());
            rc4.init(Cipher.DECRYPT_MODE, secretKey);
            String cleartext = new String(rc4.doFinal(ciphertext));

            //printCipherTest(ciphertext);

            // test if ENCRYPT_MODE is ok
            for (int i = 0; i < expectedCipherText.length; i++)
            {
                if (expectedCipherText[i] != ciphertext[i])
                {
                    harness.fail("cipher text differ at index " + i);
                }
            }

            // test if DECRYPT_MODE is ok
            if (!originalText.equals(cleartext))
            {
                harness.fail("Decrypted text is different from the original!");
            }
        }
        catch (InvalidKeyException e)
        {
          harness.fail("Failure: " + e.getMessage());
          harness.debug(e);
        }
        catch (IllegalBlockSizeException e)
        {
            harness.fail("Failure: " + e.getMessage());
            harness.debug(e);
        }
        catch (BadPaddingException e)
        {
            harness.fail("Failure: " + e.getMessage());
            harness.debug(e);
        }
    }

    @SuppressWarnings({ "boxing", "unused" })
    private static void printCipherTest(byte[] ciphertext) {
        for (int i=0; i < ciphertext.length; i++) {
            System.out.format("(byte)0x%02x, ", ciphertext[i]);
            if ((i+1)%8 == 0) {
                System.out.println();
            }
        }
    }

    /**
     * Run the test harness.
     *
     * @param harness  the test harness (<code>null</code> not permitted).
     * @throws NoSuchAlgorithmException
     * @throws InvalidKeyException
     * @throws InvalidKeySpecException
     * @throws NoSuchPaddingException
     * @throws IllegalBlockSizeException
     * @throws BadPaddingException
     */
    private static void run(TestHarness harness) {
        SecretKey secretKey;
        try
        {
            secretKey = generateSecretKey();
            Cipher RC4 = Cipher.getInstance("RC4");

            // check crypto for many open texts.
            checkCipher(harness, secretKey, RC4, ORIGINAL_TEXT_1, EXPECTED_CIPHER_TEXT_1);
            checkCipher(harness, secretKey, RC4, ORIGINAL_TEXT_2, EXPECTED_CIPHER_TEXT_2);
            checkCipher(harness, secretKey, RC4, ORIGINAL_TEXT_3, EXPECTED_CIPHER_TEXT_3);
            checkCipher(harness, secretKey, RC4, ORIGINAL_TEXT_4, EXPECTED_CIPHER_TEXT_4);
            checkCipher(harness, secretKey, RC4, ORIGINAL_TEXT_5, EXPECTED_CIPHER_TEXT_5);
            checkCipher(harness, secretKey, RC4, ORIGINAL_TEXT_6, EXPECTED_CIPHER_TEXT_6);
            checkCipher(harness, secretKey, RC4, ORIGINAL_TEXT_7, EXPECTED_CIPHER_TEXT_7);
        }
        catch (InvalidKeyException e)
        {
            harness.fail("Failure: " + e.getMessage());
            harness.debug(e);
        }
        catch (NoSuchAlgorithmException e)
        {
            harness.fail("Failure: " + e.getMessage());
            harness.debug(e);
        }
        catch (InvalidKeySpecException e)
        {
            harness.fail("Failure: " + e.getMessage());
            harness.debug(e);
        }
        catch (NoSuchPaddingException e)
        {
            harness.fail("Failure: " + e.getMessage());
            harness.debug(e);
        }
    }

    /**
     * Test if RC4 crypto algorithm is available on given JVM.
     */
    public boolean isRC4CipherAvailable() {
        try {
            Cipher.getInstance("RC4");
        }
        catch (NoSuchAlgorithmException e) {
            return false;
        }
        catch (NoSuchPaddingException e) {
            return false;
        }
        return true;
    }

    /**
     * Runs the test using the specified harness.
     *
     * @param harness  the test harness (<code>null</code> not permitted).
     */
    public void test(TestHarness harness) {
        if (!isRC4CipherAvailable()) {
            return;
        }
        run(harness);
    }

    /**
     * Runs the test from CLI.
     *
     * @param args
     * @throws Exception
     */
    public static void main(String[] args) throws Exception {
        run(null);
    }

}
