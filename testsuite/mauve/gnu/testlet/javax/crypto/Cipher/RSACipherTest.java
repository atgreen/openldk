// Test basic usage of RSA crypto algorithm.

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


import java.math.BigInteger;
import java.security.InvalidKeyException;
import java.security.KeyFactory;
import java.security.NoSuchAlgorithmException;
import java.security.interfaces.RSAPrivateKey;
import java.security.interfaces.RSAPublicKey;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.RSAPrivateKeySpec;
import java.security.spec.RSAPublicKeySpec;



import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;


/**
 * Test the basic usage of RSA crypto algorithm.
 * 
 * @author Pavel Tisnovsky (ptisnovs@redhat.com)
 */
public class RSACipherTest implements Testlet {

    /**
     * Text which should be crypted into byte stream.
     */
    static final private String ORIGINAL_TEXT_1 = "";

    /**
     * Text which should be crypted into byte stream.
     */
    static final private String ORIGINAL_TEXT_2 = "\0";

    /**
     * Text which should be crypted into byte stream.
     */
    static final private String ORIGINAL_TEXT_3 = " ";

    /**
     * Text which should be crypted into byte stream.
     */
    static final private String ORIGINAL_TEXT_4 = "a";

    /**
     * Text which should be crypted into byte stream.
     */
    static final private String ORIGINAL_TEXT_5 = "Hello World!";

    /**
     * Text which should be crypted into byte stream.
     */
    static final private String ORIGINAL_TEXT_6 = "The quick brown fox jumps over the lazy dog";

    /**
     * Text which should be crypted into byte stream.
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

    static final private String PUBLIC_MODULUS = "20521618938750603886170744528061864800146016572819080111682671691278915352252891634200145574388427469417751651363578227984376950979495478398227030691493445339382263983444097283703619373617601731938263971183654870713491367757745954357344770541391919418932193615123185798218976740947824407136202739666081938920517644571472041627033780738340359795905800010579818905513797329848506741408960279798882512687625717535119998977014933204822468082603589932145482019064491156658721732916066235818372701810201909767317918681953323275040404827189693865288044179436810701655573732550803016254388672687249662548511933125930329915723";
    static final private String PUBLIC_EXPONENT = "65537";
    static final private String PRIVATE_MODULUS = "20521618938750603886170744528061864800146016572819080111682671691278915352252891634200145574388427469417751651363578227984376950979495478398227030691493445339382263983444097283703619373617601731938263971183654870713491367757745954357344770541391919418932193615123185798218976740947824407136202739666081938920517644571472041627033780738340359795905800010579818905513797329848506741408960279798882512687625717535119998977014933204822468082603589932145482019064491156658721732916066235818372701810201909767317918681953323275040404827189693865288044179436810701655573732550803016254388672687249662548511933125930329915723";
    static final private String PRIVATE_EXPONENT = "8000478567604222489458817502967493868253516691876764222553553896458127275433135194681076634963826874034874264802164025283440363421061529717178092286306323579370688996704101280171925400856458553962229648347382119210975547342881259957431052494507889301520019940894416850702578020526067925024489677563336642498311583438627434254066916514271727557705610984000137209177546192306797912010857347995421016713475854744382175824136703731479787147945167966533384454182558767171908782359933795514186110809500085594996579279145828982043523663886747894662875604861018419475159222870626595731403703852674247380889399799091109758673";

    /**
     * Generate public key spec for RSA crypto algorithm.
     *
     * @return public key spec for RSA crypto algorithm.
     *
     * @throws NoSuchAlgorithmException
     * @throws InvalidKeyException
     * @throws InvalidKeySpecException
     */
    private static RSAPublicKeySpec generatePublicKeySpec() throws NoSuchAlgorithmException, InvalidKeyException,
                    InvalidKeySpecException {
        return new RSAPublicKeySpec(new BigInteger(PUBLIC_MODULUS), new BigInteger(PUBLIC_EXPONENT));
    }

    /**
     * Generate private key spec for RSA crypto algorithm.
     *
     * @return private key spec for RSA crypto algorithm.
     *
     * @throws NoSuchAlgorithmException
     * @throws InvalidKeyException
     * @throws InvalidKeySpecException
     */
    private static RSAPrivateKeySpec generatePrivateKeySpec() throws NoSuchAlgorithmException, InvalidKeyException,
                    InvalidKeySpecException {
        return new RSAPrivateKeySpec(new BigInteger(PRIVATE_MODULUS), new BigInteger(PRIVATE_EXPONENT));
    }

    /**
     * Check if RSA crypting and decrypting works as expected.
     *
     * @param harness  the test harness (<code>null</code> not permitted).
     * @param RSA instance of class which implements RSA crypto algorithm.
     * @throws IllegalBlockSizeException 
     * @throws InvalidKeyException
     * @throws IllegalBlockSizeException
     * @throws BadPaddingException
     */
    private static void checkCipher(TestHarness harness, RSAPublicKey publicKey, RSAPrivateKey privateKey, Cipher RSA, String originalText) throws IllegalBlockSizeException {
        try
        {
            RSA.init(Cipher.ENCRYPT_MODE, publicKey);
            byte[] ciphertext;
            ciphertext = RSA.doFinal(originalText.getBytes());
            RSA.init(Cipher.DECRYPT_MODE, privateKey);
            String cleartext = new String(RSA.doFinal(ciphertext));

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
        catch (BadPaddingException e)
        {
            harness.fail("Failure: " + e.getMessage());
            harness.debug(e);
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
        try
        {
            KeyFactory keyFactory = KeyFactory.getInstance("RSA");
            RSAPublicKey publicKey = (RSAPublicKey) keyFactory.generatePublic(generatePublicKeySpec());
            RSAPrivateKey privateKey = (RSAPrivateKey) keyFactory.generatePrivate(generatePrivateKeySpec());

            Cipher RSA = Cipher.getInstance("RSA/ECB/PKCS1Padding");

            // check crypto for many open texts.
            checkCipher(harness, publicKey, privateKey, RSA, ORIGINAL_TEXT_1);
            checkCipher(harness, publicKey, privateKey, RSA, ORIGINAL_TEXT_2);
            checkCipher(harness, publicKey, privateKey, RSA, ORIGINAL_TEXT_3);
            checkCipher(harness, publicKey, privateKey, RSA, ORIGINAL_TEXT_4);
            checkCipher(harness, publicKey, privateKey, RSA, ORIGINAL_TEXT_5);
            checkCipher(harness, publicKey, privateKey, RSA, ORIGINAL_TEXT_6);
            try {
                checkCipher(harness, publicKey, privateKey, RSA, ORIGINAL_TEXT_7);
                harness.fail("IllegalBlockSizeException don't thrown as expected!");
            }
            catch (IllegalBlockSizeException e) {
                // OK, expected here
            }
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
        catch (IllegalBlockSizeException e)
        {
            harness.fail("Failure: " + e.getMessage());
            harness.debug(e);
        }
    }

    /**
     * Test if RSA crypto algorithm is available on given JVM.
     */
    public boolean isRSACipherAvailable() {
        try {
            Cipher.getInstance("RSA");
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
        if (!isRSACipherAvailable()) {
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
