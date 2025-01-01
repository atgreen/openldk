/* TestOfCipherEngineInit.java -- Some more tests for engineInit
 * related to the tests in TestOfPR27849.
 Copyright (C) 2006 Free Software Foundation, Inc.
 This file is part of Mauve.

 Mauve is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2, or (at your option)
 any later version.

 Mauve is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with Mauve; see the file COPYING.  If not, write to the
 Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 02110-1301 USA.

 */

// Tags: GNU-CRYPTO JDK1.4

package gnu.testlet.gnu.javax.crypto.jce;

import gnu.javax.crypto.jce.spec.BlockCipherParameterSpec;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.security.AlgorithmParameters;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.SecureRandom;
import java.security.spec.AlgorithmParameterSpec;
import java.util.Arrays;

import javax.crypto.Cipher;
import javax.crypto.KeyGenerator;
import javax.crypto.spec.IvParameterSpec;

/**
 * Regression tests for logic around CipherSpi.engineInit()s. If the cipher
 * being initialized requires parameters not derivable from the key, then
 * defaults/random parameters must be generated, provided the opmode is ENCRYPT
 * or WRAP. If the opmode is DECRYPT or UNWRAP then an exception is expected to
 * be thrown.
 * <p>
 * Refer to the class documentation of {@link javax.crypto.CipherSpi} for more
 * information.
 */
public class TestOfCipherEngineInit
    implements Testlet
{
  private static final String input = "Does this work ?";
  private Key key;

  private Cipher cipher;

  byte[] iv;

  /* (non-Javadoc)
   * @see gnu.testlet.Testlet#test(gnu.testlet.TestHarness)
   */
  public void test(TestHarness harness)
  {
    setUp(harness);
    testECB(harness);
    testNotECB(harness);
    testInitWithParameterSpec(harness);
    testInitWithIVParameterSpec(harness);

    // TODO: Add tests for WRAP and UNWRAP too
  }

  private void setUp(TestHarness harness)
  {
    try
      {
        KeyGenerator keyG = KeyGenerator.getInstance("DESede");
        keyG.init(new SecureRandom());
        key = keyG.generateKey();
      }
    catch (Exception e)
      {
        harness.debug(e);
        harness.fail(String.valueOf(e));
      }
  }

  private void testECB(TestHarness harness)
  {
    try
      {
        cipher = Cipher.getInstance("DESede/ECB/NoPadding");

        cipher.init(Cipher.ENCRYPT_MODE, key);
        iv = cipher.getIV();
        harness.check(iv == null, "(ECB Encrypt) getIV() MUST return null");
        byte[] plaintext = input.getBytes();
        byte[] ciphertext = cipher.doFinal(plaintext);

        iv = null;
        cipher.init(Cipher.DECRYPT_MODE, key);
        iv = cipher.getIV();
        harness.check(iv == null, "(ECB Decrypt) getIV() MUST return null");
        byte[] plaintext2 = cipher.doFinal(ciphertext);
        String recovered = new String(plaintext2);

        harness.check(input.equals(recovered),
                      "Original and recovered texts MUST be equal");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail(String.valueOf(x));
      }
  }

  public void testNotECB(TestHarness harness)
  {
    try
      {
        cipher = Cipher.getInstance("DESede/CBC/NoPadding");

        cipher.init(Cipher.ENCRYPT_MODE, key);
        iv = cipher.getIV();
        harness.check(iv != null, "(CBC Encrypt) getIV() MUST NOT return null");
        harness.check(iv.length == 8, "(CBC Encrypt) IV length MUST be 8");
        byte[] plaintext = input.getBytes();
        byte[] ciphertext = cipher.doFinal(plaintext);

        iv = null;
        String msg = "(CBC Decrypt) init(2) MUST throw InvalidKeyException";
        try
          {
            cipher.init(Cipher.DECRYPT_MODE, key);
            harness.fail(msg);
          }
        catch (Exception e)
          {
            String type = e.getClass().getName();
            harness.check(type.equals(InvalidKeyException.class.getName()), msg);
          }
        cipher.init(Cipher.DECRYPT_MODE, key, cipher.getParameters());
        iv = cipher.getIV();
        harness.check(iv != null, "(CBC Decrypt) getIV() MUST NOT return null");
        harness.check(iv.length == 8, "(CBC Decrypt) IV length MUST be 8");
        byte[] plaintext2 = cipher.doFinal(ciphertext);
        String recovered = new String(plaintext2);

        harness.check(input.equals(recovered),
                      "Original and recovered texts MUST be equal");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail(String.valueOf(x));
      }
  }

  // Similar test to above but tests the behaviour for:
  // Cipher.init(int i, Key k, AlgorithmParameterSpec param)
  // If param is null, and the cipher needs algorithm parameters to function
  // then init should create random/default parameters provided the cipher is
  // in ENCRYPT or WRAP mode, if in DECRYPT or UNWRAP mode the function must
  // throw an InvalidAlgorithmParameterException.
  //
  // Extrapolation: If algorithm does not require additional params then none
  // should be created!
  private void testInitWithParameterSpec(TestHarness harness)
  {
    try
      {
        // This cipher does not need extra algorithm parameters like
        // an IV to be provided so it should not generate one
        cipher = Cipher.getInstance("DESede/ECB/NoPadding");

        cipher.init(Cipher.ENCRYPT_MODE, key, (AlgorithmParameterSpec) null);
        iv = cipher.getIV();
        harness.check(iv == null,
                      "(ECB Encrypt + null AlgorithmParameterSpec) getIV() MUST return null");
        byte[] plaintext = input.getBytes();
        byte[] ciphertext = cipher.doFinal(plaintext);

        iv = null;
        // No need for an IV so none should be generated in decrypt mode either
        cipher.init(Cipher.DECRYPT_MODE, key, (AlgorithmParameterSpec) null);
        iv = cipher.getIV();
        harness.check(iv == null,
                      "(ECB Decrypt + null AlgorithmParameterSpec) getIV() MUST return null");
        byte[] plaintext2 = cipher.doFinal(ciphertext);
        String recovered = new String(plaintext2);

        // Encryption and decryption should still work
        harness.check(input.equals(recovered),
                      "Original and recovered texts MUST be equal");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail(String.valueOf(x));
      }

    try
      {
        cipher = Cipher.getInstance("DESede/CBC/NoPadding");

        // null param for CBC should result in random IV being generated
        cipher.init(Cipher.ENCRYPT_MODE, key, (AlgorithmParameterSpec) null);
        iv = cipher.getIV();
        harness.check(iv != null,
                      "(CBC Encrypt + null AlgorithmParameterSpec) getIV() MUST return non-null IV");
        harness.check(iv.length == 8,
                      "(CBC Encrypt + null AlgorithmParameterSpec) IV length MUST be 8");
        // test for non-uniformity of IV; i.e. not all the bytes are the same
        harness.check(iv[0] != iv[1],
                      "(CBC Encrypt + null AlgorithmParameterSpec) IV MUST be random");
        byte[] plaintext = input.getBytes();
        byte[] ciphertext = cipher.doFinal(plaintext);

        iv = null;

        AlgorithmParameters ap = cipher.getParameters();
        AlgorithmParameterSpec backupAlg = ap.getParameterSpec(IvParameterSpec.class);
        String msg = "(CBC Decrypt + null AlgorithmParameterSpec) init(3) MUST throw InvalidAlgorithmParameterException";
        try
          {
            // Should not be able to init a CBC cipher in DECRYPT opmode without
            // params
            cipher.init(Cipher.DECRYPT_MODE, key, (AlgorithmParameterSpec) null);
            harness.fail(msg);
          }
        catch (Exception e)
          {
            String type = e.getClass().getName();
            harness.check(type.equals(InvalidAlgorithmParameterException.class.getName()),
                          msg);
          }
        try
          {
            // Now use a proper algorithm parameter and test init.
            // This should pass!!
            cipher.init(Cipher.DECRYPT_MODE, key, backupAlg);
          }
        catch (Exception e)
          {
            harness.fail("(CBC Decrypt + non-null AlgorithmParameterSpec) init(3) MUST NOT throw an exception");
          }

        iv = cipher.getIV();
        harness.check(iv != null,
                      "(CBC Decrypt + valid AlgorithmParameterSpec) getIV() MUST NOT return null");
        harness.check(iv.length == 8,
                      "(CBC Decrypt + valid AlgorithmParameterSpec) IV length MUST be 8");
        harness.check(iv[0] != iv[1],
                      "(CBC Encrypt + null AlgorithmParameterSpec) IV MUST be random");
        byte[] plaintext2 = cipher.doFinal(ciphertext);
        String recovered = new String(plaintext2);

        harness.check(input.equals(recovered),
                      "Original and recovered texts MUST be equal");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail(String.valueOf(x));
      }
  }
  
  // Similar test to above but tests the behaviour for:
  // Cipher.init(int i, Key k, IVParameterSpec param)
  // If the IV passed to the algorithm is too short or too long
  // then a InvalidAlgorithmParameterException should be thrown
  private void testInitWithIVParameterSpec(TestHarness harness)
  {
    try
      {
        cipher = Cipher.getInstance("DESede/CBC/NoPadding");
        
        // Blocksize for CBC
        int blocksize = 8;
   
        IvParameterSpec IVSpec = new IvParameterSpec(new byte[blocksize -1]);
 
        // check if short IV are properly throwing exceptions
        String msg = "(CBC Encrypt + short IV) MUST throw InvalidAlgorithmParameterException";
        try{
          cipher.init(Cipher.ENCRYPT_MODE, key, IVSpec);
          harness.fail(msg);
        } catch (Exception e) {
          String type = e.getClass().getName();
          harness.debug(e);
          harness.check(type.equals(InvalidAlgorithmParameterException.class.getName()),msg);
        }
        
        msg = "(CBC Decrypt + short IV) MUST throw InvalidAlgorithmParameterException";
        try {
          cipher.init(Cipher.DECRYPT_MODE, key, IVSpec);
          harness.fail(msg);
        } catch (Exception e) {
          String type = e.getClass().getName();
          harness.check(type.equals(InvalidAlgorithmParameterException.class.getName()),msg);
        }
        
        IVSpec = new IvParameterSpec(new byte[blocksize +1]);
        
        // check if long IV are properly throwing exceptions
        msg = "(CBC Encrypt + long IV) MUST throw InvalidAlgorithmParameterException";
        try{
          cipher.init(Cipher.ENCRYPT_MODE, key, IVSpec);
          harness.fail(msg);
        } catch (Exception e) {
          String type = e.getClass().getName();
          harness.check(type.equals(InvalidAlgorithmParameterException.class.getName()), msg);
        }
        
        msg = "(CBC Decrypt + long IV) MUST throw InvalidAlgorithmParameterException";
        try {
          cipher.init(Cipher.DECRYPT_MODE, key, IVSpec);
          harness.fail(msg);
        } catch (Exception e) {
          String type = e.getClass().getName();
          harness.check(type.equals(InvalidAlgorithmParameterException.class.getName()), msg);
        }
        
        byte[] iv = new byte[] { '0', '1', '1', '2', '3', '5', '8', '9'};
        IVSpec = new IvParameterSpec(iv);
        
        // check if no exceptions are being called for a properly sized IV
        try{
          cipher.init(Cipher.ENCRYPT_MODE, key, IVSpec);
        } catch (Exception e){
          harness.fail("(CBC Encrypt + IV of lenght 8) MUST NOT throw an exception");
        }
        
        byte[] plaintext = input.getBytes();
        byte[] ciphertext = cipher.doFinal(plaintext);
        
        try{
          cipher.init(Cipher.DECRYPT_MODE, key, IVSpec);
        } catch (Exception e){
          harness.fail("(CBC Decrypt + IV of lenght 8) MUST NOT throw an exception");
        }
        
        byte[] plaintext2 = cipher.doFinal(ciphertext);
        String recovered = new String(plaintext2);
        harness.check(input.equals(recovered),
                      "Original and recovered texts MUST be equal");
                
        byte[] cipherIV = cipher.getIV();
        harness.check(iv != null,
                       "(CBC Decrypt + valid IvParameterSpec) getIV() MUST NOT return null");
        harness.check(iv.length == 8,
                      "(CBC Decrypt + valid IvParameterSpec) IV length MUST be 8");
        harness.check(Arrays.equals(cipherIV, iv), 
                      "(CBC Decrypt + valid IvParameter) Cipher IV MUST match specified IV");
                
      } catch (Exception x){
        harness.debug(x);
        harness.fail(String.valueOf(x));
      }
  }
  
  
}
