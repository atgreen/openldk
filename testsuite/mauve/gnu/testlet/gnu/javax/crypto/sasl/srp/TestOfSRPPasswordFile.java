/* TestOfSRPPasswordFile.java -- 
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

package gnu.testlet.gnu.javax.crypto.sasl.srp;

import gnu.java.security.Registry;
import gnu.java.security.hash.IMessageDigest;
import gnu.java.security.key.IKeyPairGenerator;
import gnu.java.security.util.Util;
import gnu.javax.crypto.key.srp6.SRPKeyPairGenerator;
import gnu.javax.crypto.key.srp6.SRPPrivateKey;
import gnu.javax.crypto.key.srp6.SRPPublicKey;
import gnu.javax.crypto.sasl.srp.PasswordFile;
import gnu.javax.crypto.sasl.srp.SRP;
import gnu.javax.crypto.sasl.srp.SRPRegistry;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
import java.security.KeyPair;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Random;

/**
 * Regression tests for SRP password file operations.
 */
public class TestOfSRPPasswordFile implements Testlet
{
  private Random prng = new Random();

  public void test(final TestHarness harness)
  {
    harness.checkPoint("TestOfSRPPasswordFile");
    try
      {
        exerciseFile(harness, Registry.SHA160_HASH);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("exerciseFile()");
      }
    try
      {
        exerciseFile(harness, Registry.MD5_HASH);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("exerciseFile(\"MD5\")");
      }
  }

  private void exerciseFile(final TestHarness harness, final String md)
      throws IOException
  {
    final String user = "test";
    final String password = "test";
    final String pFile = "./__TestOfSRPPasswordFile";
    final String p2File = pFile + "2"; // ./test2
    final String cFile = pFile + ".conf"; // ./test.conf

    final File f = new File(pFile);
    if (!f.exists())
      {
        if (f.createNewFile())
          f.deleteOnExit();
      }
    else if (!f.isFile())
      throw new RuntimeException("File object " + pFile
                                 + " exists but is not a file");
    else if (!f.canRead() || !f.canWrite())
      throw new RuntimeException("File " + pFile
                                 + " exists but is not accessible");
    final PasswordFile tpasswd = new PasswordFile(pFile, p2File, cFile);
    if (!tpasswd.contains(user))
      {
        final byte[] testSalt = new byte[10];
        prng.nextBytes(testSalt);
        tpasswd.add(user, password, testSalt, SRPRegistry.N_2048_BITS);
      }
    else
      tpasswd.changePasswd(user, password);

    final String[] entry = tpasswd.lookup(user, md);
    final BigInteger v = new BigInteger(1, Util.fromBase64(entry[0]));
    final byte[] salt = Util.fromBase64(entry[1]);

    final String[] mpi = tpasswd.lookupConfig(entry[2]);
    final BigInteger N = new BigInteger(1, Util.fromBase64(mpi[0]));
    final BigInteger g = new BigInteger(1, Util.fromBase64(mpi[1]));

    final IKeyPairGenerator kpg = new SRPKeyPairGenerator();
    final HashMap attributes = new HashMap();
    attributes.put(SRPKeyPairGenerator.SHARED_MODULUS, N);
    attributes.put(SRPKeyPairGenerator.GENERATOR, g);
    kpg.setup(attributes);

    final KeyPair clientKP = kpg.generate();
    final BigInteger A = ((SRPPublicKey) clientKP.getPublic()).getY();
    final BigInteger a = ((SRPPrivateKey) clientKP.getPrivate()).getX();

    attributes.put(SRPKeyPairGenerator.USER_VERIFIER, v);
    kpg.setup(attributes);

    final KeyPair serverKP = kpg.generate();
    final BigInteger B = ((SRPPublicKey) serverKP.getPublic()).getY();
    final BigInteger b = ((SRPPrivateKey) serverKP.getPrivate()).getX();

    // compute u = H(A | B)
    //      IMessageDigest hash = srp.newDigest();
    //      IMessageDigest hash = HashFactory.getInstance(md);
    final SRP srp = SRP.instance(md);
    final IMessageDigest hash = srp.newDigest();
    byte[] buffy;
    buffy = Util.trim(A);
    hash.update(buffy, 0, buffy.length);
    buffy = Util.trim(B);
    hash.update(buffy, 0, buffy.length);

    final BigInteger u = new BigInteger(1, hash.digest());

    // compute S = ((A * (v ** u)) ** b) % N
    final BigInteger S1 = A.multiply(v.modPow(u, N)).modPow(b, N);

    // compute K = H(S) (as of rev 08)
    final byte[] s1Bytes = Util.trim(S1);
    hash.update(s1Bytes, 0, s1Bytes.length);

    final byte[] K1 = hash.digest();

    final BigInteger x = new BigInteger(1, srp.computeX(salt, user, password));

    // compute S = ((B - (3 * (g ** x))) ** (a + (u * x))) % N
    // compute S = ((B - (3 * v)) ** (a + (u * x))) % N
    final BigInteger S2 =
        B.subtract(BigInteger.valueOf(3L).multiply(v))
        .modPow(a.add(u.multiply(x)), N);

    // compute K = H(S) (as of rev 08)
    final byte[] s2Bytes = Util.trim(S2);
    hash.update(s2Bytes, 0, s2Bytes.length);

    final byte[] K2 = hash.digest();

    harness.check(S1.equals(S2));
    harness.check(Arrays.equals(K1, K2));

    try
      {
        new File(pFile).delete(); // remove test file
      }
    catch (Exception ignored)
      {
      }
    try
      {
        new File(p2File).delete(); // remove test2 file
      }
    catch (Exception ignored)
      {
      }
    try
      {
        new File(cFile).delete(); // remove test.conf file
      }
    catch (Exception ignored)
      {
      }
  }
}