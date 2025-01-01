/* TestOfSRPPrimitives.java -- 
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
 * Regression tests for SRP cryptographic primitives.
 */
public class TestOfSRPPrimitives implements Testlet
{
  private String user = "TestOfSRPPrimitives";

  private String password = "secret";

  private String pFile = "./__TestOfSRPPrimitives";

  private String p2File = pFile + "2";

  private String cFile = pFile + ".conf";

  private PasswordFile tpasswd;

  private Random prng = new Random();

  public void test(TestHarness harness)
  {
    harness.checkPoint("setting up the test");
    try
      {
        File f = new File(pFile);
        if (!f.exists())
          {
            if (f.createNewFile())
                f.deleteOnExit();
          }
        else if (!f.isFile())
          {
            throw new RuntimeException("File object " + pFile
                                       + " exists but is not a file");
          }
        else if (!f.canRead() || !f.canWrite())
          {
            throw new RuntimeException("File " + pFile
                                       + " exists but is not accessible");
          }
        tpasswd = new PasswordFile(pFile, p2File, cFile);
        if (!tpasswd.contains(user))
          {
            byte[] testSalt = new byte[10];
            prng.nextBytes(testSalt);
            tpasswd.add(user, password, testSalt, "1");
          }
        else
          {
            tpasswd.changePasswd(user, password);
          }

        for (int i = 0; i < SRPRegistry.SRP_ALGORITHMS.length; i++)
          {
            exerciseAlgorithm(harness,
                              SRP.instance(SRPRegistry.SRP_ALGORITHMS[i]));
          }

      }
    catch (IOException x)
      {
        harness.debug(x);
        harness.fail("TestOfSRPPrimitives");
      }
    finally
      {
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

  private void exerciseAlgorithm(TestHarness harness, SRP srp)
  {
    harness.checkPoint("TestOfSRPPrimitives.exerciseAlgorithm("
                       + srp.getAlgorithm() + ")");
    try
      {
        String[] entry = tpasswd.lookup(user, srp.getAlgorithm());
        BigInteger v = new BigInteger(1, Util.fromBase64(entry[0]));
        byte[] s = Util.fromBase64(entry[1]);

        String[] mpi = tpasswd.lookupConfig(entry[2]);
        BigInteger N = new BigInteger(1, Util.fromBase64(mpi[0]));
        BigInteger g = new BigInteger(1, Util.fromBase64(mpi[1]));

        IKeyPairGenerator kpg = new SRPKeyPairGenerator();
        HashMap attributes = new HashMap();
        attributes.put(SRPKeyPairGenerator.SHARED_MODULUS, N);
        attributes.put(SRPKeyPairGenerator.GENERATOR, g);
        kpg.setup(attributes);

        KeyPair clientKP = kpg.generate();
        BigInteger A = ((SRPPublicKey) clientKP.getPublic()).getY();
        BigInteger a = ((SRPPrivateKey) clientKP.getPrivate()).getX();

        attributes.put(SRPKeyPairGenerator.USER_VERIFIER, v);
        kpg.setup(attributes);

        KeyPair serverKP = kpg.generate();
        BigInteger B = ((SRPPublicKey) serverKP.getPublic()).getY();
        BigInteger b = ((SRPPrivateKey) serverKP.getPrivate()).getX();

        // compute u = H(A | B)
        IMessageDigest hash = srp.newDigest();
        byte[] buffy;
        buffy = Util.trim(A);
        hash.update(buffy, 0, buffy.length);
        buffy = Util.trim(B);
        hash.update(buffy, 0, buffy.length);

        BigInteger u = new BigInteger(1, hash.digest());

        // compute S = ((A * (v ** u)) ** b) % N
        BigInteger S1 = A.multiply(v.modPow(u, N)).modPow(b, N);

        // compute K = H(S) (as of rev 08)
        byte[] s1Bytes = Util.trim(S1);
        hash.update(s1Bytes, 0, s1Bytes.length);

        byte[] K1 = hash.digest();

        BigInteger x = new BigInteger(1, srp.computeX(s, user, password));

        // compute S = ((B - (3 * (g ** x))) ** (a + (u * x))) % N
        // compute S = ((B - (3 * v)) ** (a + (u * x))) % N
        BigInteger S2 =
            B.subtract(BigInteger.valueOf(3L).multiply(v))
            .modPow(a.add(u.multiply(x)), N);

        // compute K = H(S) (as of rev 08)
        byte[] s2Bytes = Util.trim(S2);
        hash.update(s2Bytes, 0, s2Bytes.length);

        byte[] K2 = hash.digest();

        harness.check(Arrays.equals(K1, K2)); // #1,4,7,10

        // ===================================================================

        String L = "ALSM=IE,Slsd=fi4fg_;asdg_gsdfmof"; // available options
        String o = "KLK=FSOIIOAS,Oiasf,oaa=sdin_;asd"; // chosen options
        byte[] sid = "abc".getBytes();
        int ttl = 23;
        byte[] cIV = new byte[16];
        byte[] sIV = new byte[16];
        byte[] sCB = "host.acme.com".getBytes();
        byte[] cCB = "user@acme.com".getBytes();
        byte[] cn = "client".getBytes();
        byte[] sn = "server".getBytes();
        prng.nextBytes(cIV);
        prng.nextBytes(sIV);
        byte[] cM1 = srp.generateM1(N, g, user, s, A, B, K1, user, L, cn, cCB);
        byte[] cM2 =
            srp.generateM2(A, cM1, K1, user, user, o, sid, ttl, cIV, sIV, sCB);
        byte[] sM1 = srp.generateM1(N, g, user, s, A, B, K2, user, L, cn, cCB);
        byte[] sM2 =
            srp.generateM2(A, sM1, K2, user, user, o, sid, ttl, cIV, sIV, sCB);

        harness.check(Arrays.equals(cM1, sM1)); // #2,5,8,11
        harness.check(Arrays.equals(cM2, sM2)); // #3,6,9,12
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfSRPPrimitives.exerciseAlgorithm("
                     + srp.getAlgorithm() + ")");
      }
  }
}