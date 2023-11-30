/* TestOfPBKDF2.java -- 
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

package gnu.testlet.gnu.javax.crypto.prng;

import gnu.java.security.util.Util;
import gnu.javax.crypto.mac.MacFactory;
import gnu.javax.crypto.prng.IPBE;
import gnu.javax.crypto.prng.PBKDF2;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Arrays;
import java.util.HashMap;

/**
 * Test of the key-derivation function PBKDF2 from PKCS #5. Based on test
 * vectors in <a href="http://www.ietf.org/internet-drafts/draft-raeburn-krb-rijndael-krb-05.txt">
 * AES Encryption for Kerberos 5</a>.
 */
public class TestOfPBKDF2 implements Testlet
{
  public void test(TestHarness harness)
  {
    try
      {
        harness.checkPoint("PBKDF2");
        PBKDF2 kdf = new PBKDF2(MacFactory.getInstance("HMAC-SHA1"));
        HashMap attr = new HashMap();
        byte[] dk = new byte[32];
        byte[] edk;
        byte[] salt;
        char[] password;

        // Iteration count = 1
        // Pass phrase = "password"
        // Salt = "ATHENA.MIT.EDUraeburn"
        // 256-bit PBKDF2 output:
        //   cd ed b5 28 1b b2 f8 01 56 5a 11 22 b2 56 35 15
        //   0a d1 f7 a0 4b b9 f3 a3 33 ec c0 e2 e1 f7 08 37
        edk = Util.toBytesFromString(
            "cdedb5281bb2f801565a1122b25635150ad1f7a04bb9f3a333ecc0e2e1f70837");
        password = "password".toCharArray();
        salt = "ATHENA.MIT.EDUraeburn".getBytes();
        attr.put(IPBE.ITERATION_COUNT, new Integer(1));
        attr.put(IPBE.PASSWORD, password);
        attr.put(IPBE.SALT, salt);
        try
          {
            kdf.init(attr);
            kdf.nextBytes(dk, 0, dk.length);
            harness.check(Arrays.equals(dk, edk));
          }
        catch (Exception x)
          {
            harness.debug(x);
            harness.fail(x.toString());
          }

        // Iteration count = 2
        // Pass phrase = "password"
        // Salt="ATHENA.MIT.EDUraeburn"
        // 256-bit PBKDF2 output:
        //   01 db ee 7f 4a 9e 24 3e 98 8b 62 c7 3c da 93 5d
        //   a0 53 78 b9 32 44 ec 8f 48 a9 9e 61 ad 79 9d 86
        edk = Util.toBytesFromString(
            "01dbee7f4a9e243e988b62c73cda935da05378b93244ec8f48a99e61ad799d86");
        attr.put(IPBE.ITERATION_COUNT, new Integer(2));
        try
          {
            kdf.init(attr);
            kdf.nextBytes(dk, 0, dk.length);
            harness.check(Arrays.equals(dk, edk));
          }
        catch (Exception x)
          {
            harness.debug(x);
            harness.fail(x.toString());
          }

        // Iteration count = 1200
        // Pass phrase = "password"
        // Salt = "ATHENA.MIT.EDUraeburn"
        // 256-bit PBKDF2 output:
        //   5c 08 eb 61 fd f7 1e 4e 4e c3 cf 6b a1 f5 51 2b
        //   a7 e5 2d db c5 e5 14 2f 70 8a 31 e2 e6 2b 1e 13
        edk = Util.toBytesFromString(
            "5c08eb61fdf71e4e4ec3cf6ba1f5512ba7e52ddbc5e5142f708a31e2e62b1e13");
        attr.put(IPBE.ITERATION_COUNT, new Integer(1200));
        attr.put(IPBE.PASSWORD, password);
        attr.put(IPBE.SALT, salt);
        try
          {
            kdf.init(attr);
            kdf.nextBytes(dk, 0, dk.length);
            harness.check(Arrays.equals(dk, edk));
          }
        catch (Exception x)
          {
            harness.debug(x);
            harness.fail(x.toString());
          }

        // Iteration count = 5
        // Pass phrase = "password"
        // Salt=0x1234567878563412
        // 256-bit PBKDF2 output:
        //   d1 da a7 86 15 f2 87 e6 a1 c8 b1 20 d7 06 2a 49
        //
        edk = Util.toBytesFromString(
            "d1daa78615f287e6a1c8b120d7062a493f98d203e6be49a6adf4fa574b6e64ee");
        salt = Util.toBytesFromString("1234567878563412");
        attr.put(IPBE.ITERATION_COUNT, new Integer(5));
        attr.put(IPBE.PASSWORD, password);
        attr.put(IPBE.SALT, salt);
        try
          {
            kdf.init(attr);
            kdf.nextBytes(dk, 0, dk.length);
            harness.check(Arrays.equals(dk, edk));
          }
        catch (Exception x)
          {
            harness.debug(x);
            harness.fail(x.toString());
          }

        // Iteration count = 1200
        // Pass phrase = (64 characters)
        // "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
        // Salt="pass phrase equals block size"
        // 256-bit PBKDF2 output:
        //   13 9c 30 c0 96 6b c3 2b a5 5f db f2 12 53 0a c9
        //   c5 ec 59 f1 a4 52 f5 cc 9a d9 40 fe a0 59 8e d1
        edk = Util.toBytesFromString(
            "139c30c0966bc32ba55fdbf212530ac9c5ec59f1a452f5cc9ad940fea0598ed1");
        password =
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
            .toCharArray();
        salt = "pass phrase equals block size".getBytes();
        attr.put(IPBE.ITERATION_COUNT, new Integer(1200));
        attr.put(IPBE.PASSWORD, password);
        attr.put(IPBE.SALT, salt);
        try
          {
            kdf.init(attr);
            kdf.nextBytes(dk, 0, dk.length);
            harness.check(Arrays.equals(dk, edk));
          }
        catch (Exception x)
          {
            harness.debug(x);
            harness.fail(x.toString());
          }

        // Iteration count = 1200
        // Pass phrase = (65 characters)
        // "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
        // Salt = "pass phrase exceeds block size"
        // 256-bit PBKDF2 output:
        //   9c ca d6 d4 68 77 0c d5 1b 10 e6 a6 87 21 be 61
        //   1a 8b 4d 28 26 01 db 3b 36 be 92 46 91 5e c8 2a
        edk = Util.toBytesFromString(
            "9ccad6d468770cd51b10e6a68721be611a8b4d282601db3b36be9246915ec82a");
        password =
            "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
            .toCharArray();
        salt = "pass phrase exceeds block size".getBytes();
        attr.put(IPBE.ITERATION_COUNT, new Integer(1200));
        attr.put(IPBE.PASSWORD, password);
        attr.put(IPBE.SALT, salt);
        try
          {
            kdf.init(attr);
            kdf.nextBytes(dk, 0, dk.length);
            harness.check(Arrays.equals(dk, edk));
          }
        catch (Exception x)
          {
            harness.debug(x);
            harness.fail(x.toString());
          }
      }
    catch (Exception x)
      {
        x.printStackTrace(System.err);
      }
  }
}