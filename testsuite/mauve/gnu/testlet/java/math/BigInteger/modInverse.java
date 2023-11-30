// $Id: modInverse.java,v 1.2 2003/02/17 19:48:54 raif Exp $
//
// Copyright (C) 2002 Free Software Foundation, Inc.
//
// This file is part of Mauve.
//
// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

// Tags: JDK1.1

package gnu.testlet.java.math.BigInteger;

import java.math.BigInteger;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
 * Test of <code>modeInv()</code> and subordinate methods.
 *
 * @version $Revision: 1.2 $
 */
public class modInverse implements Testlet
{
  private static final BigInteger e = BigInteger.valueOf(65537L); // 0x10001
  private BigInteger p, q, phi;
  private String[] ps = {
    "c6c93915786185fa7ee88f1983cc8d238cd6c5f7a36416c8be317c3df475277784ae1b87b263b88a84d5bacaf63798474ffe490fa412cb437abe5908efbe41b1",
    "f2d6323e96c9ad655ab520dccbc5bdf3312dcf4e32858650caa21d7e8c7ed6d13d8bbe166e0ac7cb787ef38bec6c55529f3f93b0d7c9e5ceb5188571699619bf",
    "e50fce1d57633704798f7b2465ddccebf6e5c9f22a8e3017a39f8de7cb3b78285003dca54bf9c7a2c478add7cfd7cf678b831be1db331f2f3961435c6937a545",
    "a9782bf45cdb460875a56c89b580df3f959f33e07ea43ec166241c5add827303815ab0131b7e98430038aed9e136b83e1a82d099bb40a26ac9497ef3abb58dfd",
//    "b668a33af6c9b7b39b0116c4f10c023a1dd26e320da8a4fd5ed16ad3c078353fb3effd729911bc4616e6936b8f431a64e955e994b568f3ce260d00ab78a099f7",
    "d265038c4fee2f3f87c8a2e15c1fa67dfac4ad5eb78bec468d9df27ffe3224581a2a189f87946a012a228f579abfb0d183e99cd831341af9b750b4582236e15d",
    "ca911176fce31e4332ec9ada6fa268f6ea1a9a71c81599a77797d74d5c7c48491fafce22428c516d7318c36907aa76df89e92be5ab66b42b25be777640ecc76d",
    "eb97f1e80a81d9b725dd5708fe7d65ab5339d7a339c703ee73de339fb0f10a4d76bd827536b9f6da49507ee12ca37b8157f8103f3d12a9eb9468576d9b2ef59f"
  };
  private String[] qs = {
    "bc5e04097e88241c2e9f145a829c158bacb17756b0c6aba175318c4b0b799067a83509dc45fb34c82aa7d3caacc80f1d0013c9bdd24bd52f31f04edfa169ef75",
    "da554d942ebe105e7a60070bfcaf3953f29ecfd6493aac69c6427a00be66c978515e7222180cc84606bcf7348c8aba0f9b05870cf2ab1c3669199c4316d40669",
    "ceb5591d98f1e1bfe3095f21a7e7c47d18bfcfbb8e0a1971a13941bd4cc2c861c2ef4b85cdf52b6aaeeb20264456b3c3c2a7f6a52b21eb91276acb3caa3603d1",
    "d47c206d19142ad870648eb09ca183cf4875f8009d91fcc0e085ac65455caf17ee5e91f2ccb564a88a8d13100faf1c95c6481c1b2e3fb6483f1bcdb2894356ad",
//    "e5479f61bb2745bef20003071d5355d3f3c67832c4e1db7ffe976c0c02fb28cd126f5cad75b624015ae181e48eed42ec1905509667588371e8e003b8b718984f",
    "cc36f153789677c45232afdaed78f2a20658f53fcbaa0626f64d0fa29a6f70516420999fee96dca6d232c644b09d1e27cdc0215fcbc4c36a5c493f2e1fed7bb1",
    "e63821b08b4bcc12e80a3e019f4f424c20aa72b426fc912bb2157569f9ee4422f970bbc4bf75ac05e77e48d436ce980e0646c2ba3eafb9e98aff77e19b59257f",
    "d8e26d53f31a647889ce845e892b076e578f0a68565005d5d23ed8a4ff8370cbb12cb41854badfe17053db1a94e754ea241ede1d879bff36b75f5fa96eb64927"
  };

  public void test(TestHarness harness)
  {
    testRecursion(harness);
    for (int i = 0; i < ps.length; i++)
      testCase(harness, i, ps[i], qs[i]);
  }

  private void testRecursion(TestHarness harness)
  {
    harness.checkPoint("testRecursion");
    try
    {
      BigInteger k = new BigInteger("1a1eb1e6b8f115eee3dc1334afc7de2f7efbd568", 16);
      BigInteger q = new BigInteger("de09f1902cf484f232fee5d27262372d1c6072d7", 16);
      BigInteger t = new BigInteger("a3a790f0b7d2bea3a81dc676032cf99c23c28bee", 16);
      BigInteger tt = k.modInverse(q);
//      harness.check(true, "k.modInverse(q)="+tt.toString(16));
      harness.check(t.equals(k.modInverse(q)), "recursion ok");
    }
    catch (Exception x)
    {
      harness.debug(x);
      harness.fail(String.valueOf(x));
    }
  }

  private void testCase(TestHarness harness, int i, String ps, String qs)
  {
    harness.checkPoint("modInverse test #"+(i+1));
    try
    {
      p = new BigInteger(ps, 16);
      q = new BigInteger(qs, 16);
      harness.check(p.isProbablePrime(50), "p["+i+"] is probable prime");
      harness.check(p.gcd(e).equals(BigInteger.ONE), "gcd(p["+i+"], e) == 1");
      harness.check(q.isProbablePrime(50), "q["+i+"] is probable prime");
      harness.check(q.gcd(e).equals(BigInteger.ONE), "gcd(q["+i+"], e) == 1");
      phi = p.subtract(BigInteger.ONE).multiply(q.subtract(BigInteger.ONE));
      e.modInverse(phi);
    }
    catch (ArithmeticException x)
    {
      harness.debug(x);
      harness.fail(String.valueOf(x));
    }
  }
}
