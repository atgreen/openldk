/* TestOfPR28556.java
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

package gnu.testlet.gnu.java.security.key.rsa;

import gnu.java.security.util.Util;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.security.KeyFactory;
import java.security.spec.PKCS8EncodedKeySpec;

/**
 * Regression test for PR Classpath/28556
 */
public class TestOfPR28556
    implements Testlet
{
  private static final String RSA_KEY_DER =
      "\u3082\u0278\u0201\u0030\u0d06\u092a\u8648\u86f7"
    + "\u0d01\u0101\u0500\u0482\u0262\u3082\u025e\u0201"
    + "\u0002\u8181\u00b9\u616a\uf64f\uc22e\u1221\uf11b"
    + "\u34f7\u3198\uc49d\u5e2c\u1298\ue268\u3068\udfeb"
    + "\u49cd\u08e7\ud0e6\uc079\udca5\uc084\uacb1\u7dc9"
    + "\ufd7c\u2c13\uf78f\u2083\ua681\uebaf\u59e2\uff43"
    + "\ud6cc\u63f1\u55b4\u2a1e\u2d90\u2728\uc6b5\ueddb"
    + "\u9eac\u5bce\u7edf\ude12\ubb79\u11de\u1305\ua474"
    + "\uf074\ue0c9\u54e8\u216d\u422a\u1e49\u1810\u7b9f"
    + "\ube8f\u03b1\u4cbb\u868b\uf161\uda93\ue8e2\uca0e"
    + "\udb2c\ubc0c\u0102\u0301\u0001\u0281\u8100\ua986"
    + "\ua8e6\u2ef2\u0867\u949a\u84a2\udf1b\u7ff4\ued64"
    + "\u5d31\u7496\u3769\u6dbe\ub7d0\u79ac\u1732\u3692"
    + "\uf5de\u0dc6\u0c8e\u5092\u13d1\ub768\u27aa\u503d"
    + "\u0fa7\u8950\u1abf\u3c92\ueb5a\ud6f6\ude8e\u4e3e"
    + "\uda86\uc782\uc550\u24d6\ue3be\u87a2\u03d9\u260e"
    + "\u9b30\u245f\uf458\u02ac\uc824\uf547\u7bee\u73e2"
    + "\u767c\ude28\ue10b\u479d\ufc39\uc52c\u67d1\u7d19"
    + "\u22a1\u26de\uf7cd\u5bc7\uc9f5\uf4f0\u2771\u0241"
    + "\u00e8\u3e09\u11bd\ufd4d\u53f6\u7e86\u42c7\u208c"
    + "\ufb33\u8cfa\u6ffd\u984a\u2615\u5146\uba79\u7fd5"
    + "\u95f0\u3fff\u85d0\ue984\u90b7\u544f\u3a74\u2ee7"
    + "\u7f54\uc2ca\u9d5b\uf626\u7714\u4565\ua303\uff6f"
    + "\u3702\u4100\ucc58\u2a56\uf26e\ub17d\u54f9\ud8ee"
    + "\ub386\u9fe6\ub213\u68b5\u01ec\uaa90\u8f0c\uff03"
    + "\u067b\u9586\u7b8f\u0799\u2a62\u0830\u4f27\u2092"
    + "\udc75\u38cd\u5d03\ub139\u3108\ub4f2\ubd5b\ue2e4"
    + "\uf471\uca87\u0241\u00dc\u3aaf\u98fe\u842c\u8719"
    + "\u7143\uda21\u4051\ud088\u4300\udda0\u2a80\uedfa"
    + "\u3b17\u8a0f\u5b54\uec19\u6666\ue5bb\u8525\uaba1"
    + "\uddb6\u3fe5\u1af1\u75c2\ua7f1\u4125\u8a97\u5146"
    + "\u8cc4\u63c0\u8fc2\u2302\u4045\u0389\ud92f\uabbe"
    + "\ufa2b\u56ee\ub33f\ua2ba\u227a\u0620\u18f1\ufb72"
    + "\u67bc\u4891\u5ffe\u3282\uff96\u7f69\ufb8a\udaed"
    + "\u1513\uc68d\u33cc\u8d32\u8ff9\u5823\ue4c2\uf0c3"
    + "\udc2f\ua3f6\uef88\ub75d\uc502\u4100\ucef4\udc8e"
    + "\u88b5\ud482\uf24f\udc2d\u5320\u0705\u5c23\ua799"
    + "\ua366\uf5d8\ube2a\ub841\uecae\uec7a\u6a7d\uee48"
    + "\u2e6b\u6d17\u816c\u2945\u53d1\u931b\ue7c6\u2720"
    + "\u6b8d\ud887\u7d9e\ud38b\ue11f\u49e2";

  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfPR28556");
    KeyFactory kf;
    PKCS8EncodedKeySpec spec;
    try
      {
        kf = KeyFactory.getInstance("RSA");
        spec = new PKCS8EncodedKeySpec(Util.toBytesFromUnicode(RSA_KEY_DER));
        kf.generatePrivate(spec);
        harness.check(true,
                      "MUST be able to parse generated OpenSSL RSA private key");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfPR28556: " + x);
      }
  }
}
