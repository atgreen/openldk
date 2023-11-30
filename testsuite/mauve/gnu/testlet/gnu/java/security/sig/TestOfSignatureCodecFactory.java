/* TestOfSignatureCodecFactory.java
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

package gnu.testlet.gnu.java.security.sig;

import java.util.Iterator;

import gnu.java.security.Registry;
import gnu.java.security.sig.ISignatureCodec;
import gnu.java.security.sig.SignatureCodecFactory;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class TestOfSignatureCodecFactory
    implements Testlet
{
  public void test(TestHarness harness)
  {
    testGetNames(harness);
    testSpecificNames(harness);
  }

  private void testGetNames(TestHarness harness)
  {
    harness.checkPoint("testGetNames");

    for (Iterator it = SignatureCodecFactory.getNames().iterator(); it.hasNext();)
      getCodec(harness, (String) it.next());
  }

  private void testSpecificNames(TestHarness harness)
  {
    harness.checkPoint("testSpecificNames");

    getCodec(harness, Registry.DSS_SIG + "/" + Registry.RAW_ENCODING_SHORT_NAME);
    getCodec(harness, Registry.DSS_SIG + "/" + Registry.X509_ENCODING_SORT_NAME);

    getCodec(harness, Registry.RSA_PKCS1_V1_5_SIG + "-"
             + Registry.MD2_HASH + "/" + Registry.RAW_ENCODING_SHORT_NAME);
    getCodec(harness, Registry.RSA_PKCS1_V1_5_SIG + "-"
             + Registry.MD5_HASH + "/" + Registry.RAW_ENCODING_SHORT_NAME);
    getCodec(harness, Registry.RSA_PKCS1_V1_5_SIG + "-"
             + Registry.SHA160_HASH + "/" + Registry.RAW_ENCODING_SHORT_NAME);
    getCodec(harness, Registry.RSA_PKCS1_V1_5_SIG + "-"
             + Registry.SHA256_HASH + "/" + Registry.RAW_ENCODING_SHORT_NAME);
    getCodec(harness, Registry.RSA_PKCS1_V1_5_SIG + "-"
             + Registry.SHA384_HASH + "/" + Registry.RAW_ENCODING_SHORT_NAME);
    getCodec(harness, Registry.RSA_PKCS1_V1_5_SIG + "-"
             + Registry.SHA512_HASH + "/" + Registry.RAW_ENCODING_SHORT_NAME);
    getCodec(harness, Registry.RSA_PKCS1_V1_5_SIG + "-"
             + Registry.RIPEMD128_HASH + "/" + Registry.RAW_ENCODING_SHORT_NAME);
    getCodec(harness, Registry.RSA_PKCS1_V1_5_SIG + "-"
             + Registry.RIPEMD_160_HASH + "/" + Registry.RAW_ENCODING_SHORT_NAME);

    getCodec(harness, Registry.RSA_PKCS1_V1_5_SIG + "-"
             + Registry.MD2_HASH + "/" + Registry.X509_ENCODING_SORT_NAME);
    getCodec(harness, Registry.RSA_PKCS1_V1_5_SIG + "-"
             + Registry.MD5_HASH + "/" + Registry.X509_ENCODING_SORT_NAME);
    getCodec(harness, Registry.RSA_PKCS1_V1_5_SIG + "-"
             + Registry.SHA160_HASH + "/" + Registry.X509_ENCODING_SORT_NAME);
    getCodec(harness, Registry.RSA_PKCS1_V1_5_SIG + "-"
             + Registry.SHA256_HASH + "/" + Registry.X509_ENCODING_SORT_NAME);
    getCodec(harness, Registry.RSA_PKCS1_V1_5_SIG + "-"
             + Registry.SHA384_HASH + "/" + Registry.X509_ENCODING_SORT_NAME);
    getCodec(harness, Registry.RSA_PKCS1_V1_5_SIG + "-"
             + Registry.SHA512_HASH + "/" + Registry.X509_ENCODING_SORT_NAME);

    getCodec(harness, Registry.RSA_PSS_SIG + "-"
             + Registry.MD2_HASH + "/" + Registry.RAW_ENCODING_SHORT_NAME);
    getCodec(harness, Registry.RSA_PSS_SIG + "-"
             + Registry.MD5_HASH + "/" + Registry.RAW_ENCODING_SHORT_NAME);
    getCodec(harness, Registry.RSA_PSS_SIG + "-"
             + Registry.SHA160_HASH + "/" + Registry.RAW_ENCODING_SHORT_NAME);
    getCodec(harness, Registry.RSA_PSS_SIG + "-"
             + Registry.SHA256_HASH + "/" + Registry.RAW_ENCODING_SHORT_NAME);
    getCodec(harness, Registry.RSA_PSS_SIG + "-"
             + Registry.SHA384_HASH + "/" + Registry.RAW_ENCODING_SHORT_NAME);
    getCodec(harness, Registry.RSA_PSS_SIG + "-"
             + Registry.SHA512_HASH + "/" + Registry.RAW_ENCODING_SHORT_NAME);
    getCodec(harness, Registry.RSA_PSS_SIG + "-"
             + Registry.RIPEMD128_HASH + "/" + Registry.RAW_ENCODING_SHORT_NAME);
    getCodec(harness, Registry.RSA_PSS_SIG + "-"
             + Registry.RIPEMD_160_HASH + "/" + Registry.RAW_ENCODING_SHORT_NAME);
  }

  private ISignatureCodec getCodec(TestHarness harness, String codecName)
  {
    String msg = "Signature codec \"" + codecName + "\" MUST succeed";
    ISignatureCodec result = null;
    try
      {
        result = SignatureCodecFactory.getInstance(codecName);
        harness.check(result != null, msg);
        return result;
      }
    catch (RuntimeException x)
      {
        harness.debug(x);
        harness.fail(msg);
      }

    return result;
  }
}
