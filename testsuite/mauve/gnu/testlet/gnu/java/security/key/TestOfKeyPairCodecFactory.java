/* TestOfKeyPairCodecFactory.java
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

package gnu.testlet.gnu.java.security.key;

import java.util.Iterator;

import gnu.java.security.Registry;
import gnu.java.security.key.IKeyPairCodec;
import gnu.java.security.key.KeyPairCodecFactory;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class TestOfKeyPairCodecFactory implements Testlet
{
  public void test(TestHarness harness)
  {
    testGetNames(harness);
    testSpecificNames(harness);
  }

  private void testGetNames(TestHarness harness)
  {
    harness.checkPoint("testGetNames");

    for (Iterator it = KeyPairCodecFactory.getNames().iterator(); it.hasNext();)
      getCodec(harness, (String) it.next());
  }

  private void testSpecificNames(TestHarness harness)
  {
    harness.checkPoint("testSpecificNames");

    getCodec(harness, Registry.DSS_KPG + "/" + Registry.RAW_ENCODING_SHORT_NAME);
    getCodec(harness, Registry.DSS_KPG + "/" + Registry.X509_ENCODING_SORT_NAME);
    getCodec(harness, Registry.DSS_KPG + "/" + Registry.PKCS8_ENCODING_SHORT_NAME);

    getCodec(harness, Registry.RSA_KPG + "/" + Registry.RAW_ENCODING_SHORT_NAME);
    getCodec(harness, Registry.RSA_KPG + "/" + Registry.X509_ENCODING_SORT_NAME);
    getCodec(harness, Registry.RSA_KPG + "/" + Registry.PKCS8_ENCODING_SHORT_NAME);

    getCodec(harness, Registry.DH_KPG + "/" + Registry.RAW_ENCODING_SHORT_NAME);
    getCodec(harness, Registry.DH_KPG + "/" + Registry.X509_ENCODING_SORT_NAME);
    getCodec(harness, Registry.DH_KPG + "/" + Registry.PKCS8_ENCODING_SHORT_NAME);

    getCodec(harness, Registry.SRP_KPG + "/" + Registry.RAW_ENCODING_SHORT_NAME);
  }

  private IKeyPairCodec getCodec(TestHarness harness, String codecName)
  {
    String msg = "Key-pair codec \"" + codecName + "\" MUST succeed";
    IKeyPairCodec result = null;
    try
      {
        result = KeyPairCodecFactory.getInstance(codecName);
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
