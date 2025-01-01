/* SimpleX509KeyManager.java -- key manager for testing.
   Copyright (C) 2006  Casey Marshall <csm@gnu.org>

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

// Tags: not-a-test

package gnu.testlet.javax.net.ssl.SSLEngine;

import gnu.java.security.key.dss.DSSPrivateKey;
import gnu.java.security.key.rsa.GnuRSAPrivateKey;

import java.math.BigInteger;
import java.net.Socket;
import java.security.KeyFactory;
import java.security.Principal;
import java.security.PrivateKey;
import java.security.cert.X509Certificate;
import java.security.spec.PKCS8EncodedKeySpec;

import javax.net.ssl.SSLEngine;
import javax.net.ssl.X509ExtendedKeyManager;

/**
 * @author Casey Marshall (csm@gnu.org)
 */
public class SimpleX509KeyManager
  extends X509ExtendedKeyManager
{

  /* (non-Javadoc)
   * @see javax.net.ssl.X509KeyManager#chooseClientAlias(java.lang.String[], java.security.Principal[], java.net.Socket)
   */
  public String chooseClientAlias(String[] arg0, Principal[] arg1, Socket arg2)
  {
    for (int i = 0; i < arg0.length; i++)
      {
        if (arg0[i].equalsIgnoreCase("rsa_sign"))
          return "rsakey";
        if (arg0[i].equalsIgnoreCase("dss_sign"))
          return "dsskey";
      }
    return null;
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.X509KeyManager#chooseServerAlias(java.lang.String, java.security.Principal[], java.net.Socket)
   */
  public String chooseServerAlias(String arg0, Principal[] arg1, Socket arg2)
  {
    if (arg0.equalsIgnoreCase("DHE_RSA")
        || arg0.equalsIgnoreCase("SRP_RSA")
        || arg0.equalsIgnoreCase("RSA")
        || arg0.equalsIgnoreCase("RSA_PSK"))
      return "rsakey";
    if (arg0.equalsIgnoreCase("DHE_DSS")
        || arg0.equalsIgnoreCase("SRP_DSS"))
      return "dsakey";
    return null;
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.X509KeyManager#getCertificateChain(java.lang.String)
   */
  public X509Certificate[] getCertificateChain(String arg0)
  {
    if (arg0.equals("rsakey"))
      return new X509Certificate[] { SimpleX509TrustManager.RSA };
    if (arg0.equals("dsakey"))
      return new X509Certificate[] { SimpleX509TrustManager.DSA };
    return null;
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.X509KeyManager#getClientAliases(java.lang.String, java.security.Principal[])
   */
  public String[] getClientAliases(String arg0, Principal[] arg1)
  {
    if (arg0.equalsIgnoreCase("rsa_sign"))
      return new String[] { "rsakey" };
    if (arg0.equalsIgnoreCase("dss_sign"))
      return new String[] { "dsakey" };
    return new String[0];
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.X509KeyManager#getPrivateKey(java.lang.String)
   */
  public PrivateKey getPrivateKey(String arg0)
  {
    if (arg0.equals("dsakey"))
      return DSAKEY;
    if (arg0.equals("rsakey"))
      return RSAKEY;
    return null;
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.X509KeyManager#getServerAliases(java.lang.String, java.security.Principal[])
   */
  public String[] getServerAliases(String arg0, Principal[] arg1)
  {
    if (arg0.equalsIgnoreCase("DHE_RSA")
        || arg0.equalsIgnoreCase("SRP_RSA")
        || arg0.equalsIgnoreCase("RSA")
        || arg0.equalsIgnoreCase("RSA_PSK"))
      return new String[] { "rsakey" };
    if (arg0.equalsIgnoreCase("DHE_DSS")
        || arg0.equalsIgnoreCase("SRP_DSS"))
      return new String[] { "dsakey" };
    return new String[0];
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.X509ExtendedKeyManager#chooseEngineClientAlias(java.lang.String[], java.security.Principal[], javax.net.ssl.SSLEngine)
   */
  public String chooseEngineClientAlias(String[] arg0, Principal[] arg1, SSLEngine arg2)
  {
    for (int i = 0; i < arg0.length; i++)
      {
        if (arg0[i].equalsIgnoreCase("rsa_sign"))
          return "rsakey";
        if (arg0[i].equalsIgnoreCase("dss_sign"))
          return "dsskey";
      }
    return null;
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.X509ExtendedKeyManager#chooseEngineServerAlias(java.lang.String, java.security.Principal[], javax.net.ssl.SSLEngine)
   */
  public String chooseEngineServerAlias(String arg0, Principal[] arg1, SSLEngine arg2)
  {
    if (arg0.equalsIgnoreCase("DHE_RSA")
        || arg0.equalsIgnoreCase("SRP_RSA")
        || arg0.equalsIgnoreCase("RSA")
        || arg0.equalsIgnoreCase("RSA_PSK"))
      return "rsakey";
    if (arg0.equalsIgnoreCase("DHE_DSS")
        || arg0.equalsIgnoreCase("SRP_DSS"))
      return "dsakey";
    return null;
  }
  
  static final PrivateKey RSAKEY = new GnuRSAPrivateKey
    (new BigInteger("00e83e0911bdfd4d53f67e8642c720" +
                    "8cfb338cfa6ffd984a26155146ba79" +
                    "7fd595f03fff85d0e98490b7544f3a" +
                    "742ee77f54c2ca9d5bf62677144565" +
                    "a303ff6f37", 16),
     new BigInteger("00cc582a56f26eb17d54f9d8eeb386" +
                    "9fe6b21368b501ecaa908f0cff0306" +
                    "7b95867b8f07992a6208304f272092" +
                    "dc7538cd5d03b1393108b4f2bd5be2" +
                    "e4f471ca87", 16),
     new BigInteger("10001", 16),
     new BigInteger("00a986a8e62ef20867949a84a2df1b" +
                    "7ff4ed645d31749637696dbeb7d079" +
                    "ac17323692f5de0dc60c8e509213d1" +
                    "b76827aa503d0fa789501abf3c92eb" +
                    "5ad6f6de8e4e3eda86c782c55024d6" +
                    "e3be87a203d9260e9b30245ff45802" +
                    "acc824f5477bee73e2767cde28e10b" +
                    "479dfc39c52c67d17d1922a126def7" +
                    "cd5bc7c9f5f4f02771", 16));
  static final PrivateKey DSAKEY = new DSSPrivateKey
    (new BigInteger("00df089411968aba94c203bebe06f9" +
                    "81342c98354c7fd675d5360038fe41" +
                    "2939a8d656db002a9bff95026dd94c" +
                    "b5a4861f994276db28e8007e7dcf10" +
                    "df05011a8fe82a102f3642e75f7c7f" +
                    "9b4c4c66d39c1708a2a783f584fd14" +
                    "c6927253f25bfd2effa9710465e5a0" +
                    "f63969852515d876e1f05fc6d4c18d" +
                    "7e00e1318877835dd9", 16),
     new BigInteger("009ff16bac62ce0f0dc77d16de6cf3000adea61c7b", 16),
     new BigInteger("12946984d6336481e7e6c87aa8dc0f" +
                    "d28afea6c842367048e065d9ff9c51" +
                    "106358298cf2d205c78e3c7e4569e4" +
                    "86e132d48eddaae88875fde5f6b71e" +
                    "2d75960c8f10541ac94eba96008233" +
                    "de8aa2041ca1e98a55b90c8c43bf2c" +
                    "15e040df4e7f167db198549e6a4eb8" +
                    "ae41b41576d95791f6779377cf49c8" +
                    "b68498a2e200038e", 16),
     new BigInteger("2564481362698611a67084e8857612b5dd8dd668", 16));
}
