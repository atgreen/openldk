/* SimpleX509TrustManager.java -- trust manager for testing.
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

import java.io.ByteArrayInputStream;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;

import javax.net.ssl.X509TrustManager;

/**
 * @author Casey Marshall (csm@gnu.org)
 */
public class SimpleX509TrustManager
  implements X509TrustManager
{
  static final String CA_CERT =
    "-----BEGIN CERTIFICATE-----\n" +
    "MIIEjDCCA3SgAwIBAgIJAMOUZM/pKkaNMA0GCSqGSIb3DQEBBQUAMIGKMREwDwYD\n" +
    "VQQDEwhNYXV2ZSBDQTELMAkGA1UEBhMCVVMxFjAUBgNVBAgTDU1hc3NhY2h1c2V0\n" +
    "dHMxDzANBgNVBAcTBkJvc3RvbjEOMAwGA1UEChMFTWF1dmUxLzAtBgkqhkiG9w0B\n" +
    "CQEWIG1hdXZlLWRpc2N1c3NAc291cmNlcy5yZWRoYXQuY29tMB4XDTA2MDcxNzAz\n" +
    "MDg0OVoXDTExMDcxNjAzMDg0OVowgYoxETAPBgNVBAMTCE1hdXZlIENBMQswCQYD\n" +
    "VQQGEwJVUzEWMBQGA1UECBMNTWFzc2FjaHVzZXR0czEPMA0GA1UEBxMGQm9zdG9u\n" +
    "MQ4wDAYDVQQKEwVNYXV2ZTEvMC0GCSqGSIb3DQEJARYgbWF1dmUtZGlzY3Vzc0Bz\n" +
    "b3VyY2VzLnJlZGhhdC5jb20wggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIB\n" +
    "AQC1sXjS8OAxk6DmfW0sbsOadgjQsKsydQ3xRinjA7dMPYyPaoY5huE7Ur4ClHka\n" +
    "ThF7enGkQi5mIaGUD5noDoE0+2IcRqp/bIXFEvwgVFrCvG9GbG8mmoEEhdBSG0ol\n" +
    "+VUwsCmwaTbzi4erCIq6907CvVUmqB3NMLKD6blghHUCWQT2BvbWev5CGktPvsp+\n" +
    "9mDpI898Xdp+zqrVkd4vGyvFI51Fj6GdZhud6ctFBMsZApkTHTaLi3m+4LGvR4gP\n" +
    "x+5ukOWQKe/MACIna6ARVxLSiYHiusdSOOvjIWW6cSC89Lmnlqp2IjDEqObLhNjF\n" +
    "ilvfnJ2/q+WJvEDSyjvO0ywvAgMBAAGjgfIwge8wHQYDVR0OBBYEFP2xH8L1npmn\n" +
    "dwjY7nndHfo+EILFMIG/BgNVHSMEgbcwgbSAFP2xH8L1npmndwjY7nndHfo+EILF\n" +
    "oYGQpIGNMIGKMREwDwYDVQQDEwhNYXV2ZSBDQTELMAkGA1UEBhMCVVMxFjAUBgNV\n" +
    "BAgTDU1hc3NhY2h1c2V0dHMxDzANBgNVBAcTBkJvc3RvbjEOMAwGA1UEChMFTWF1\n" +
    "dmUxLzAtBgkqhkiG9w0BCQEWIG1hdXZlLWRpc2N1c3NAc291cmNlcy5yZWRoYXQu\n" +
    "Y29tggkAw5Rkz+kqRo0wDAYDVR0TBAUwAwEB/zANBgkqhkiG9w0BAQUFAAOCAQEA\n" +
    "cAQ0W4kd7mvT/yZvTxjZ9NSvXLGJvaxDoSnDgJ4OVBI+BxYyp7BfseBpywUY2vke\n" +
    "nyMwjKsrDIgkEGwJGN7s2fTbw4L8f3/RThB26uF03UUZYKBQNKeeQjNAMI8rXBnO\n" +
    "oZHARSSWbH9IdllFaaP1aB0bq6ystrwaVj2y689UY6X1/MY8gc8WaUP/C/7Lj8T6\n" +
    "eJFSuZSvNaAdaAW/G9KQNPONVWHBty7OrFK/U56JcyKg0VSc0Edd9YxWqHdByuFr\n" +
    "hM9SaKM3GMmOi3Avh59STCXqgdrKh4CE6ytGiutN1bMKrhx6xKZMZjWHzZ1Ab8Jd\n" +
    "FypiU79sLqjQamv/fV5CBw==\n" +
    "-----END CERTIFICATE-----\n";

  static final String DSA_CERT =
    "-----BEGIN CERTIFICATE-----\n" +
    "MIIFRDCCBCygAwIBAgIBATANBgkqhkiG9w0BAQUFADCBijERMA8GA1UEAxMITWF1\n" +
    "dmUgQ0ExCzAJBgNVBAYTAlVTMRYwFAYDVQQIEw1NYXNzYWNodXNldHRzMQ8wDQYD\n" +
    "VQQHEwZCb3N0b24xDjAMBgNVBAoTBU1hdXZlMS8wLQYJKoZIhvcNAQkBFiBtYXV2\n" +
    "ZS1kaXNjdXNzQHNvdXJjZXMucmVkaGF0LmNvbTAeFw0wNjA3MTcwMzEwNDdaFw0w\n" +
    "NzA3MTcwMzEwNDdaMIGHMQswCQYDVQQGEwJVUzEWMBQGA1UECBMNTWFzc2FjaHVz\n" +
    "ZXR0czEOMAwGA1UEChMFTWF1dmUxDjAMBgNVBAsTBU1hdXZlMQ8wDQYDVQQDEwZk\n" +
    "c2FrZXkxLzAtBgkqhkiG9w0BCQEWIG1hdXZlLWRpc2N1c3NAc291cmNlcy5yZWRo\n" +
    "YXQuY29tMIIBtjCCASsGByqGSM44BAEwggEeAoGBAN8IlBGWirqUwgO+vgb5gTQs\n" +
    "mDVMf9Z11TYAOP5BKTmo1lbbACqb/5UCbdlMtaSGH5lCdtso6AB+fc8Q3wUBGo/o\n" +
    "KhAvNkLnX3x/m0xMZtOcFwiip4P1hP0UxpJyU/Jb/S7/qXEEZeWg9jlphSUV2Hbh\n" +
    "8F/G1MGNfgDhMYh3g13ZAhUAn/FrrGLODw3HfRbebPMACt6mHHsCgYASlGmE1jNk\n" +
    "gefmyHqo3A/Siv6myEI2cEjgZdn/nFEQY1gpjPLSBceOPH5FaeSG4TLUjt2q6Ih1\n" +
    "/eX2tx4tdZYMjxBUGslOupYAgjPeiqIEHKHpilW5DIxDvywV4EDfTn8WfbGYVJ5q\n" +
    "TriuQbQVdtlXkfZ3k3fPSci2hJii4gADjgOBhAACgYATRBEs6kCrp+8MsPhTkb8P\n" +
    "dT8FhIVN6txvwWfBFnMzbWrn32MDdxPL5pNT3wYcwqJ5jNxFZdexpuid6JYYx6KU\n" +
    "tO/UOkDvu1XIMaJF0Auy+m+NMF5FQD9uF5d0p6CQvq0sgwrz30ss7etBdFEltOsp\n" +
    "LNDAVdMuxO54oerzZ6Z6vKOCASAwggEcMAkGA1UdEwQCMAAwHQYDVR0OBBYEFJFR\n" +
    "LmRjeyJPwYtRnzLiXMpf0MUAMIG/BgNVHSMEgbcwgbSAFP2xH8L1npmndwjY7nnd\n" +
    "Hfo+EILFoYGQpIGNMIGKMREwDwYDVQQDEwhNYXV2ZSBDQTELMAkGA1UEBhMCVVMx\n" +
    "FjAUBgNVBAgTDU1hc3NhY2h1c2V0dHMxDzANBgNVBAcTBkJvc3RvbjEOMAwGA1UE\n" +
    "ChMFTWF1dmUxLzAtBgkqhkiG9w0BCQEWIG1hdXZlLWRpc2N1c3NAc291cmNlcy5y\n" +
    "ZWRoYXQuY29tggkAw5Rkz+kqRo0wLgYJYIZIAYb4QgEEBCEWH2h0dHBzOi8vd3d3\n" +
    "LnNpYWwub3JnL2NhLWNybC5wZW0wDQYJKoZIhvcNAQEFBQADggEBABeqOYKPgeS2\n" +
    "y+z3IQwoYABlqahQAur+HOoXrZqs/XJ8YgyOWJtLkdHFyTYEo48yVZNp9zW11DMx\n" +
    "mB5ChsGTR4YBG8DvQ3ua+aZo3Sdcum7IChgUfhLTfklSV8el13rjj8DyIBv2WQrn\n" +
    "KCofgObOrDoXUaNEBGMGVC5znCoFbmdE8SsTXMtjRC+sNhRvDFpKiXzNFJOwR7v7\n" +
    "zrVC6uyaXxfQMbcSdq0Ma9yVzHCc7rZfCQOoeieX6reAxp+iC7q/I+bTC5bi0GdH\n" +
    "wPSpQ+DnpimjvrSU2ESYORxZBrdxzQHCXaoJCtCF26w5P9KkpTCavo+ERgTaiVA6\n" +
    "5Wcv1NVwcM0=\n" +
    "-----END CERTIFICATE-----\n";
  
  static final String RSA_CERT =
    "-----BEGIN CERTIFICATE-----\n" +
    "MIIELDCCAxSgAwIBAgIBAjANBgkqhkiG9w0BAQUFADCBijERMA8GA1UEAxMITWF1\n" +
    "dmUgQ0ExCzAJBgNVBAYTAlVTMRYwFAYDVQQIEw1NYXNzYWNodXNldHRzMQ8wDQYD\n" +
    "VQQHEwZCb3N0b24xDjAMBgNVBAoTBU1hdXZlMS8wLQYJKoZIhvcNAQkBFiBtYXV2\n" +
    "ZS1kaXNjdXNzQHNvdXJjZXMucmVkaGF0LmNvbTAeFw0wNjA3MTcwMzEwNDhaFw0w\n" +
    "NzA3MTcwMzEwNDhaMIGHMQswCQYDVQQGEwJVUzEWMBQGA1UECBMNTWFzc2FjaHVz\n" +
    "ZXR0czEOMAwGA1UEChMFTWF1dmUxDjAMBgNVBAsTBU1hdXZlMQ8wDQYDVQQDEwZy\n" +
    "c2FrZXkxLzAtBgkqhkiG9w0BCQEWIG1hdXZlLWRpc2N1c3NAc291cmNlcy5yZWRo\n" +
    "YXQuY29tMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQC5YWr2T8IuEiHxGzT3\n" +
    "MZjEnV4sEpjiaDBo3+tJzQjn0ObAedylwISssX3J/XwsE/ePIIOmgeuvWeL/Q9bM\n" +
    "Y/FVtCoeLZAnKMa17duerFvOft/eErt5Ed4TBaR08HTgyVToIW1CKh5JGBB7n76P\n" +
    "A7FMu4aL8WHak+jiyg7bLLwMAQIDAQABo4IBIDCCARwwCQYDVR0TBAIwADAdBgNV\n" +
    "HQ4EFgQU9Wreo4FMtz6QD0GYU3ZpYrRoB3Awgb8GA1UdIwSBtzCBtIAU/bEfwvWe\n" +
    "mad3CNjued0d+j4QgsWhgZCkgY0wgYoxETAPBgNVBAMTCE1hdXZlIENBMQswCQYD\n" +
    "VQQGEwJVUzEWMBQGA1UECBMNTWFzc2FjaHVzZXR0czEPMA0GA1UEBxMGQm9zdG9u\n" +
    "MQ4wDAYDVQQKEwVNYXV2ZTEvMC0GCSqGSIb3DQEJARYgbWF1dmUtZGlzY3Vzc0Bz\n" +
    "b3VyY2VzLnJlZGhhdC5jb22CCQDDlGTP6SpGjTAuBglghkgBhvhCAQQEIRYfaHR0\n" +
    "cHM6Ly93d3cuc2lhbC5vcmcvY2EtY3JsLnBlbTANBgkqhkiG9w0BAQUFAAOCAQEA\n" +
    "DDBAL1KQl+KZSLpN7vHOUETz8ypin+BK5DKlvHS1vhjutSYoDdZBVYauXL2BaTN8\n" +
    "WXKdf8B5ucJOtDvSs4flvKT9YHw7Jg6JgxO0efScBsqCbB6JDZv0fpagSqZUpFTj\n" +
    "MKRpWOHaqTynUCou6vdAcEtAGMV9GupwZe26qEJbvYF0gj77bfoPelQB6B7H3xEZ\n" +
    "3zUC57ViXghvCTMPtOC/FoI2NcT1FMm/ffpsKbW5q4/daWt0sicpOho97mUip5MS\n" +
    "F3VXSY3POgoHXT+5oFyuvgAh6az7GmdM3/0CpL99dGNzyHv2s5LEsd1Hcl7SKoUJ\n" +
    "cTxGbnNzFAM+bKAoKK64IQ==\n" +
    "-----END CERTIFICATE-----\n";
  
  static final X509Certificate CA;
  static final X509Certificate DSA;
  static final X509Certificate RSA;
  
  static
  {
    X509Certificate ca = null;
    X509Certificate dsa = null;
    X509Certificate rsa = null;
    try
      {
        CertificateFactory cf = CertificateFactory.getInstance("X509");
        ca = (X509Certificate) cf.generateCertificate(new ByteArrayInputStream(CA_CERT.getBytes()));
        dsa = (X509Certificate) cf.generateCertificate(new ByteArrayInputStream(DSA_CERT.getBytes()));
        rsa = (X509Certificate) cf.generateCertificate(new ByteArrayInputStream(RSA_CERT.getBytes()));
      }
    catch (Exception x)
      {
        x.printStackTrace();
      }
    CA = ca;
    DSA = dsa;
    RSA = rsa;
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.X509TrustManager#checkClientTrusted(java.security.cert.X509Certificate[], java.lang.String)
   */
  public void checkClientTrusted(X509Certificate[] arg0, String arg1)
    throws CertificateException
  {
    if (DSA.equals(arg0[0]) || RSA.equals(arg0[0]))
      return;
    throw new CertificateException();
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.X509TrustManager#checkServerTrusted(java.security.cert.X509Certificate[], java.lang.String)
   */
  public void checkServerTrusted(X509Certificate[] arg0, String arg1)
    throws CertificateException
  {
    if (DSA.equals(arg0[0]) || RSA.equals(arg0[0]))
      return;
    throw new CertificateException();
  }

  /* (non-Javadoc)
   * @see javax.net.ssl.X509TrustManager#getAcceptedIssuers()
   */
  public X509Certificate[] getAcceptedIssuers()
  {
    return new X509Certificate[] { CA };
  }
}
