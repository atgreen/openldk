/* BaseInvalidTest.java -- superclass of "invalid" tests.
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL; see the file `COPYING' */

// Tags: not-a-test
// Uses: PKITS
// Files: data/certs/TrustAnchorRootCertificate.crt data/crls/TrustAnchorRootCRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

import java.security.cert.*;
import java.util.*;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public abstract class BaseInvalidTest extends PKITS implements Testlet
{

  // Fields.
  // -------------------------------------------------------------------------

  public static final String PROVIDER = System.getProperty("pkits.provider", "GNU");
  public static final String TRUST_ANCHOR_CERT = "data/certs/TrustAnchorRootCertificate.crt";
  public static final String TRUST_ANCHOR_CRL = "data/crls/TrustAnchorRootCRL.crl";

  protected String[] certPath;
  protected String[] crls;
  protected String[] certs;

  // Constructors.
  // -------------------------------------------------------------------------

  protected BaseInvalidTest(String[] certPath, String[] crls, String[] certs)
  {
    if (certPath == null || crls == null || certs == null)
      throw new NullPointerException();
    this.certPath = certPath;
    this.crls = crls;
    this.certs = certs;
  }

  protected BaseInvalidTest(String[] certPath, String[] crls)
  {
    this(certPath, crls, new String[0]);
  }

  // Instance method.
  // -------------------------------------------------------------------------

  public void test(TestHarness harness)
  {
    String testName = getClass().getName();
    if (testName.lastIndexOf ('.') > 0)
      testName = testName.substring (testName.lastIndexOf ('.') + 1);
    harness.checkPoint(testName);
    try
      {
        CertificateFactory factory = CertificateFactory.getInstance("X.509", PROVIDER);
        TrustAnchor anchor = new TrustAnchor((X509Certificate) factory.generateCertificate(getClass().getResourceAsStream(TRUST_ANCHOR_CERT)), null);
        List pathList = new ArrayList(certPath.length);
        for (int i = 0; i < certPath.length; i++)
          {
            pathList.add(factory.generateCertificate(getClass().getResourceAsStream(certPath[i])));
          }
        List crlsAndCerts = new ArrayList(crls.length + certs.length + 1);
        crlsAndCerts.add(factory.generateCRL(getClass().getResourceAsStream(TRUST_ANCHOR_CRL)));
        for (int i = 0; i < crls.length; i++)
          {
            crlsAndCerts.add(factory.generateCRL(getClass().getResourceAsStream(crls[i])));
          }
        for (int i = 0; i < certs.length; i++)
          {
            crlsAndCerts.add(factory.generateCertificate(getClass().getResourceAsStream(certs[i])));
          }
        CertPath path = factory.generateCertPath(pathList);
        CertStore certStore = CertStore.getInstance("Collection", new CollectionCertStoreParameters(crlsAndCerts), PROVIDER);
        PKIXParameters params = new PKIXParameters(Collections.singleton(anchor));
        params.addCertStore(certStore);
        params.setExplicitPolicyRequired(false);
        params.setInitialPolicies(Collections.singleton(PKITS.ANY_POLICY));
        params.setPolicyMappingInhibited(false);
        params.setAnyPolicyInhibited(false);
        setupAdditionalParams(params);
        CertPathValidator validator = CertPathValidator.getInstance("PKIX", PROVIDER);
        try
          {
            CertPathValidatorResult result = validator.validate (path, params);
            harness.verbose (((PKIXCertPathValidatorResult) result).getPolicyTree().toString());
            harness.check (false);
          }
        catch (CertPathValidatorException expected)
          {
            harness.verbose("expected failure reason is: " + expected);
            harness.check(true);
          }
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail(x.toString());
      }
  }

  /**
   * Subclasses should override this method to add any additional parameters
   * before the path verification is run.
   *
   * @param params The parameters.
   */
  protected void setupAdditionalParams(PKIXParameters params)
  {
  }
}
