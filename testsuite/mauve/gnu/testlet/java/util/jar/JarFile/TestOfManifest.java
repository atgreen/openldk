/* TestOfManifest.java
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

// Tags: JDK1.4

package gnu.testlet.java.util.jar.JarFile;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.security.cert.Certificate;
import java.util.Enumeration;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

/**
 * Simple test for validating Marco Trudel's patch for parsing long file names
 * in a Jar file's manifest.
 */
public class TestOfManifest
    implements Testlet
{
  private static final String FILENAME = "jfaceSmall.jar";
  private static final String FILEPATH = "gnu#testlet#java#util#jar#JarFile#"
      + FILENAME;
  private static final String ENTRYNAME =
      "org/eclipse/jface/viewers/TreeViewer$TreeColorAndFontCollector.class";

  /* (non-Javadoc)
   * @see gnu.testlet.Testlet#test(gnu.testlet.TestHarness)
   */
  public void test(TestHarness harness)
  {
    checkManifestEntries(harness);
    checkCertificates(harness);
  }

  private void checkManifestEntries(TestHarness harness)
  {
    harness.checkPoint("checkManifestEntries");
    try
      {
        File file = harness.getResourceFile(FILEPATH);
        JarFile jarFile = new JarFile(file);
        readEntries(jarFile); // will parse the signatures
        boolean ok = readCertificates(harness, jarFile);
        harness.check(ok, "Jar entry MUST be signed");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("checkManifestEntries: " + x);
      }
  }

  /**
   * @param harness this test-harness.
   */
  private void checkCertificates(TestHarness harness)
  {
    harness.checkPoint("checkCertificates");
    try
      {
        File file = harness.getResourceFile(FILEPATH);
        JarFile jarFile = new JarFile(file, true);
        JarEntry je = jarFile.getJarEntry(ENTRYNAME);

        Certificate[] certsBefore = je.getCertificates();
        int certsBeforeCount = certsBefore == null ? 0 : certsBefore.length;
        harness.verbose("***       before: " + certsBeforeCount);
        harness.check(certsBeforeCount == 0, "Certificate count MUST be 0");

        read1Entry(jarFile, je);

        Certificate[] certsAfter = je.getCertificates();
        int certsAfterCount = certsAfter == null ? 0 : certsAfter.length;
        harness.verbose("***        after: " + certsAfterCount);
        harness.check(certsAfterCount == 1, "Certificate count MUST be 1");
        harness.check(certsBeforeCount != certsAfterCount,
                      "Certificate counts MUST NOT be the same");

        JarEntry je_ = jarFile.getJarEntry(ENTRYNAME);
        Certificate[] sameCerts = je_.getCertificates();
        int sameCertsCount = sameCerts == null ? 0 : sameCerts.length;
        harness.verbose("*** w/ new entry: " + sameCertsCount);
        harness.check(sameCertsCount == 1,
                      "Certificate count (w/ new entry) MUST be 1");
        harness.check(certsAfterCount == sameCertsCount,
                      "Certificate counts (w/ new entry) MUST be the same");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("checkCertificates: " + x);
      }
  }

  private static void readEntries(JarFile jarFile) throws Exception
  {
    for (Enumeration entries = jarFile.entries(); entries.hasMoreElements();)
      read1Entry(jarFile, (JarEntry) entries.nextElement());
  }

  private static void read1Entry(JarFile jar, JarEntry entry) throws Exception
  {
    InputStream stream = null;
    try
      {
        stream = jar.getInputStream(entry);
        byte[] ba = new byte[8192];
        int n;
        while ((n = stream.read(ba)) >= 0)
          /* keep reading */;
      }
    finally
      {
        if (stream != null)
          try
            {
              stream.close();
            }
          catch (IOException ignored)
            {
            }
      }
  }

  private boolean readCertificates(TestHarness harness, JarFile jarFile)
  {
    for (Enumeration entries = jarFile.entries(); entries.hasMoreElements();)
      {
        JarEntry entry = (JarEntry) entries.nextElement();
        if (entry.isDirectory())
          continue;
        Certificate[] certs = entry.getCertificates();
        if (certs == null || certs.length == 0) // No certificate
          {
            if (! entry.getName().startsWith("META-INF"))
              {
                harness.verbose("Entry " + entry.getName() + " in jar file "
                                + FILENAME + " does not have a certificate");
                return false;
              }
          }
      }
    return true;
  }
}
