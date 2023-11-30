/* TestOfMessageDigest.java -- 
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

package gnu.testlet.gnu.java.security.jce;

import gnu.java.security.Registry;
import gnu.java.security.hash.HashFactory;
import gnu.java.security.hash.IMessageDigest;
import gnu.java.security.provider.Gnu;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.security.MessageDigest;
import java.security.Security;
import java.util.Arrays;
import java.util.Iterator;

/**
 * Conformance tests for the JCE Provider implementations of MessageDigest
 * SPI classes.
 */
public class TestOfMessageDigest implements Testlet
{
  public void test(TestHarness harness)
  {
    setUp();

    testUnknownHash(harness);
    testEquality(harness);
    testCloneability(harness);
  }

  /** Should fail with an unknown algorithm. */
  public void testUnknownHash(TestHarness harness)
  {
    harness.checkPoint("testUnknownHash");
    try
      {
        MessageDigest.getInstance("Gaudot", Registry.GNU_SECURITY);
        harness.fail("testUnknownHash()");
      }
    catch (Exception x)
      {
        harness.check(true);
      }
  }

  /**
   * Tests if the result of using a hash through gnu.crypto Factory classes
   * yields same value as using instances obtained the JCE way.
   */
  public void testEquality(TestHarness harness)
  {
    harness.checkPoint("testEquality");
    String mdName;
    IMessageDigest gnu = null;
    MessageDigest jce = null;
    byte[] in = this.getClass().getName().getBytes();
    byte[] ba1, ba2;
    for (Iterator it = HashFactory.getNames().iterator(); it.hasNext();)
      {
        mdName = (String) it.next();
        try
          {
            gnu = HashFactory.getInstance(mdName);
            harness.check(gnu != null, "HashFactory.getInstance(" + mdName
                                       + ")");
          }
        catch (InternalError x)
          {
            harness.fail("HashFactory.getInstance(" + mdName + "): "
                         + String.valueOf(x));
          }

        try
          {
            jce = MessageDigest.getInstance(mdName, Registry.GNU_SECURITY);
            harness.check(jce != null, "MessageDigest.getInstance()");
          }
        catch (Exception x)
          {
            harness.debug(x);
            harness.fail("MessageDigest.getInstance(" + mdName + "): "
                         + String.valueOf(x));
          }

        gnu.update(in, 0, in.length);
        ba1 = gnu.digest();
        ba2 = jce.digest(in);

        harness.check(Arrays.equals(ba1, ba2), "testEquality(" + mdName + ")");
      }
  }

  /**
   * Tests if the result of a cloned, partially in-progress hash instance, when
   * used later to further process data, yields the same result as the original
   * copy.
   */
  public void testCloneability(TestHarness harness)
  {
    harness.checkPoint("testCloneability");
    String mdName;
    MessageDigest md1, md2;
    byte[] abc = "abc".getBytes();
    byte[] in = this.getClass().getName().getBytes();
    byte[] ba1, ba2;
//    for (Iterator it = Gnu.getMessageDigestNames().iterator(); it.hasNext();)
    for (Iterator it = Security.getAlgorithms("MessageDigest").iterator(); it.hasNext();)
      {
        mdName = (String) it.next();
        try
          {
            md1 = MessageDigest.getInstance(mdName, Registry.GNU_SECURITY);

            md1.update(abc); // start with abc
            md2 = (MessageDigest) md1.clone(); // now clone it

            ba1 = md1.digest(in); // now finish both with in
            ba2 = md2.digest(in);

            harness.check(Arrays.equals(ba1, ba2), "testCloneability(" + mdName
                                                   + ")");
          }
        catch (Exception x)
          {
            harness.debug(x);
            harness.fail("testCloneability(" + mdName + "): "
                         + String.valueOf(x));
          }
      }
  }

  private void setUp()
  {
    Security.addProvider(new Gnu()); // dynamically adds our provider
  }
}