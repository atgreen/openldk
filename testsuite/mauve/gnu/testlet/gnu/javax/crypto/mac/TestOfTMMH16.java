/* TestOfTMMH16.java -- 
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

package gnu.testlet.gnu.javax.crypto.mac;

import gnu.java.security.prng.BasePRNG;
import gnu.java.security.prng.IRandom;
import gnu.java.security.prng.LimitReachedException;
import gnu.javax.crypto.mac.IMac;
import gnu.javax.crypto.mac.TMMH16;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

/**
 * Conformance test for the {@link TMMH16} implementation.
 */
public class TestOfTMMH16 implements Testlet
{
  private IRandom keystream;

  private byte[] output, message, result;

  private IMac mac;

  private HashMap attributes = new HashMap();

  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfTMMH16");

    /*
     KEY_LENGTH: 10
     TAG_LENGTH: 2
     key: { 0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc }
     message: { 0xca, 0xfe, 0xba, 0xbe, 0xba, 0xde }
     output: { 0x9d, 0x6a }
     */
    try
      {
        attributes.clear();
        keystream = new DummyKeystream();
        keystream.init(null);

        output = new byte[] { (byte) 0x9d, (byte) 0x6a };
        mac = new TMMH16();
        attributes.put(TMMH16.KEYSTREAM, keystream);
        attributes.put(TMMH16.TAG_LENGTH, new Integer(2));
        mac.init(attributes);
        message = new byte[] { (byte) 0xca, (byte) 0xfe, (byte) 0xba,
                              (byte) 0xbe, (byte) 0xba, (byte) 0xde };
        for (int i = 0; i < message.length; i++)
          {
            mac.update(message[i]);
          }
        result = mac.digest();
        harness.check(Arrays.equals(result, output), "testVector1");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfTMMH16.testVector1");
      }

    /*
     KEY_LENGTH: 10
     TAG_LENGTH: 2
     key: { 0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc }
     message: { 0xca, 0xfe, 0xba }
     output: { 0xc8, 0x8e }
     */
    try
      {
        attributes.clear();
        keystream = new DummyKeystream();
        keystream.init(null);

        output = new byte[] { (byte) 0xc8, (byte) 0x8e };
        mac = new TMMH16();
        attributes.put(TMMH16.KEYSTREAM, keystream);
        attributes.put(TMMH16.TAG_LENGTH, new Integer(2));
        mac.init(attributes);
        message = new byte[] { (byte) 0xca, (byte) 0xfe, (byte) 0xba };
        for (int i = 0; i < message.length; i++)
          {
            mac.update(message[i]);
          }
        result = mac.digest();
        harness.check(Arrays.equals(result, output), "testVector2");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfTMMH16.testVector2");
      }

    /*
     KEY_LENGTH: 10
     TAG_LENGTH: 4
     key: { 0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xfe, 0xdc }
     message: { 0xca, 0xfe, 0xba, 0xbe, 0xba, 0xde }
     output: { 0x9d, 0x6a, 0xc0, 0xd3 }
     */
    try
      {
        attributes.clear();
        keystream = new DummyKeystream();
        keystream.init(null);

        output = new byte[] { (byte) 0x9d, (byte) 0x6a, (byte) 0xc0,
                             (byte) 0xd3 };
        mac = new TMMH16();
        attributes.put(TMMH16.KEYSTREAM, keystream);
        attributes.put(TMMH16.TAG_LENGTH, new Integer(4));
        mac.init(attributes);
        message = new byte[] { (byte) 0xca, (byte) 0xfe, (byte) 0xba,
                              (byte) 0xbe, (byte) 0xba, (byte) 0xde };
        for (int i = 0; i < message.length; i++)
          {
            mac.update(message[i]);
          }
        result = mac.digest();
        harness.check(Arrays.equals(result, output), "testVector3");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfTMMH16.testVector3");
      }
  }

  class DummyKeystream extends BasePRNG
  {
    DummyKeystream()
    {
      super("???");
    }

    public Object clone()
    {
      return null;
    }

    public void setup(Map attributes)
    {
    }

    public void fillBlock() throws LimitReachedException
    {
      buffer = new byte[] {
          (byte) 0x01, (byte) 0x23, (byte) 0x45, (byte) 0x67,
          (byte) 0x89, (byte) 0xab, (byte) 0xcd, (byte) 0xef,
          (byte) 0xfe, (byte) 0xdc };
    }
  }
}