/* TestOfAssembly.java -- 
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

package gnu.testlet.gnu.javax.crypto.assembly;

import gnu.java.security.Registry;
import gnu.javax.crypto.assembly.Assembly;
import gnu.javax.crypto.assembly.Cascade;
import gnu.javax.crypto.assembly.Direction;
import gnu.javax.crypto.assembly.Stage;
import gnu.javax.crypto.assembly.Transformer;
import gnu.javax.crypto.assembly.TransformerException;
import gnu.javax.crypto.cipher.Blowfish;
import gnu.javax.crypto.cipher.IBlockCipher;
import gnu.javax.crypto.mode.IMode;
import gnu.javax.crypto.mode.ModeFactory;
import gnu.javax.crypto.pad.IPad;
import gnu.javax.crypto.pad.PadFactory;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.ByteArrayOutputStream;
import java.util.Arrays;
import java.util.HashMap;

/**
 * Simple symmetry tests for 3 assembly constructions.
 */
public class TestOfAssembly implements Testlet
{
  private Assembly asm;

  private HashMap attributes = new HashMap();

  private HashMap modeAttributes = new HashMap();

  public TestOfAssembly()
  {
    super();
  }

  public void test(TestHarness harness)
  {
    TestOfAssembly testcase = new TestOfAssembly();

    // build an OFB-Blowfish cascade
    Cascade ofbBlowfish = new Cascade();
    Object modeNdx = ofbBlowfish.append(Stage.getInstance(
        ModeFactory.getInstance(
            Registry.OFB_MODE, new Blowfish(), 8),
            Direction.FORWARD));

    testcase.attributes.put(modeNdx, testcase.modeAttributes);

    IPad pkcs7 = PadFactory.getInstance(Registry.PKCS7_PAD);

    testcase.asm = new Assembly();
    testcase.asm.addPreTransformer(Transformer.getCascadeTransformer(ofbBlowfish));
    testcase.asm.addPreTransformer(Transformer.getPaddingTransformer(pkcs7));

    testcase.testSymmetry(harness, 1);

    // add a compression transformer.
    // the resulting assembly encrypts + pad first and compresses later
    //      testcase.asm = new Assembly();
    //      testcase.asm.addPreTransformer(Transformer.getCascadeTransformer(ofbBlowfish));
    //      testcase.asm.addPreTransformer(Transformer.getPaddingTransformer(pkcs7));
    testcase.asm.addPostTransformer(Transformer.getDeflateTransformer());

    testcase.testSymmetry(harness, 2);

    // now build an assembly that compresses first and encrypts + pads later
    testcase.asm = new Assembly();
    testcase.asm.addPreTransformer(Transformer.getCascadeTransformer(ofbBlowfish));
    testcase.asm.addPreTransformer(Transformer.getPaddingTransformer(pkcs7));
    testcase.asm.addPreTransformer(Transformer.getDeflateTransformer());

    testcase.testSymmetry(harness, 3);
  }

  private void testSymmetry(TestHarness harness, int ndx)
  {
    harness.checkPoint("TestOfAssembly.testSymmetry#" + ndx);

    byte[] km = new byte[] { 0, 1, 2, 3, 4, 5, 6, 7, 8 };
    byte[] iv = new byte[] { -1, -2, -3, -4, -5, -6, -7, -8, -9 };
    byte[] pt = new byte[] { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 };
    byte[] tpt = new byte[11 * pt.length];

    // forward
    modeAttributes.put(IBlockCipher.KEY_MATERIAL, km);
    modeAttributes.put(IMode.IV, iv);
    attributes.put(Assembly.DIRECTION, Direction.FORWARD);
    try
      {
        asm.init(attributes);
      }
    catch (TransformerException x)
      {
        harness.debug(x);
        harness.fail("Forward initialisation");
        return;
      }

    byte[] ct = null;
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    try
      {
        for (int i = 0; i < 10; i++)
          { // transform in parts of 12-byte a time
            System.arraycopy(pt, 0, tpt, i * pt.length, pt.length);
            ct = asm.update(pt);
            baos.write(ct, 0, ct.length);
          }
      }
    catch (TransformerException x)
      {
        harness.debug(x);
        harness.fail("Forward transformation");
        return;
      }
    try
      {
        System.arraycopy(pt, 0, tpt, 10 * pt.length, pt.length);
        ct = asm.lastUpdate(pt);
      }
    catch (TransformerException x)
      {
        harness.debug(x);
        harness.fail("Forward last transformation");
        return;
      }
    baos.write(ct, 0, ct.length);
    ct = baos.toByteArray();

    // reversed
    attributes.put(Assembly.DIRECTION, Direction.REVERSED);
    try
      {
        asm.init(attributes);
      }
    catch (TransformerException x)
      {
        harness.debug(x);
        harness.fail("Reverse initialisation");
        return;
      }

    byte[] ot;
    try
      {
        ot = asm.lastUpdate(ct); // transform the lot in one go
      }
    catch (TransformerException x)
      {
        harness.debug(x);
        harness.fail("Reverse transformation");
        return;
      }

    harness.check(Arrays.equals(ot, tpt), "symmetric test");
  }
}