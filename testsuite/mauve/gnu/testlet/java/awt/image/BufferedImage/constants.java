/* constants.java -- some checks for the constant image types in the
       BufferedImage class.
   Copyright (C) 2006  Francis Kung  <fkung@redhat.com>
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

package gnu.testlet.java.awt.image.BufferedImage;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Transparency;
import java.awt.color.ColorSpace;
import java.awt.image.BufferedImage;
import java.awt.image.ComponentColorModel;
import java.awt.image.DataBuffer;
import java.awt.image.DirectColorModel;
import java.awt.image.IndexColorModel;
import java.awt.image.MultiPixelPackedSampleModel;
import java.awt.image.PixelInterleavedSampleModel;
import java.awt.image.SinglePixelPackedSampleModel;

public class constants implements Testlet 
{
  public void test(TestHarness harness)
  {
    test_int_rgb(harness);
    test_int_argb(harness);
    test_int_argb_pre(harness);
    test_3byte_bgr(harness);
    test_4byte_abgr(harness);
    test_4byte_abgr_pre(harness);
    test_ushort_565_rgb(harness);
    test_ushort_555_rgb(harness);
    test_byte_gray(harness);
    test_ushort_gray(harness);
    test_byte_binary(harness);
    test_byte_indexed(harness);
  }
  
  public void test_int_rgb(TestHarness harness)
  {
    BufferedImage img = new BufferedImage(10, 10, BufferedImage.TYPE_INT_RGB);
    harness.check(img.getColorModel().equals(new DirectColorModel(24, 0xff0000,
                                                                  0xff00,
                                                                  0xff)));
    harness.check(img.getSampleModel().equals(new SinglePixelPackedSampleModel(DataBuffer.TYPE_INT,
                                                                               10, 10,
                                                                               new int[]{0xff0000,
                                                                                         0xff00,
                                                                                         0xff})));
    harness.check(img.isAlphaPremultiplied(), img.getColorModel().isAlphaPremultiplied());
  }

  public void test_int_argb(TestHarness harness)
  {
    BufferedImage img = new BufferedImage(10, 10, BufferedImage.TYPE_INT_ARGB);
    harness.check(img.getColorModel().equals(new DirectColorModel(32, 0xff0000,
                                                                  0xff00,
                                                                  0xff,
                                                                  0xff000000)));
    harness.check(img.getSampleModel().equals(new SinglePixelPackedSampleModel(DataBuffer.TYPE_INT,
                                                                               10, 10,
                                                                               new int[]{0xff0000,
                                                                                         0xff00,
                                                                                         0xff,
                                                                                         0xff000000})));
    harness.check(img.isAlphaPremultiplied(), img.getColorModel().isAlphaPremultiplied());
  }
  
  public void test_int_argb_pre(TestHarness harness)
  {
    BufferedImage img = new BufferedImage(10, 10, BufferedImage.TYPE_INT_ARGB_PRE);
    harness.check(img.getColorModel().equals(new DirectColorModel(ColorSpace.getInstance(ColorSpace.CS_sRGB),
                                                                  32, 0xff0000,
                                                                  0xff00, 0xff,
                                                                  0xff000000,
                                                                  true,
                                                                  DataBuffer.TYPE_INT)));
    harness.check(img.getSampleModel().equals(new SinglePixelPackedSampleModel(DataBuffer.TYPE_INT,
                                                                               10, 10,
                                                                               new int[]{0xff0000,
                                                                                         0xff00,
                                                                                         0xff,
                                                                                         0xff000000})));
    harness.check(img.isAlphaPremultiplied(), img.getColorModel().isAlphaPremultiplied());
  }
  
  public void test_int_bgr(TestHarness harness)
  {  
    BufferedImage img = new BufferedImage(10, 10, BufferedImage.TYPE_INT_BGR);
    harness.check(img.getColorModel().equals(new DirectColorModel(24, 0xff,
                                                                  0xff00,
                                                                  0xff0000)));
    harness.check(img.getSampleModel().equals(new SinglePixelPackedSampleModel(DataBuffer.TYPE_INT,
                                                                               10, 10,
                                                                               new int[]{0xff,
                                                                                         0xff00,
                                                                                         0xff0000})));
    harness.check(img.isAlphaPremultiplied(), img.getColorModel().isAlphaPremultiplied());
  }
  
  public void test_3byte_bgr(TestHarness harness)
  {
    BufferedImage img = new BufferedImage(10, 10, BufferedImage.TYPE_3BYTE_BGR);
    harness.check(img.getColorModel().equals(new ComponentColorModel(ColorSpace.getInstance(ColorSpace.CS_sRGB),
                                                                     false, false,
                                                                     BufferedImage.OPAQUE,
                                                                     DataBuffer.TYPE_BYTE)));
    harness.check(img.getSampleModel().equals(new PixelInterleavedSampleModel( DataBuffer.TYPE_BYTE,
                                                                               10, 10,
                                                                               3, 10 * 3, 
                                                                               new int[]{ 2, 1, 0 } )));
    harness.check(img.isAlphaPremultiplied(), img.getColorModel().isAlphaPremultiplied());
  }
  
  public void test_4byte_abgr(TestHarness harness)
  {
    BufferedImage img = new BufferedImage(10, 10, BufferedImage.TYPE_4BYTE_ABGR);
    harness.check(img.getColorModel().equals(new ComponentColorModel(ColorSpace.getInstance(ColorSpace.CS_sRGB),
                                                                     true, false,
                                                                     BufferedImage.TRANSLUCENT,
                                                                     DataBuffer.TYPE_BYTE)));
    harness.check(img.getSampleModel().equals(new PixelInterleavedSampleModel(DataBuffer.TYPE_BYTE, 
                                                                              10, 10,
                                                                              4, 4*10,
                                                                              new int[]{3, 2, 1, 0})));
    harness.check(img.isAlphaPremultiplied(), img.getColorModel().isAlphaPremultiplied());
  }
  
  public void test_4byte_abgr_pre(TestHarness harness)
  {
    BufferedImage img = new BufferedImage(10, 10, BufferedImage.TYPE_4BYTE_ABGR_PRE);
    harness.check(img.getColorModel().equals(new ComponentColorModel(ColorSpace.getInstance(ColorSpace.CS_sRGB),
                                                                     true, true,
                                                                     BufferedImage.TRANSLUCENT,
                                                                     DataBuffer.TYPE_BYTE)));
    harness.check(img.getSampleModel().equals(new PixelInterleavedSampleModel(DataBuffer.TYPE_BYTE, 
                                                                              10, 10,
                                                                              4, 4*10,
                                                                              new int[]{3, 2, 1, 0})));
    harness.check(img.isAlphaPremultiplied(), img.getColorModel().isAlphaPremultiplied());
  }
  
  public void test_ushort_565_rgb(TestHarness harness)
  {
    BufferedImage img = new BufferedImage(10, 10, BufferedImage.TYPE_USHORT_565_RGB);
    harness.check(img.getColorModel().equals(new DirectColorModel( 16, 0xF800, 0x7E0, 0x1F )));
    harness.check(img.getSampleModel().equals(new SinglePixelPackedSampleModel( DataBuffer.TYPE_USHORT,
                                                                                10, 10,
                                                                                new int[]{ 0xF800, 
                                                                                   0x7E0, 
                                                                                   0x1F } ) ));
    harness.check(img.isAlphaPremultiplied(), img.getColorModel().isAlphaPremultiplied());
  }
  
  public void test_ushort_555_rgb(TestHarness harness)
  {
    BufferedImage img = new BufferedImage(10, 10, BufferedImage.TYPE_USHORT_555_RGB);
    harness.check(img.getColorModel().equals(new DirectColorModel( 15, 0x7C00, 0x3E0, 0x1F )));
    harness.check(img.getSampleModel().equals(new SinglePixelPackedSampleModel( DataBuffer.TYPE_USHORT,
                                                                               10, 10,
                                                                               new int[]{ 0x7C00, 
                                                                                  0x3E0, 
                                                                                  0x1F } )));
    harness.check(img.isAlphaPremultiplied(), img.getColorModel().isAlphaPremultiplied());
  }
  
  public void test_byte_gray(TestHarness harness)
  {
    BufferedImage img = new BufferedImage(10, 10, BufferedImage.TYPE_BYTE_GRAY);
    harness.check(img.getColorModel().equals(new ComponentColorModel( ColorSpace.getInstance( ColorSpace.CS_GRAY ),
                                                                      new int[]{8}, false, false, 
                                                                      Transparency.OPAQUE, DataBuffer.TYPE_BYTE)));
    harness.check(img.getSampleModel().equals(new PixelInterleavedSampleModel( DataBuffer.TYPE_BYTE,
                                                                              10, 10,
                                                                              1, 10, new int[]{ 0 } )));
    harness.check(img.isAlphaPremultiplied(), img.getColorModel().isAlphaPremultiplied());
  }
  
  public void test_ushort_gray(TestHarness harness)
  {
    BufferedImage img = new BufferedImage(10, 10, BufferedImage.TYPE_USHORT_GRAY);
    harness.check(img.getColorModel().equals(new ComponentColorModel( ColorSpace.getInstance( ColorSpace.CS_GRAY ),
                                                                      new int[]{16}, false, false, 
                                                                      Transparency.OPAQUE, DataBuffer.TYPE_USHORT)));
    harness.check(img.getSampleModel().equals(new PixelInterleavedSampleModel( DataBuffer.TYPE_USHORT,
                                                                              10, 10,
                                                                              1, 10, new int[]{ 0 } )));
    harness.check(img.isAlphaPremultiplied(), img.getColorModel().isAlphaPremultiplied());
  }
  
  public void test_byte_binary(TestHarness harness)
  {
    BufferedImage img = new BufferedImage(10, 10, BufferedImage.TYPE_BYTE_BINARY);
    byte[] t = new byte[]{ 0, (byte)255 };
    harness.check(img.getColorModel().equals(new IndexColorModel( 1, 2, t, t, t )));
    harness.check(img.getSampleModel().equals(new MultiPixelPackedSampleModel(DataBuffer.TYPE_BYTE, 
                                                                             10, 10, 1)));
    harness.check(img.isAlphaPremultiplied(), img.getColorModel().isAlphaPremultiplied());
  }
  
  public void test_byte_indexed(TestHarness harness)
  {
    BufferedImage img = new BufferedImage(10, 10, BufferedImage.TYPE_BYTE_INDEXED);
    byte[] r = new byte[256];
    byte[] g = new byte[256];
    byte[] b = new byte[256];
    int index = 0;
    for( int i = 0; i < 6; i++ )
      for( int j = 0; j < 6; j++ )
    for( int k = 0; k < 6; k++ )
      {
        r[ index ] = (byte)(i * 51);
        g[ index ] = (byte)(j * 51);
        b[ index ] = (byte)(k * 51);
        index++;
      }
    while( index < 256 )
      {
    r[ index ] = g[ index ] = b[ index ] = 
      (byte)(18 + (index - 216) * 6);
    index++;
      }
    harness.check(img.getColorModel().equals(new IndexColorModel( 8, 256, r, g, b )));
    harness.check(img.getSampleModel().equals(new PixelInterleavedSampleModel( DataBuffer.TYPE_BYTE,
                                                                              10, 10,
                                                                              1, 10, new int[]{ 0 } )));
    harness.check(img.isAlphaPremultiplied(), img.getColorModel().isAlphaPremultiplied());
  }
}
