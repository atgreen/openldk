// Tags: JDK1.2 

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

// This file is part of Mauve.

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version. 

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.awt.image.IndexColorModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Color;
import java.awt.Transparency;
import java.awt.image.DataBuffer;
import java.awt.image.IndexColorModel;
import java.math.BigInteger;

/**
 * Some checks for the constructors in the IndexColorModel class.
 */
public class constructors implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness) 
  {
    testConstructor1(harness);
    testConstructor2(harness);
    testConstructor3(harness);
    testConstructor4(harness);
    testConstructor5(harness);
    testConstructor6(harness);
    testConstructor7(harness);
  }   

  private static final byte[] R4 = {(byte) 1, (byte) 2, (byte) 3, (byte) 4};
  private static final byte[] G4 = {(byte) 5, (byte) 6, (byte) 7, (byte) 8};
  private static final byte[] B4 = {(byte) 9, (byte) 10, (byte) 11, (byte) 12};
  private static final byte[] A4 = {(byte) 13, (byte) 14, (byte) 15, (byte) 16};
  
  private static final byte[] CMAP = {(byte) 1, (byte) 5, (byte) 9, (byte) 2, 
        (byte) 6, (byte) 10, (byte) 3, (byte) 7, (byte) 11, (byte) 4, 
        (byte) 8, (byte) 12};
  
  private static final int[] CMAP_INT = {new Color(1, 5, 9).getRGB(), 
        new Color(2, 6, 10).getRGB(), new Color(3, 7, 11).getRGB(),
        new Color(4, 8, 12).getRGB()};
  
  private static final byte[] CMAP_WITH_ALPHA = {(byte) 1, (byte) 5, (byte) 9, 
        (byte) 13, (byte) 2, (byte) 6, (byte) 10, (byte) 14, (byte) 3, 
        (byte) 7, (byte) 11, (byte) 15, (byte) 4, (byte) 8, (byte) 12, 
        (byte) 16};
  
  private static final int[] CMAP_INT_WITH_ALPHA = {new Color(1, 5, 9, 13).getRGB(), 
        new Color(2, 6, 10, 14).getRGB(), new Color(3, 7, 11, 15).getRGB(),
        new Color(4, 8, 12, 16).getRGB()};

  private void testConstructor1(TestHarness harness) 
  {  
    harness.checkPoint("IndexColorModel(int, int, byte[], byte[], byte[])");
    IndexColorModel m1 = new IndexColorModel(2, 4, R4, G4, B4);
    harness.check(m1.getTransparency(), Transparency.OPAQUE);
    harness.check(m1.getMapSize(), 4);
    harness.check(m1.getPixelSize(), 2);
    harness.check(m1.getTransparentPixel(), -1);
    harness.check(!m1.hasAlpha());
    harness.check(!m1.isAlphaPremultiplied());
    harness.check(m1.getNumComponents(), 3);
    harness.check(m1.getNumColorComponents(), 3);
    
    // try bits < 1
    boolean pass = false;
    try
    {
      /* IndexColorModel m = */ new IndexColorModel(0, 4, R4, G4, B4);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;   
    }
    harness.check(pass);
    
    // try bits > 16
    pass = false;
    try
    {
      /* IndexColorModel m = */ new IndexColorModel(17, 4, R4, G4, B4);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;   
    }
    harness.check(pass);
    
    // try size bigger than arrays
    pass = false;
    try
    {
      /* IndexColorModel m = */ new IndexColorModel(2, 99, R4, G4, B4);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;  
    }
    harness.check(pass);
    
    // try null red array
    pass = false;
    try 
    {
      /* IndexColorModel m = */ new IndexColorModel(2, 4, null, G4, B4);      
    }
    catch (NullPointerException e) 
    {
      pass = true;   
    }
    harness.check(pass);

    // try null green array
    pass = false;
    try 
    {
      /* IndexColorModel m = */ new IndexColorModel(2, 4, R4, null, B4);      
    }
    catch (NullPointerException e) 
    {
      pass = true;   
    }
    harness.check(pass);

    // try null blue array
    pass = false;
    try 
    {
      /* IndexColorModel m = */ new IndexColorModel(2, 4, R4, G4, null);      
    }
    catch (NullPointerException e) 
    {
      pass = true;   
    }
    harness.check(pass);
  }

  private void testConstructor2(TestHarness harness) 
  {  
    harness.checkPoint("IndexColorModel(int, int, byte[], byte[], byte[], byte[])");
    IndexColorModel m1 = new IndexColorModel(2, 4, R4, G4, B4, A4);
    harness.check(m1.getTransparency(), Transparency.TRANSLUCENT);
    harness.check(m1.getMapSize(), 4);
    harness.check(m1.getPixelSize(), 2);
    harness.check(m1.hasAlpha());
    harness.check(!m1.isAlphaPremultiplied());
    
    // try bits < 1
    boolean pass = false;
    try
    {
      /* IndexColorModel m = */ new IndexColorModel(0, 4, R4, G4, B4, A4);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;   
    }
    harness.check(pass);
    
    // try bits > 16
    pass = false;
    try
    {
      /* IndexColorModel m = */ new IndexColorModel(17, 4, R4, G4, B4, A4);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;   
    }
    harness.check(pass);
    
    // try size bigger than arrays
    pass = false;
    try
    {
      /* IndexColorModel m = */ new IndexColorModel(2, 99, R4, G4, B4, A4);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;  
    }
    harness.check(pass);
    
    // try null red array
    pass = false;
    try 
    {
      /* IndexColorModel m = */ new IndexColorModel(2, 4, null, G4, B4, A4);      
    }
    catch (NullPointerException e) 
    {
      pass = true;   
    }
    harness.check(pass);

    // try null green array
    pass = false;
    try 
    {
      /* IndexColorModel m = */ new IndexColorModel(2, 4, R4, null, B4, A4);      
    }
    catch (NullPointerException e) 
    {
      pass = true;   
    }
    harness.check(pass);

    // try null blue array
    pass = false;
    try 
    {
      /* IndexColorModel m = */ new IndexColorModel(2, 4, R4, G4, null, A4);      
    }
    catch (NullPointerException e) 
    {
      pass = true;   
    }
    harness.check(pass);

    // try null alpha array
    IndexColorModel m = new IndexColorModel(2, 4, R4, G4, B4, null);
    harness.check(m.getTransparency(), Transparency.OPAQUE);
  }

  private void testConstructor3(TestHarness harness) 
  {  
    harness.checkPoint("IndexColorModel(int, int, byte[], byte[], byte[], int)");
    IndexColorModel m1 = new IndexColorModel(2, 4, R4, G4, B4, 2);
    harness.check(m1.getTransparency(), Transparency.BITMASK);
    harness.check(m1.getMapSize(), 4);
    harness.check(m1.getPixelSize(), 2);
    harness.check(m1.getTransparentPixel(), 2);
    harness.check(m1.hasAlpha());
    harness.check(!m1.isAlphaPremultiplied());
    
    // try bits < 1
    boolean pass = false;
    try
    {
      /* IndexColorModel m = */ new IndexColorModel(0, 4, R4, G4, B4, 2);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;   
    }
    harness.check(pass);
    
    // try bits > 16
    pass = false;
    try
    {
      /* IndexColorModel m = */ new IndexColorModel(17, 4, R4, G4, B4, 2);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;   
    }
    harness.check(pass);
    
    // try size bigger than arrays
    pass = false;
    try
    {
      /* IndexColorModel m = */ new IndexColorModel(2, 99, R4, G4, B4, 2);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;  
    }
    harness.check(pass);
    
    // try null red array
    pass = false;
    try 
    {
      /* IndexColorModel m = */ new IndexColorModel(2, 4, null, G4, B4, 2);      
    }
    catch (NullPointerException e) 
    {
      pass = true;   
    }
    harness.check(pass);

    // try null green array
    pass = false;
    try 
    {
      /* IndexColorModel m = */ new IndexColorModel(2, 4, R4, null, B4, 2);      
    }
    catch (NullPointerException e) 
    {
      pass = true;   
    }
    harness.check(pass);

    // try null blue array
    pass = false;
    try 
    {
      /* IndexColorModel m = */ new IndexColorModel(2, 4, R4, G4, null, 2);      
    }
    catch (NullPointerException e) 
    {
      pass = true;   
    }
    harness.check(pass);

    // try negative trans
    IndexColorModel m = new IndexColorModel(2, 4, R4, G4, B4, -1);
    harness.check(m.getTransparentPixel(), -1);
    harness.check(m.getTransparency(), Transparency.OPAQUE);
    
    m = new IndexColorModel(2, 4, R4, G4, B4, -99);
    harness.check(m.getTransparentPixel(), -1);
    harness.check(m.getTransparency(), Transparency.OPAQUE);
    
    // try trans too large
    m = new IndexColorModel(2, 4, R4, G4, B4, 4);
    harness.check(m.getTransparentPixel(), -1);
  }

  private void testConstructor4(TestHarness harness) 
  {  
    harness.checkPoint("IndexColorModel(int, int, byte[], int, boolean)");
    IndexColorModel m1 = new IndexColorModel(2, 4, CMAP, 0, false);
    harness.check(!m1.isAlphaPremultiplied());

    harness.check(m1.getRed(0), 1);
    harness.check(m1.getRed(1), 2);
    harness.check(m1.getRed(2), 3);
    harness.check(m1.getRed(3), 4);
    harness.check(m1.getGreen(0), 5);
    harness.check(m1.getGreen(1), 6);
    harness.check(m1.getGreen(2), 7);
    harness.check(m1.getGreen(3), 8);
    harness.check(m1.getBlue(0), 9);
    harness.check(m1.getBlue(1), 10);
    harness.check(m1.getBlue(2), 11);
    harness.check(m1.getBlue(3), 12);
    harness.check(m1.getAlpha(0), 255);
    harness.check(m1.getAlpha(1), 255);
    harness.check(m1.getAlpha(2), 255);
    harness.check(m1.getAlpha(3), 255);
    harness.check(!m1.hasAlpha());
    
    IndexColorModel m2 = new IndexColorModel(2, 4, CMAP_WITH_ALPHA, 0, true);
    harness.check(m2.getRed(0), 1);
    harness.check(m2.getRed(1), 2);
    harness.check(m2.getRed(2), 3);
    harness.check(m2.getRed(3), 4);
    harness.check(m2.getGreen(0), 5);
    harness.check(m2.getGreen(1), 6);
    harness.check(m2.getGreen(2), 7);
    harness.check(m2.getGreen(3), 8);
    harness.check(m2.getBlue(0), 9);
    harness.check(m2.getBlue(1), 10);
    harness.check(m2.getBlue(2), 11);
    harness.check(m2.getBlue(3), 12);
    harness.check(m2.getAlpha(0), 13);
    harness.check(m2.getAlpha(1), 14);
    harness.check(m2.getAlpha(2), 15);
    harness.check(m2.getAlpha(3), 16);
    harness.check(m2.hasAlpha());
    
    // try bits < 1
    boolean pass = false;
    try
    {
      /* IndexColorModel m = */ new IndexColorModel(0, 4, CMAP, 0, false);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;   
    }
    harness.check(pass);
    
    // try bits > 16
    pass = false;
    try
    {
      /* IndexColorModel m = */ new IndexColorModel(17, 4, CMAP, 0, false);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;   
    }
    harness.check(pass);
    
    // try size bigger than arrays
    pass = false;
    try
    {
      /* IndexColorModel m = */ new IndexColorModel(2, 99, CMAP, 0, false);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;  
    }
    harness.check(pass);
    
    // try null cmap array
    pass = false;
    try 
    {
      /* IndexColorModel m = */ new IndexColorModel(2, 4, null, 0, false);      
    }
    catch (NullPointerException e) 
    {
      pass = true;   
    }
    harness.check(pass);
  }

  private void testConstructor5(TestHarness harness) 
  {  
    harness.checkPoint("IndexColorModel(int, int, byte[], int, boolean, int)");
    IndexColorModel m1 = new IndexColorModel(2, 4, CMAP, 0, false, 1);
    harness.check(m1.getTransparentPixel(), 1);
    harness.check(!m1.isAlphaPremultiplied());

    harness.check(m1.getRed(0), 1);
    harness.check(m1.getRed(1), 2);
    harness.check(m1.getRed(2), 3);
    harness.check(m1.getRed(3), 4);
    harness.check(m1.getGreen(0), 5);
    harness.check(m1.getGreen(1), 6);
    harness.check(m1.getGreen(2), 7);
    harness.check(m1.getGreen(3), 8);
    harness.check(m1.getBlue(0), 9);
    harness.check(m1.getBlue(1), 10);
    harness.check(m1.getBlue(2), 11);
    harness.check(m1.getBlue(3), 12);
    harness.check(m1.getAlpha(0), 255);
    harness.check(m1.getAlpha(1), 0);
    harness.check(m1.getAlpha(2), 255);
    harness.check(m1.getAlpha(3), 255);
    
    IndexColorModel m2 = new IndexColorModel(2, 4, CMAP_WITH_ALPHA, 0, true, 2);
    harness.check(m2.getRed(0), 1);
    harness.check(m2.getRed(1), 2);
    harness.check(m2.getRed(2), 3);
    harness.check(m2.getRed(3), 4);
    harness.check(m2.getGreen(0), 5);
    harness.check(m2.getGreen(1), 6);
    harness.check(m2.getGreen(2), 7);
    harness.check(m2.getGreen(3), 8);
    harness.check(m2.getBlue(0), 9);
    harness.check(m2.getBlue(1), 10);
    harness.check(m2.getBlue(2), 11);
    harness.check(m2.getBlue(3), 12);
    harness.check(m2.getAlpha(0), 13);
    harness.check(m2.getAlpha(1), 14);
    harness.check(m2.getAlpha(2), 0);
    harness.check(m2.getAlpha(3), 16);
    
    // try bits < 1
    boolean pass = false;
    try
    {
      /* IndexColorModel m = */ new IndexColorModel(0, 4, CMAP, 0, false, 3);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;   
    }
    harness.check(pass);
    
    // try bits > 16
    pass = false;
    try
    {
      /* IndexColorModel m = */ new IndexColorModel(17, 4, CMAP, 0, false, 3);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;   
    }
    harness.check(pass);
    
    // try size bigger than arrays
    pass = false;
    try
    {
      /* IndexColorModel m = */ new IndexColorModel(2, 99, CMAP, 0, false, 3);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;  
    }
    harness.check(pass);
    
    // try null cmap array
    pass = false;
    try 
    {
      /* IndexColorModel m = */ new IndexColorModel(2, 4, null, 0, false, 3);      
    }
    catch (NullPointerException e) 
    {
      pass = true;   
    }
    harness.check(pass);
  }

  private void testConstructor6(TestHarness harness) 
  {  
    harness.checkPoint("IndexColorModel(int, int, int[], int, boolean, int, int)");
    IndexColorModel m1 = new IndexColorModel(2, 4, CMAP_INT, 0, false, 1, DataBuffer.TYPE_BYTE);
    harness.check(m1.getTransferType(), DataBuffer.TYPE_BYTE);
    harness.check(m1.getTransparentPixel(), 1);
    harness.check(!m1.isAlphaPremultiplied());

    harness.check(m1.getRed(0), 1);
    harness.check(m1.getRed(1), 2);
    harness.check(m1.getRed(2), 3);
    harness.check(m1.getRed(3), 4);
    harness.check(m1.getGreen(0), 5);
    harness.check(m1.getGreen(1), 6);
    harness.check(m1.getGreen(2), 7);
    harness.check(m1.getGreen(3), 8);
    harness.check(m1.getBlue(0), 9);
    harness.check(m1.getBlue(1), 10);
    harness.check(m1.getBlue(2), 11);
    harness.check(m1.getBlue(3), 12);
    harness.check(m1.getAlpha(0), 255);
    harness.check(m1.getAlpha(1), 0);
    harness.check(m1.getAlpha(2), 255);
    harness.check(m1.getAlpha(3), 255);

    IndexColorModel m2 = new IndexColorModel(2, 4, CMAP_INT_WITH_ALPHA, 0, true, 1, DataBuffer.TYPE_BYTE);
    harness.check(m2.getTransferType(), DataBuffer.TYPE_BYTE);
    harness.check(m2.getRed(0), 1);
    harness.check(m2.getRed(1), 2);
    harness.check(m2.getRed(2), 3);
    harness.check(m2.getRed(3), 4);
    harness.check(m2.getGreen(0), 5);
    harness.check(m2.getGreen(1), 6);
    harness.check(m2.getGreen(2), 7);
    harness.check(m2.getGreen(3), 8);
    harness.check(m2.getBlue(0), 9);
    harness.check(m2.getBlue(1), 10);
    harness.check(m2.getBlue(2), 11);
    harness.check(m2.getBlue(3), 12);
    harness.check(m2.getAlpha(0), 13);
    harness.check(m2.getAlpha(1), 0);
    harness.check(m2.getAlpha(2), 15);
    harness.check(m2.getAlpha(3), 16);
  }

  private void testConstructor7(TestHarness harness) 
  {  
    harness.checkPoint("IndexColorModel(int, int, int[], int, int, BigInteger)");
    IndexColorModel m1 = new IndexColorModel(2, 4, CMAP_INT, 0, DataBuffer.TYPE_BYTE, new BigInteger("11"));
    harness.check(!m1.isAlphaPremultiplied());

    harness.check(m1.isValid(0));
    harness.check(m1.isValid(1));
    harness.check(!m1.isValid(2));
    harness.check(m1.isValid(3));
  }

}
