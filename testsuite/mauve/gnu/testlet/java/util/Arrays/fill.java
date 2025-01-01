//Tags: JDK1.2

//Copyright (C) 2004 David Gilbert <david.gilbert@object-refinery.com>

//This file is part of Mauve.

//Mauve is free software; you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation; either version 2, or (at your option)
//any later version.

//Mauve is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.

//You should have received a copy of the GNU General Public License
//along with Mauve; see the file COPYING.  If not, write to
//the Free Software Foundation, 59 Temple Place - Suite 330,
//Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.util.Arrays;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Arrays;

public class fill implements Testlet
{
  public void test (TestHarness harness)
  {
    testBoolean(harness);
    testByte(harness);
    testChar(harness);
    testDouble(harness);
    testFloat(harness);
    testInt(harness);
    testLong(harness);
    testObject(harness);
    testShort(harness);
  }

  private void testBoolean(TestHarness harness)
  {
    harness.checkPoint("Arrays.fill(boolean[], boolean");
    boolean[] b1 = new boolean[0];
    boolean[] b2 = new boolean[1];
    boolean[] b3 = new boolean[2];
    
    Arrays.fill(b1, true);
    harness.check(b1.length == 0);
    
    Arrays.fill(b2, true);
    harness.check(b2[0] == true);
    
    Arrays.fill(b3, true);
    harness.check(b3[0] == true);
    harness.check(b3[1] == true);
    
    boolean pass = false;
    try
    {
      Arrays.fill((boolean[]) null, true);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    harness.checkPoint("Arrays.fill(boolean[], int, int, boolean");
    
    Arrays.fill(b1, 0, 0, false);
    
    Arrays.fill(b2, 0, 1, false);
    harness.check(b2[0] == false);
    
    Arrays.fill(b3, 1, 2, false);
    harness.check(b3[0] == true);
    harness.check(b3[1] == false);
    
    // from index should be <= toIndex
    pass = false;
    try
    {
      Arrays.fill(b3, 2, 1, false);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // from index should be >= 0
    pass = false;
    try
    {
      Arrays.fill(b3, -1, 1, false);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // to index should be < array.length
    pass = false;
    try
    {
      Arrays.fill(b3, 0, 4, false);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
  }

  private void testByte(TestHarness harness)
  {
    harness.checkPoint("Arrays.fill(byte[], byte");
    byte[] b1 = new byte[0];
    byte[] b2 = new byte[1];
    byte[] b3 = new byte[2];
      
    Arrays.fill(b1, (byte) 1);
    harness.check(b1.length == 0);
      
    Arrays.fill(b2, (byte) 1);
    harness.check(b2[0] == (byte) 1);
      
    Arrays.fill(b3, (byte) 1);
    harness.check(b3[0] == (byte) 1);
    harness.check(b3[1] == (byte) 1);
      
    boolean pass = false;
    try
    {
      Arrays.fill((byte[]) null, (byte) 1);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
     
    harness.checkPoint("Arrays.fill(byte[], int, int, byte");
    
    Arrays.fill(b1, 0, 0, (byte) 2);
      
    Arrays.fill(b2, 0, 1, (byte) 2);
    harness.check(b2[0] == (byte) 2);
      
    Arrays.fill(b3, 1, 2, (byte) 2);
    harness.check(b3[0] == (byte) 1);
    harness.check(b3[1] == (byte) 2);
      
    // from index should be <= toIndex
    pass = false;
    try
    {
      Arrays.fill(b3, 2, 1, (byte) 0);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
      
    // from index should be >= 0
    pass = false;
    try
    {
      Arrays.fill(b3, -1, 1, (byte) 0);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
      
    // to index should be < array.length
    pass = false;
    try
    {
      Arrays.fill(b3, 0, 4, (byte) 0);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
  }

  private void testChar(TestHarness harness)
  {
    harness.checkPoint("Arrays.fill(char[], char");
    char[] b1 = new char[0];
    char[] b2 = new char[1];
    char[] b3 = new char[2];
      
    Arrays.fill(b1, 'A');
    harness.check(b1.length == 0);
      
    Arrays.fill(b2, 'A');
    harness.check(b2[0] == 'A');
      
    Arrays.fill(b3, 'A');
    harness.check(b3[0] == 'A');
    harness.check(b3[1] == 'A');
      
    boolean pass = false;
    try
    {
      Arrays.fill((char[]) null, 'A');
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
     
    harness.checkPoint("Arrays.fill(char[], int, int, char");
    
    Arrays.fill(b1, 0, 0, 'B');
      
    Arrays.fill(b2, 0, 1, 'B');
    harness.check(b2[0] == 'B');
      
    Arrays.fill(b3, 1, 2, 'B');
    harness.check(b3[0] == 'A');
    harness.check(b3[1] == 'B');
      
    // from index should be <= toIndex
    pass = false;
    try
    {
      Arrays.fill(b3, 2, 1, 'B');
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
      
    // from index should be >= 0
    pass = false;
    try
    {
      Arrays.fill(b3, -1, 1, 'B');
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
      
    // to index should be < array.length
    pass = false;
    try
    {
      Arrays.fill(b3, 0, 4, 'B');
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
  }

  private void testDouble(TestHarness harness)
  {
    harness.checkPoint("Arrays.fill(double[], double");
    double[] b1 = new double[0];
    double[] b2 = new double[1];
    double[] b3 = new double[2];
      
    Arrays.fill(b1, 1.0);
    harness.check(b1.length == 0);
      
    Arrays.fill(b2, 1.0);
    harness.check(b2[0] == 1.0);
      
    Arrays.fill(b3, 1.0);
    harness.check(b3[0] == 1.0);
    harness.check(b3[1] == 1.0);
      
    boolean pass = false;
    try
    {
      Arrays.fill((double[]) null, 1.0);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
     
    harness.checkPoint("Arrays.fill(double[], int, int, double");
    
    Arrays.fill(b1, 0, 0, 2.0);
      
    Arrays.fill(b2, 0, 1, 2.0);
    harness.check(b2[0] == 2.0);
      
    Arrays.fill(b3, 1, 2, 2.0);
    harness.check(b3[0] == 1.0);
    harness.check(b3[1] == 2.0);
      
    // from index should be <= toIndex
    pass = false;
    try
    {
      Arrays.fill(b3, 2, 1, 2.0);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
      
    // from index should be >= 0
    pass = false;
    try
    {
      Arrays.fill(b3, -1, 1, 2.0);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
      
    // to index should be < array.length
    pass = false;
    try
    {
      Arrays.fill(b3, 0, 4, 2.0);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
  }

  private void testFloat(TestHarness harness)
  {
    harness.checkPoint("Arrays.fill(float[], float");
    float[] b1 = new float[0];
    float[] b2 = new float[1];
    float[] b3 = new float[2];
      
    Arrays.fill(b1, 1.0f);
    harness.check(b1.length == 0);
      
    Arrays.fill(b2, 1.0f);
    harness.check(b2[0] == 1.0f);
      
    Arrays.fill(b3, 1.0f);
    harness.check(b3[0] == 1.0f);
    harness.check(b3[1] == 1.0f);
      
    boolean pass = false;
    try
    {
      Arrays.fill((float[]) null, 1.0f);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
     
    harness.checkPoint("Arrays.fill(float[], int, int, float");
    
    Arrays.fill(b1, 0, 0, 2.0f);
      
    Arrays.fill(b2, 0, 1, 2.0f);
    harness.check(b2[0] == 2.0f);
      
    Arrays.fill(b3, 1, 2, 2.0f);
    harness.check(b3[0] == 1.0);
    harness.check(b3[1] == 2.0);
      
    // from index should be <= toIndex
    pass = false;
    try
    {
      Arrays.fill(b3, 2, 1, 2.0f);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
      
    // from index should be >= 0
    pass = false;
    try
    {
      Arrays.fill(b3, -1, 1, 2.0f);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
      
    // to index should be < array.length
    pass = false;
    try
    {
      Arrays.fill(b3, 0, 4, 2.0f);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
  }

  private void testInt(TestHarness harness)
  {
    harness.checkPoint("Arrays.fill(int[], int");
    int[] b1 = new int[0];
    int[] b2 = new int[1];
    int[] b3 = new int[2];
      
    Arrays.fill(b1, 1);
    harness.check(b1.length == 0);
      
    Arrays.fill(b2, 1);
    harness.check(b2[0] == 1);
      
    Arrays.fill(b3, 1);
    harness.check(b3[0] == 1);
    harness.check(b3[1] == 1);
      
    boolean pass = false;
    try
    {
      Arrays.fill((int[]) null, 1);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
     
    harness.checkPoint("Arrays.fill(int[], int, int, int");
    
    Arrays.fill(b1, 0, 0, 2);
      
    Arrays.fill(b2, 0, 1, 2);
    harness.check(b2[0] == 2);
     
    Arrays.fill(b3, 1, 2, 2);
    harness.check(b3[0] == 1);
    harness.check(b3[1] == 2);
      
    // from index should be <= toIndex
    pass = false;
    try
    {
      Arrays.fill(b3, 2, 1, 2);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
      
    // from index should be >= 0
    pass = false;
    try
    {
      Arrays.fill(b3, -1, 1, 2);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
      
    // to index should be < array.length
    pass = false;
    try
    {
      Arrays.fill(b3, 0, 4, 2);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
  }

  private void testLong(TestHarness harness)
  {
    harness.checkPoint("Arrays.fill(long[], long");
    long[] b1 = new long[0];
    long[] b2 = new long[1];
    long[] b3 = new long[2];
      
    Arrays.fill(b1, 1);
    harness.check(b1.length == 0);
      
    Arrays.fill(b2, 1);
    harness.check(b2[0] == 1);
      
    Arrays.fill(b3, 1);
    harness.check(b3[0] == 1);
    harness.check(b3[1] == 1);
      
    boolean pass = false;
    try
    {
      Arrays.fill((long[]) null, 1);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
     
    harness.checkPoint("Arrays.fill(long[], int, int, long");
    
    Arrays.fill(b1, 0, 0, 2);
      
    Arrays.fill(b2, 0, 1, 2);
    harness.check(b2[0] == 2);
     
    Arrays.fill(b3, 1, 2, 2);
    harness.check(b3[0] == 1);
    harness.check(b3[1] == 2);
      
    // from index should be <= toIndex
    pass = false;
    try
    {
      Arrays.fill(b3, 2, 1, 2);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
      
    // from index should be >= 0
    pass = false;
    try
    {
      Arrays.fill(b3, -1, 1, 2);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
      
    // to index should be < array.length
    pass = false;
    try
    {
      Arrays.fill(b3, 0, 4, 2);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
  }

  private void testObject(TestHarness harness)
  {
    harness.checkPoint("Arrays.fill(Object[], Object");
    Object[] b1 = new Object[0];
    Object[] b2 = new Object[1];
    Object[] b3 = new Object[2];
      
    Arrays.fill(b1, "1");
    harness.check(b1.length == 0);
      
    Arrays.fill(b2, "1");
    harness.check(b2[0] == "1");
      
    Arrays.fill(b3, "1");
    harness.check(b3[0] == "1");
    harness.check(b3[1] == "1");
      
    boolean pass = false;
    try
    {
      Arrays.fill((Object[]) null, "1");
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
     
    harness.checkPoint("Arrays.fill(Object[], int, int, long");
    
    Arrays.fill(b1, 0, 0, "2");
      
    Arrays.fill(b2, 0, 1, "2");
    harness.check(b2[0] == "2");
     
    Arrays.fill(b3, 1, 2, "2");
    harness.check(b3[0] == "1");
    harness.check(b3[1] == "2");
      
    // from index should be <= toIndex
    pass = false;
    try
    {
      Arrays.fill(b3, 2, 1, "2");
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
      
    // from index should be >= 0
    pass = false;
    try
    {
      Arrays.fill(b3, -1, 1, "2");
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
      
    // to index should be < array.length
    pass = false;
    try
    {
      Arrays.fill(b3, 0, 4, "2");
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
  }

  private void testShort(TestHarness harness)
  {
    harness.checkPoint("Arrays.fill(short[], short");
    short[] b1 = new short[0];
    short[] b2 = new short[1];
    short[] b3 = new short[2];
      
    Arrays.fill(b1, (short) 1);
    harness.check(b1.length == 0);
      
    Arrays.fill(b2, (short) 1);
    harness.check(b2[0] == 1);
      
    Arrays.fill(b3, (short) 1);
    harness.check(b3[0] == 1);
    harness.check(b3[1] == 1);
      
    boolean pass = false;
    try
    {
      Arrays.fill((int[]) null, 1);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
     
    harness.checkPoint("Arrays.fill(short)[], int, int, short");
    
    Arrays.fill(b1, 0, 0, (short) 2);
      
    Arrays.fill(b2, 0, 1, (short) 2);
    harness.check(b2[0] == 2);
     
    Arrays.fill(b3, 1, 2, (short) 2);
    harness.check(b3[0] == 1);
    harness.check(b3[1] == 2);
      
    // from index should be <= toIndex
    pass = false;
    try
    {
      Arrays.fill(b3, 2, 1, (short) 2);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
      
    // from index should be >= 0
    pass = false;
    try
    {
      Arrays.fill(b3, -1, 1, (short) 2);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
      
    // to index should be < array.length
    pass = false;
    try
    {
      Arrays.fill(b3, 0, 4, (short) 2);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
}

}
