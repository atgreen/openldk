// Tags: JDK1.4

// Copyright (C) 2006  Red Hat

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
// Boston, MA 02111-1307, USA.

package gnu.testlet.javax.imageio.plugins.jpeg;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.util.Arrays;
import javax.imageio.plugins.jpeg.JPEGQTable;

/**
 * Test JPEGQTable construction and static fields.
 */
public class TestJPEGQTable implements Testlet
{
  public void test(TestHarness h)
  {
    JPEGQTable t;
    boolean constructionFailed;
    int[] table;

    int[] K1LuminanceValues =
      {
        16, 11, 10, 16,  24,  40,  51,  61,
        12, 12, 14, 19,  26,  58,  60,  55,
        14, 13, 16, 24,  40,  57,  69,  56,
        14, 17, 22, 29,  51,  87,  80,  62,
        18, 22, 37, 56,  68, 109, 103,  77,
        24, 35, 55, 64,  81, 104, 113,  92,
        49, 64, 78, 87, 103, 121, 120, 101,
        72, 92, 95, 98, 112, 100, 103,  99
      };

    int[] K1Div2LuminanceValues =
      {
        8, 6, 5, 8, 12, 20, 26, 31,
        6, 6, 7, 10, 13, 29, 30, 28,
        7, 7, 8, 12, 20, 29, 35, 28,
        7, 9, 11, 15, 26, 44, 40, 31,
        9, 11, 19, 28, 34, 55, 52, 39,
        12, 18, 28, 32, 41, 52, 57, 46,
        25, 32, 39, 44, 52, 61, 60, 51,
        36, 46, 48, 49, 56, 50, 52, 50
      };

    int[] K2ChrominanceValues =
      {
        17, 18, 24, 47, 99, 99, 99, 99,
        18, 21, 26, 66, 99, 99, 99, 99,
        24, 26, 56, 99, 99, 99, 99, 99,
        47, 66, 99, 99, 99, 99, 99, 99,
        99, 99, 99, 99, 99, 99, 99, 99,
        99, 99, 99, 99, 99, 99, 99, 99,
        99, 99, 99, 99, 99, 99, 99, 99,
        99, 99, 99, 99, 99, 99, 99, 99
      };

    int[] K2Div2ChrominanceValues =
      {
        9, 9, 12, 24, 50, 50, 50, 50,
        9, 11, 13, 33, 50, 50, 50, 50,
        12, 13, 28, 50, 50, 50, 50, 50,
        24, 33, 50, 50, 50, 50, 50, 50,
        50, 50, 50, 50, 50, 50, 50, 50,
        50, 50, 50, 50, 50, 50, 50, 50,
        50, 50, 50, 50, 50, 50, 50, 50,
        50, 50, 50, 50, 50, 50, 50, 50
      };

    // Test that it is impossible to construct an invalid quantization
    // table.

    // table argument is null
    constructionFailed = false;
    try
      {
        t = new JPEGQTable(null);
    }
    catch (IllegalArgumentException e)
      {
        constructionFailed = true;
      }
    h.check(constructionFailed);

    // table has length less than 64
    constructionFailed = false;
    int[] smallTable = new int[K1LuminanceValues.length - 20];
    System.arraycopy(K1LuminanceValues, 0, smallTable, 0, K1LuminanceValues.length - 20);
    try
      {
        t = new JPEGQTable(smallTable);
    }
    catch (IllegalArgumentException e)
      {
        constructionFailed = true;
      }
    h.check(constructionFailed);

    // table has length greater than 64
    constructionFailed = false;
    int[] bigTable = new int[K1LuminanceValues.length + 20];
    System.arraycopy(K1LuminanceValues, 0, bigTable, 0, K1LuminanceValues.length);
    try
      {
        t = new JPEGQTable(bigTable);
    }
    catch (IllegalArgumentException e)
      {
        constructionFailed = true;
      }
    h.check(constructionFailed);

    // check K1Luminance
    table = JPEGQTable.K1Luminance.getTable();
    h.check(Arrays.equals(table, K1LuminanceValues));

    // check K2Chrominance
    table = JPEGQTable.K2Chrominance.getTable();
    h.check(Arrays.equals(table, K2ChrominanceValues));

    // check K1Div2Luminance
    table = JPEGQTable.K1Div2Luminance.getTable();
    h.check(Arrays.equals(table, K1Div2LuminanceValues));

    // check K2Div2Chrominance
    table = JPEGQTable.K2Div2Chrominance.getTable();
    h.check(Arrays.equals(table, K2Div2ChrominanceValues));

    // scaling/rounding
    // less-than-one: 0.4 * K2Chrominance
    int roundedTable[] = new int[]
      {
        7, 7, 10, 19, 40, 40, 40, 40,
        7, 8, 10, 26, 40, 40, 40, 40,
        10, 10, 22, 40, 40, 40, 40, 40,
        19, 26, 40, 40, 40, 40, 40, 40,
        40, 40, 40, 40, 40, 40, 40, 40,
        40, 40, 40, 40, 40, 40, 40, 40,
        40, 40, 40, 40, 40, 40, 40, 40,
        40, 40, 40, 40, 40, 40, 40, 40
      };

    int[] scaledK2ChrominanceValues =
      JPEGQTable.K2Chrominance.getScaledInstance(0.4f, true).getTable();
    h.check(Arrays.equals(roundedTable, scaledK2ChrominanceValues));

    // greater-than-one: 1.7 * K1Luminance
    roundedTable = new int[]
      {
        27, 19, 17, 27, 41, 68, 87, 104,
        20, 20, 24, 32, 44, 99, 102, 94,
        24, 22, 27, 41, 68, 97, 117, 95,
        24, 29, 37, 49, 87, 148, 136, 105,
        31, 37, 63, 95, 116, 185, 175, 131,
        41, 60, 94, 109, 138, 177, 192, 156,
        83, 109, 133, 148, 175, 206, 204, 172,
        122, 156, 162, 167, 190, 170, 175, 168
      };

    int[] scaledK1LuminanceValues =
      JPEGQTable.K1Luminance.getScaledInstance(1.7f, true).getTable();
    h.check(Arrays.equals(roundedTable, scaledK1LuminanceValues));

    // scaling/clamping
    // scale by -6.4
    int[] clampedTable = new int[]
      {
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1
      };

    scaledK1LuminanceValues =
      JPEGQTable.K1Luminance.getScaledInstance(-6.4f, true).getTable();
    h.check(Arrays.equals(clampedTable, scaledK1LuminanceValues));

    // scale by 0.0
    clampedTable = new int[]
      {
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1
      };

    scaledK1LuminanceValues =
      JPEGQTable.K1Luminance.getScaledInstance(-0.0f, true).getTable();
    h.check(Arrays.equals(clampedTable, scaledK1LuminanceValues));

    // scale by 0.0
    clampedTable = new int[]
      {
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1
      };

    // 255 clamping
    clampedTable = new int[]
      {
        48, 33, 30, 48, 72, 120, 153, 183,
        36, 36, 42, 57, 78, 174, 180, 165,
        42, 39, 48, 72, 120, 171, 207, 168,
        42, 51, 66, 87, 153, 255, 240, 186,
        54, 66, 111, 168, 204, 255, 255, 231,
        72, 105, 165, 192, 243, 255, 255, 255,
        147, 192, 234, 255, 255, 255, 255, 255,
        216, 255, 255, 255, 255, 255, 255, 255
      };

    scaledK1LuminanceValues =
      JPEGQTable.K1Luminance.getScaledInstance(3.0f, true).getTable();
    h.check(Arrays.equals(clampedTable, scaledK1LuminanceValues));

    // 32767 clamping
    clampedTable = new int[]
      {
        16000, 11000, 10000, 16000, 24000, 32767, 32767, 32767,
        12000, 12000, 14000, 19000, 26000, 32767, 32767, 32767,
        14000, 13000, 16000, 24000, 32767, 32767, 32767, 32767,
        14000, 17000, 22000, 29000, 32767, 32767, 32767, 32767,
        18000, 22000, 32767, 32767, 32767, 32767, 32767, 32767,
        24000, 32767, 32767, 32767, 32767, 32767, 32767, 32767,
        32767, 32767, 32767, 32767, 32767, 32767, 32767, 32767,
        32767, 32767, 32767, 32767, 32767, 32767, 32767, 32767
      };

    scaledK1LuminanceValues =
      JPEGQTable.K1Luminance.getScaledInstance(1000.0f, false).getTable();
    h.check(Arrays.equals(clampedTable, scaledK1LuminanceValues));
  }
}
