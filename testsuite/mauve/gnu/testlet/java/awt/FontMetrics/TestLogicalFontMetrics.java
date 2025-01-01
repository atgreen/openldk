// Tags: GUI JDK1.0

// Copyright (C) 2006 Red Hat

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

package gnu.testlet.java.awt.FontMetrics;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.*;
import java.io.*;

/**
 * Test that the metrics returned for the six logical fonts are
 * similar to those returned by Sun's implementation.
 */
public class TestLogicalFontMetrics implements Testlet
{
  BufferedWriter ascentsWriter;
  BufferedWriter descentsWriter;
  BufferedWriter leadingWriter;

  final int NUM_SIZES = 40;
  final int NUM_STYLES = 4;

  // Set this to true to create ascents.txt, descents.txt and
  // leading.txt for the five logical fonts in the four possible
  // styles, in sizes 0 through NUM_SIZES - 1.
  static final boolean GENERATE_METRICS_DATA = false;

  int[] ascents;
  int[] descents;
  int[] leading;
  String[] fontNames;

  TestHarness harness;

  boolean paintedOnce = false;

  int index = 0;

  public int getTolerance (int size)
  {
    if (size < 19)
      return 2;
    else
      return 5;
  }

  public void test (TestHarness h)
  {
    Frame f = new Frame ();

    harness = h;

    fontNames = new String[] { "Dialog", "DialogInput",
                               "Monospaced", "Serif", "SansSerif" };

    // metrics data generated from previous runs against Sun
    ascents = new int[]
      {
        // Dialog
        // size 0
        0, 0, 0, 0, 
        // size 1
        1, 1, 1, 1, 
        // size 2
        2, 2, 2, 2, 
        // size 3
        3, 3, 3, 3, 
        // size 4
        4, 4, 4, 4, 
        // size 5
        5, 5, 5, 5, 
        // size 6
        6, 6, 6, 6, 
        // size 7
        7, 7, 7, 7, 
        // size 8
        8, 8, 8, 8, 
        // size 9
        9, 9, 9, 9, 
        // size 10
        10, 10, 10, 10, 
        // size 11
        11, 11, 11, 11, 
        // size 12
        12, 12, 12, 12, 
        // size 13
        13, 13, 13, 13, 
        // size 14
        14, 14, 14, 14, 
        // size 15
        15, 15, 15, 15, 
        // size 16
        16, 16, 16, 16, 
        // size 17
        17, 17, 17, 17, 
        // size 18
        18, 18, 18, 18, 
        // size 19
        19, 19, 19, 19, 
        // size 20
        19, 20, 19, 20, 
        // size 21
        20, 20, 20, 20, 
        // size 22
        21, 21, 21, 21, 
        // size 23
        22, 22, 22, 22, 
        // size 24
        23, 23, 23, 23, 
        // size 25
        24, 24, 24, 24, 
        // size 26
        25, 25, 25, 25, 
        // size 27
        26, 26, 26, 26, 
        // size 28
        27, 27, 27, 27, 
        // size 29
        28, 28, 28, 28, 
        // size 30
        29, 29, 29, 29, 
        // size 31
        30, 30, 30, 30, 
        // size 32
        31, 31, 31, 31, 
        // size 33
        32, 32, 32, 32, 
        // size 34
        33, 33, 33, 33, 
        // size 35
        34, 34, 34, 34, 
        // size 36
        35, 35, 35, 35, 
        // size 37
        36, 36, 36, 36, 
        // size 38
        37, 37, 37, 37, 
        // size 39
        38, 38, 38, 38, 
        // DialogInput
        // size 0
        0, 0, 0, 0, 
        // size 1
        1, 1, 1, 1, 
        // size 2
        2, 2, 2, 2, 
        // size 3
        3, 3, 3, 3, 
        // size 4
        4, 4, 4, 4, 
        // size 5
        5, 5, 5, 5, 
        // size 6
        6, 6, 6, 6, 
        // size 7
        7, 7, 7, 7, 
        // size 8
        8, 8, 8, 8, 
        // size 9
        9, 9, 9, 9, 
        // size 10
        10, 10, 10, 10, 
        // size 11
        11, 11, 11, 11, 
        // size 12
        12, 12, 12, 12, 
        // size 13
        13, 13, 13, 13, 
        // size 14
        14, 14, 14, 14, 
        // size 15
        15, 15, 15, 15, 
        // size 16
        16, 16, 16, 16, 
        // size 17
        17, 17, 17, 17, 
        // size 18
        18, 17, 18, 17, 
        // size 19
        19, 18, 19, 18, 
        // size 20
        19, 19, 19, 19, 
        // size 21
        20, 20, 20, 20, 
        // size 22
        21, 21, 21, 21, 
        // size 23
        22, 22, 22, 22, 
        // size 24
        23, 23, 23, 23, 
        // size 25
        24, 24, 24, 24, 
        // size 26
        25, 25, 25, 25, 
        // size 27
        26, 26, 26, 26, 
        // size 28
        27, 27, 27, 27, 
        // size 29
        28, 28, 28, 28, 
        // size 30
        29, 29, 29, 29, 
        // size 31
        30, 30, 30, 30, 
        // size 32
        31, 31, 31, 31, 
        // size 33
        32, 32, 32, 32, 
        // size 34
        33, 33, 33, 33, 
        // size 35
        34, 34, 34, 34, 
        // size 36
        35, 34, 35, 34, 
        // size 37
        36, 35, 36, 35, 
        // size 38
        37, 36, 37, 36, 
        // size 39
        38, 37, 38, 37, 
        // Monospaced
        // size 0
        0, 0, 0, 0, 
        // size 1
        1, 1, 1, 1, 
        // size 2
        2, 2, 2, 2, 
        // size 3
        3, 3, 3, 3, 
        // size 4
        4, 4, 4, 4, 
        // size 5
        5, 5, 5, 5, 
        // size 6
        6, 6, 6, 6, 
        // size 7
        7, 7, 7, 7, 
        // size 8
        8, 8, 8, 8, 
        // size 9
        9, 9, 9, 9, 
        // size 10
        10, 10, 10, 10, 
        // size 11
        11, 11, 11, 11, 
        // size 12
        12, 12, 12, 12, 
        // size 13
        13, 13, 13, 13, 
        // size 14
        14, 14, 14, 14, 
        // size 15
        15, 15, 15, 15, 
        // size 16
        16, 16, 16, 16, 
        // size 17
        17, 17, 17, 17, 
        // size 18
        18, 17, 18, 17, 
        // size 19
        19, 18, 19, 18, 
        // size 20
        19, 19, 19, 19, 
        // size 21
        20, 20, 20, 20, 
        // size 22
        21, 21, 21, 21, 
        // size 23
        22, 22, 22, 22, 
        // size 24
        23, 23, 23, 23, 
        // size 25
        24, 24, 24, 24, 
        // size 26
        25, 25, 25, 25, 
        // size 27
        26, 26, 26, 26, 
        // size 28
        27, 27, 27, 27, 
        // size 29
        28, 28, 28, 28, 
        // size 30
        29, 29, 29, 29, 
        // size 31
        30, 30, 30, 30, 
        // size 32
        31, 31, 31, 31, 
        // size 33
        32, 32, 32, 32, 
        // size 34
        33, 33, 33, 33, 
        // size 35
        34, 34, 34, 34, 
        // size 36
        35, 34, 35, 34, 
        // size 37
        36, 35, 36, 35, 
        // size 38
        37, 36, 37, 36, 
        // size 39
        38, 37, 38, 37, 
        // Serif
        // size 0
        0, 0, 0, 0, 
        // size 1
        1, 1, 1, 1, 
        // size 2
        2, 2, 2, 2, 
        // size 3
        3, 3, 3, 3, 
        // size 4
        4, 4, 4, 4, 
        // size 5
        5, 5, 5, 5, 
        // size 6
        6, 6, 6, 6, 
        // size 7
        7, 7, 7, 7, 
        // size 8
        8, 8, 8, 8, 
        // size 9
        9, 9, 9, 9, 
        // size 10
        10, 10, 10, 10, 
        // size 11
        11, 11, 11, 11, 
        // size 12
        12, 12, 12, 12, 
        // size 13
        13, 13, 13, 13, 
        // size 14
        14, 14, 14, 14, 
        // size 15
        15, 15, 15, 15, 
        // size 16
        16, 16, 16, 16, 
        // size 17
        17, 17, 17, 17, 
        // size 18
        18, 18, 18, 18, 
        // size 19
        19, 19, 19, 19, 
        // size 20
        19, 19, 19, 19, 
        // size 21
        20, 20, 20, 20, 
        // size 22
        21, 21, 21, 21, 
        // size 23
        22, 22, 22, 22, 
        // size 24
        23, 23, 23, 23, 
        // size 25
        24, 24, 24, 24, 
        // size 26
        25, 25, 25, 25, 
        // size 27
        26, 26, 26, 26, 
        // size 28
        27, 27, 27, 27, 
        // size 29
        28, 28, 28, 28, 
        // size 30
        29, 29, 29, 29, 
        // size 31
        30, 30, 30, 30, 
        // size 32
        31, 31, 31, 31, 
        // size 33
        32, 32, 32, 32, 
        // size 34
        33, 33, 33, 33, 
        // size 35
        34, 34, 34, 34, 
        // size 36
        35, 35, 35, 35, 
        // size 37
        36, 36, 36, 36, 
        // size 38
        37, 37, 37, 37, 
        // size 39
        38, 38, 38, 38, 
        // SansSerif
        // size 0
        0, 0, 0, 0, 
        // size 1
        1, 1, 1, 1, 
        // size 2
        2, 2, 2, 2, 
        // size 3
        3, 3, 3, 3, 
        // size 4
        4, 4, 4, 4, 
        // size 5
        5, 5, 5, 5, 
        // size 6
        6, 6, 6, 6, 
        // size 7
        7, 7, 7, 7, 
        // size 8
        8, 8, 8, 8, 
        // size 9
        9, 9, 9, 9, 
        // size 10
        10, 10, 10, 10, 
        // size 11
        11, 11, 11, 11, 
        // size 12
        12, 12, 12, 12, 
        // size 13
        13, 13, 13, 13, 
        // size 14
        14, 14, 14, 14, 
        // size 15
        15, 15, 15, 15, 
        // size 16
        16, 16, 16, 16, 
        // size 17
        17, 17, 17, 17, 
        // size 18
        18, 18, 18, 18, 
        // size 19
        19, 19, 19, 19, 
        // size 20
        19, 20, 19, 20, 
        // size 21
        20, 20, 20, 20, 
        // size 22
        21, 21, 21, 21, 
        // size 23
        22, 22, 22, 22, 
        // size 24
        23, 23, 23, 23, 
        // size 25
        24, 24, 24, 24, 
        // size 26
        25, 25, 25, 25, 
        // size 27
        26, 26, 26, 26, 
        // size 28
        27, 27, 27, 27, 
        // size 29
        28, 28, 28, 28, 
        // size 30
        29, 29, 29, 29, 
        // size 31
        30, 30, 30, 30, 
        // size 32
        31, 31, 31, 31, 
        // size 33
        32, 32, 32, 32, 
        // size 34
        33, 33, 33, 33, 
        // size 35
        34, 34, 34, 34, 
        // size 36
        35, 35, 35, 35, 
        // size 37
        36, 36, 36, 36, 
        // size 38
        37, 37, 37, 37, 
        // size 39
        38, 38, 38, 38, 
        // Dialog
        // size 0
        0, 0, 0, 0, 
        // size 1
        1, 1, 1, 1, 
        // size 2
        2, 2, 2, 2, 
        // size 3
        3, 3, 3, 3, 
        // size 4
        4, 4, 4, 4, 
        // size 5
        5, 5, 5, 5, 
        // size 6
        6, 6, 6, 6, 
        // size 7
        7, 7, 7, 7, 
        // size 8
        8, 8, 8, 8, 
        // size 9
        9, 9, 9, 9, 
        // size 10
        10, 10, 10, 10, 
        // size 11
        11, 11, 11, 11, 
        // size 12
        12, 12, 12, 12, 
        // size 13
        13, 13, 13, 13, 
        // size 14
        14, 14, 14, 14, 
        // size 15
        15, 15, 15, 15, 
        // size 16
        16, 16, 16, 16, 
        // size 17
        17, 17, 17, 17, 
        // size 18
        18, 18, 18, 18, 
        // size 19
        19, 19, 19, 19, 
        // size 20
        19, 20, 19, 20, 
        // size 21
        20, 20, 20, 20, 
        // size 22
        21, 21, 21, 21, 
        // size 23
        22, 22, 22, 22, 
        // size 24
        23, 23, 23, 23, 
        // size 25
        24, 24, 24, 24, 
        // size 26
        25, 25, 25, 25, 
        // size 27
        26, 26, 26, 26, 
        // size 28
        27, 27, 27, 27, 
        // size 29
        28, 28, 28, 28, 
        // size 30
        29, 29, 29, 29, 
        // size 31
        30, 30, 30, 30, 
        // size 32
        31, 31, 31, 31, 
        // size 33
        32, 32, 32, 32, 
        // size 34
        33, 33, 33, 33, 
        // size 35
        34, 34, 34, 34, 
        // size 36
        35, 35, 35, 35, 
        // size 37
        36, 36, 36, 36, 
        // size 38
        37, 37, 37, 37, 
        // size 39
        38, 38, 38, 38, 
        // DialogInput
        // size 0
        0, 0, 0, 0, 
        // size 1
        1, 1, 1, 1, 
        // size 2
        2, 2, 2, 2, 
        // size 3
        3, 3, 3, 3, 
        // size 4
        4, 4, 4, 4, 
        // size 5
        5, 5, 5, 5, 
        // size 6
        6, 6, 6, 6, 
        // size 7
        7, 7, 7, 7, 
        // size 8
        8, 8, 8, 8, 
        // size 9
        9, 9, 9, 9, 
        // size 10
        10, 10, 10, 10, 
        // size 11
        11, 11, 11, 11, 
        // size 12
        12, 12, 12, 12, 
        // size 13
        13, 13, 13, 13, 
        // size 14
        14, 14, 14, 14, 
        // size 15
        15, 15, 15, 15, 
        // size 16
        16, 16, 16, 16, 
        // size 17
        17, 17, 17, 17, 
        // size 18
        18, 17, 18, 17, 
        // size 19
        19, 18, 19, 18, 
        // size 20
        19, 19, 19, 19, 
        // size 21
        20, 20, 20, 20, 
        // size 22
        21, 21, 21, 21, 
        // size 23
        22, 22, 22, 22, 
        // size 24
        23, 23, 23, 23, 
        // size 25
        24, 24, 24, 24, 
        // size 26
        25, 25, 25, 25, 
        // size 27
        26, 26, 26, 26, 
        // size 28
        27, 27, 27, 27, 
        // size 29
        28, 28, 28, 28, 
        // size 30
        29, 29, 29, 29, 
        // size 31
        30, 30, 30, 30, 
        // size 32
        31, 31, 31, 31, 
        // size 33
        32, 32, 32, 32, 
        // size 34
        33, 33, 33, 33, 
        // size 35
        34, 34, 34, 34, 
        // size 36
        35, 34, 35, 34, 
        // size 37
        36, 35, 36, 35, 
        // size 38
        37, 36, 37, 36, 
        // size 39
        38, 37, 38, 37, 
        // Monospaced
        // size 0
        0, 0, 0, 0, 
        // size 1
        1, 1, 1, 1, 
        // size 2
        2, 2, 2, 2, 
        // size 3
        3, 3, 3, 3, 
        // size 4
        4, 4, 4, 4, 
        // size 5
        5, 5, 5, 5, 
        // size 6
        6, 6, 6, 6, 
        // size 7
        7, 7, 7, 7, 
        // size 8
        8, 8, 8, 8, 
        // size 9
        9, 9, 9, 9, 
        // size 10
        10, 10, 10, 10, 
        // size 11
        11, 11, 11, 11, 
        // size 12
        12, 12, 12, 12, 
        // size 13
        13, 13, 13, 13, 
        // size 14
        14, 14, 14, 14, 
        // size 15
        15, 15, 15, 15, 
        // size 16
        16, 16, 16, 16, 
        // size 17
        17, 17, 17, 17, 
        // size 18
        18, 17, 18, 17, 
        // size 19
        19, 18, 19, 18, 
        // size 20
        19, 19, 19, 19, 
        // size 21
        20, 20, 20, 20, 
        // size 22
        21, 21, 21, 21, 
        // size 23
        22, 22, 22, 22, 
        // size 24
        23, 23, 23, 23, 
        // size 25
        24, 24, 24, 24, 
        // size 26
        25, 25, 25, 25, 
        // size 27
        26, 26, 26, 26, 
        // size 28
        27, 27, 27, 27, 
        // size 29
        28, 28, 28, 28, 
        // size 30
        29, 29, 29, 29, 
        // size 31
        30, 30, 30, 30, 
        // size 32
        31, 31, 31, 31, 
        // size 33
        32, 32, 32, 32, 
        // size 34
        33, 33, 33, 33, 
        // size 35
        34, 34, 34, 34, 
        // size 36
        35, 34, 35, 34, 
        // size 37
        36, 35, 36, 35, 
        // size 38
        37, 36, 37, 36, 
        // size 39
        38, 37, 38, 37, 
        // Serif
        // size 0
        0, 0, 0, 0, 
        // size 1
        1, 1, 1, 1, 
        // size 2
        2, 2, 2, 2, 
        // size 3
        3, 3, 3, 3, 
        // size 4
        4, 4, 4, 4, 
        // size 5
        5, 5, 5, 5, 
        // size 6
        6, 6, 6, 6, 
        // size 7
        7, 7, 7, 7, 
        // size 8
        8, 8, 8, 8, 
        // size 9
        9, 9, 9, 9, 
        // size 10
        10, 10, 10, 10, 
        // size 11
        11, 11, 11, 11, 
        // size 12
        12, 12, 12, 12, 
        // size 13
        13, 13, 13, 13, 
        // size 14
        14, 14, 14, 14, 
        // size 15
        15, 15, 15, 15, 
        // size 16
        16, 16, 16, 16, 
        // size 17
        17, 17, 17, 17, 
        // size 18
        18, 18, 18, 18, 
        // size 19
        19, 19, 19, 19, 
        // size 20
        19, 19, 19, 19, 
        // size 21
        20, 20, 20, 20, 
        // size 22
        21, 21, 21, 21, 
        // size 23
        22, 22, 22, 22, 
        // size 24
        23, 23, 23, 23, 
        // size 25
        24, 24, 24, 24, 
        // size 26
        25, 25, 25, 25, 
        // size 27
        26, 26, 26, 26, 
        // size 28
        27, 27, 27, 27, 
        // size 29
        28, 28, 28, 28, 
        // size 30
        29, 29, 29, 29, 
        // size 31
        30, 30, 30, 30, 
        // size 32
        31, 31, 31, 31, 
        // size 33
        32, 32, 32, 32, 
        // size 34
        33, 33, 33, 33, 
        // size 35
        34, 34, 34, 34, 
        // size 36
        35, 35, 35, 35, 
        // size 37
        36, 36, 36, 36, 
        // size 38
        37, 37, 37, 37, 
        // size 39
        38, 38, 38, 38, 
        // SansSerif
        // size 0
        0, 0, 0, 0, 
        // size 1
        1, 1, 1, 1, 
        // size 2
        2, 2, 2, 2, 
        // size 3
        3, 3, 3, 3, 
        // size 4
        4, 4, 4, 4, 
        // size 5
        5, 5, 5, 5, 
        // size 6
        6, 6, 6, 6, 
        // size 7
        7, 7, 7, 7, 
        // size 8
        8, 8, 8, 8, 
        // size 9
        9, 9, 9, 9, 
        // size 10
        10, 10, 10, 10, 
        // size 11
        11, 11, 11, 11, 
        // size 12
        12, 12, 12, 12, 
        // size 13
        13, 13, 13, 13, 
        // size 14
        14, 14, 14, 14, 
        // size 15
        15, 15, 15, 15, 
        // size 16
        16, 16, 16, 16, 
        // size 17
        17, 17, 17, 17, 
        // size 18
        18, 18, 18, 18, 
        // size 19
        19, 19, 19, 19, 
        // size 20
        19, 20, 19, 20, 
        // size 21
        20, 20, 20, 20, 
        // size 22
        21, 21, 21, 21, 
        // size 23
        22, 22, 22, 22, 
        // size 24
        23, 23, 23, 23, 
        // size 25
        24, 24, 24, 24, 
        // size 26
        25, 25, 25, 25, 
        // size 27
        26, 26, 26, 26, 
        // size 28
        27, 27, 27, 27, 
        // size 29
        28, 28, 28, 28, 
        // size 30
        29, 29, 29, 29, 
        // size 31
        30, 30, 30, 30, 
        // size 32
        31, 31, 31, 31, 
        // size 33
        32, 32, 32, 32, 
        // size 34
        33, 33, 33, 33, 
        // size 35
        34, 34, 34, 34, 
        // size 36
        35, 35, 35, 35, 
        // size 37
        36, 36, 36, 36, 
        // size 38
        37, 37, 37, 37, 
        // size 39
        38, 38, 38, 38, 
      };

    descents = new int[]
      {
        // Dialog
        // size 0
        0, 0, 0, 0, 
        // size 1
        1, 1, 1, 1, 
        // size 2
        1, 1, 1, 1, 
        // size 3
        1, 1, 1, 1, 
        // size 4
        1, 1, 1, 1, 
        // size 5
        2, 2, 2, 2, 
        // size 6
        2, 2, 2, 2, 
        // size 7
        2, 2, 2, 2, 
        // size 8
        2, 2, 2, 2, 
        // size 9
        2, 2, 2, 2, 
        // size 10
        3, 3, 3, 3, 
        // size 11
        3, 3, 3, 3, 
        // size 12
        3, 3, 3, 3, 
        // size 13
        3, 3, 3, 3, 
        // size 14
        3, 3, 3, 3, 
        // size 15
        4, 4, 4, 4, 
        // size 16
        4, 4, 4, 4, 
        // size 17
        4, 4, 4, 4, 
        // size 18
        4, 4, 4, 4, 
        // size 19
        5, 4, 5, 4, 
        // size 20
        5, 5, 5, 5, 
        // size 21
        5, 5, 5, 5, 
        // size 22
        5, 5, 5, 5, 
        // size 23
        5, 5, 5, 5, 
        // size 24
        6, 6, 6, 6, 
        // size 25
        6, 6, 6, 6, 
        // size 26
        6, 6, 6, 6, 
        // size 27
        6, 6, 6, 6, 
        // size 28
        6, 6, 6, 6, 
        // size 29
        7, 7, 7, 7, 
        // size 30
        7, 7, 7, 7, 
        // size 31
        7, 7, 7, 7, 
        // size 32
        7, 7, 7, 7, 
        // size 33
        7, 7, 7, 7, 
        // size 34
        8, 8, 8, 8, 
        // size 35
        8, 8, 8, 8, 
        // size 36
        8, 8, 8, 8, 
        // size 37
        8, 8, 8, 8, 
        // size 38
        9, 8, 9, 8, 
        // size 39
        9, 9, 9, 9, 
        // DialogInput
        // size 0
        0, 0, 0, 0, 
        // size 1
        1, 1, 1, 1, 
        // size 2
        1, 1, 1, 1, 
        // size 3
        1, 1, 1, 1, 
        // size 4
        1, 1, 1, 1, 
        // size 5
        2, 2, 2, 2, 
        // size 6
        2, 2, 2, 2, 
        // size 7
        2, 2, 2, 2, 
        // size 8
        2, 2, 2, 2, 
        // size 9
        2, 2, 2, 2, 
        // size 10
        3, 3, 3, 3, 
        // size 11
        3, 3, 3, 3, 
        // size 12
        3, 3, 3, 3, 
        // size 13
        3, 3, 3, 3, 
        // size 14
        3, 3, 3, 3, 
        // size 15
        4, 4, 4, 4, 
        // size 16
        4, 4, 4, 4, 
        // size 17
        4, 4, 4, 4, 
        // size 18
        4, 4, 4, 4, 
        // size 19
        5, 4, 5, 4, 
        // size 20
        5, 5, 5, 5, 
        // size 21
        5, 5, 5, 5, 
        // size 22
        5, 5, 5, 5, 
        // size 23
        5, 5, 5, 5, 
        // size 24
        6, 6, 6, 6, 
        // size 25
        6, 6, 6, 6, 
        // size 26
        6, 6, 6, 6, 
        // size 27
        6, 6, 6, 6, 
        // size 28
        6, 6, 6, 6, 
        // size 29
        7, 7, 7, 7, 
        // size 30
        7, 7, 7, 7, 
        // size 31
        7, 7, 7, 7, 
        // size 32
        7, 7, 7, 7, 
        // size 33
        7, 7, 7, 7, 
        // size 34
        8, 8, 8, 8, 
        // size 35
        8, 8, 8, 8, 
        // size 36
        8, 8, 8, 8, 
        // size 37
        8, 8, 8, 8, 
        // size 38
        9, 8, 9, 8, 
        // size 39
        9, 9, 9, 9, 
        // Monospaced
        // size 0
        0, 0, 0, 0, 
        // size 1
        1, 1, 1, 1, 
        // size 2
        1, 1, 1, 1, 
        // size 3
        1, 1, 1, 1, 
        // size 4
        1, 1, 1, 1, 
        // size 5
        2, 2, 2, 2, 
        // size 6
        2, 2, 2, 2, 
        // size 7
        2, 2, 2, 2, 
        // size 8
        2, 2, 2, 2, 
        // size 9
        2, 2, 2, 2, 
        // size 10
        3, 3, 3, 3, 
        // size 11
        3, 3, 3, 3, 
        // size 12
        3, 3, 3, 3, 
        // size 13
        3, 3, 3, 3, 
        // size 14
        3, 3, 3, 3, 
        // size 15
        4, 4, 4, 4, 
        // size 16
        4, 4, 4, 4, 
        // size 17
        4, 4, 4, 4, 
        // size 18
        4, 4, 4, 4, 
        // size 19
        5, 4, 5, 4, 
        // size 20
        5, 5, 5, 5, 
        // size 21
        5, 5, 5, 5, 
        // size 22
        5, 5, 5, 5, 
        // size 23
        5, 5, 5, 5, 
        // size 24
        6, 6, 6, 6, 
        // size 25
        6, 6, 6, 6, 
        // size 26
        6, 6, 6, 6, 
        // size 27
        6, 6, 6, 6, 
        // size 28
        6, 6, 6, 6, 
        // size 29
        7, 7, 7, 7, 
        // size 30
        7, 7, 7, 7, 
        // size 31
        7, 7, 7, 7, 
        // size 32
        7, 7, 7, 7, 
        // size 33
        7, 7, 7, 7, 
        // size 34
        8, 8, 8, 8, 
        // size 35
        8, 8, 8, 8, 
        // size 36
        8, 8, 8, 8, 
        // size 37
        8, 8, 8, 8, 
        // size 38
        9, 8, 9, 8, 
        // size 39
        9, 9, 9, 9, 
        // Serif
        // size 0
        0, 0, 0, 0, 
        // size 1
        1, 1, 1, 1, 
        // size 2
        1, 1, 1, 1, 
        // size 3
        1, 1, 1, 1, 
        // size 4
        1, 1, 1, 1, 
        // size 5
        2, 2, 2, 2, 
        // size 6
        2, 2, 2, 2, 
        // size 7
        2, 2, 2, 2, 
        // size 8
        2, 2, 2, 2, 
        // size 9
        2, 2, 2, 2, 
        // size 10
        3, 3, 3, 3, 
        // size 11
        3, 3, 3, 3, 
        // size 12
        3, 3, 3, 3, 
        // size 13
        3, 3, 3, 3, 
        // size 14
        3, 3, 3, 3, 
        // size 15
        4, 4, 4, 4, 
        // size 16
        4, 4, 4, 4, 
        // size 17
        4, 4, 4, 4, 
        // size 18
        4, 4, 4, 4, 
        // size 19
        5, 5, 5, 5, 
        // size 20
        5, 5, 5, 5, 
        // size 21
        5, 5, 5, 5, 
        // size 22
        5, 5, 5, 5, 
        // size 23
        5, 5, 5, 5, 
        // size 24
        6, 6, 6, 6, 
        // size 25
        6, 6, 6, 6, 
        // size 26
        6, 6, 6, 6, 
        // size 27
        6, 6, 6, 6, 
        // size 28
        6, 6, 6, 6, 
        // size 29
        7, 7, 7, 7, 
        // size 30
        7, 7, 7, 7, 
        // size 31
        7, 7, 7, 7, 
        // size 32
        7, 7, 7, 7, 
        // size 33
        7, 7, 7, 7, 
        // size 34
        8, 8, 8, 8, 
        // size 35
        8, 8, 8, 8, 
        // size 36
        8, 8, 8, 8, 
        // size 37
        8, 8, 8, 8, 
        // size 38
        9, 9, 9, 9, 
        // size 39
        9, 9, 9, 9, 
        // SansSerif
        // size 0
        0, 0, 0, 0, 
        // size 1
        1, 1, 1, 1, 
        // size 2
        1, 1, 1, 1, 
        // size 3
        1, 1, 1, 1, 
        // size 4
        1, 1, 1, 1, 
        // size 5
        2, 2, 2, 2, 
        // size 6
        2, 2, 2, 2, 
        // size 7
        2, 2, 2, 2, 
        // size 8
        2, 2, 2, 2, 
        // size 9
        2, 2, 2, 2, 
        // size 10
        3, 3, 3, 3, 
        // size 11
        3, 3, 3, 3, 
        // size 12
        3, 3, 3, 3, 
        // size 13
        3, 3, 3, 3, 
        // size 14
        3, 3, 3, 3, 
        // size 15
        4, 4, 4, 4, 
        // size 16
        4, 4, 4, 4, 
        // size 17
        4, 4, 4, 4, 
        // size 18
        4, 4, 4, 4, 
        // size 19
        5, 4, 5, 4, 
        // size 20
        5, 5, 5, 5, 
        // size 21
        5, 5, 5, 5, 
        // size 22
        5, 5, 5, 5, 
        // size 23
        5, 5, 5, 5, 
        // size 24
        6, 6, 6, 6, 
        // size 25
        6, 6, 6, 6, 
        // size 26
        6, 6, 6, 6, 
        // size 27
        6, 6, 6, 6, 
        // size 28
        6, 6, 6, 6, 
        // size 29
        7, 7, 7, 7, 
        // size 30
        7, 7, 7, 7, 
        // size 31
        7, 7, 7, 7, 
        // size 32
        7, 7, 7, 7, 
        // size 33
        7, 7, 7, 7, 
        // size 34
        8, 8, 8, 8, 
        // size 35
        8, 8, 8, 8, 
        // size 36
        8, 8, 8, 8, 
        // size 37
        8, 8, 8, 8, 
        // size 38
        9, 8, 9, 8, 
        // size 39
        9, 9, 9, 9, 
        // Dialog
        // size 0
        0, 0, 0, 0, 
        // size 1
        1, 1, 1, 1, 
        // size 2
        1, 1, 1, 1, 
        // size 3
        1, 1, 1, 1, 
        // size 4
        1, 1, 1, 1, 
        // size 5
        2, 2, 2, 2, 
        // size 6
        2, 2, 2, 2, 
        // size 7
        2, 2, 2, 2, 
        // size 8
        2, 2, 2, 2, 
        // size 9
        2, 2, 2, 2, 
        // size 10
        3, 3, 3, 3, 
        // size 11
        3, 3, 3, 3, 
        // size 12
        3, 3, 3, 3, 
        // size 13
        3, 3, 3, 3, 
        // size 14
        3, 3, 3, 3, 
        // size 15
        4, 4, 4, 4, 
        // size 16
        4, 4, 4, 4, 
        // size 17
        4, 4, 4, 4, 
        // size 18
        4, 4, 4, 4, 
        // size 19
        5, 4, 5, 4, 
        // size 20
        5, 5, 5, 5, 
        // size 21
        5, 5, 5, 5, 
        // size 22
        5, 5, 5, 5, 
        // size 23
        5, 5, 5, 5, 
        // size 24
        6, 6, 6, 6, 
        // size 25
        6, 6, 6, 6, 
        // size 26
        6, 6, 6, 6, 
        // size 27
        6, 6, 6, 6, 
        // size 28
        6, 6, 6, 6, 
        // size 29
        7, 7, 7, 7, 
        // size 30
        7, 7, 7, 7, 
        // size 31
        7, 7, 7, 7, 
        // size 32
        7, 7, 7, 7, 
        // size 33
        7, 7, 7, 7, 
        // size 34
        8, 8, 8, 8, 
        // size 35
        8, 8, 8, 8, 
        // size 36
        8, 8, 8, 8, 
        // size 37
        8, 8, 8, 8, 
        // size 38
        9, 8, 9, 8, 
        // size 39
        9, 9, 9, 9, 
        // DialogInput
        // size 0
        0, 0, 0, 0, 
        // size 1
        1, 1, 1, 1, 
        // size 2
        1, 1, 1, 1, 
        // size 3
        1, 1, 1, 1, 
        // size 4
        1, 1, 1, 1, 
        // size 5
        2, 2, 2, 2, 
        // size 6
        2, 2, 2, 2, 
        // size 7
        2, 2, 2, 2, 
        // size 8
        2, 2, 2, 2, 
        // size 9
        2, 2, 2, 2, 
        // size 10
        3, 3, 3, 3, 
        // size 11
        3, 3, 3, 3, 
        // size 12
        3, 3, 3, 3, 
        // size 13
        3, 3, 3, 3, 
        // size 14
        3, 3, 3, 3, 
        // size 15
        4, 4, 4, 4, 
        // size 16
        4, 4, 4, 4, 
        // size 17
        4, 4, 4, 4, 
        // size 18
        4, 4, 4, 4, 
        // size 19
        5, 4, 5, 4, 
        // size 20
        5, 5, 5, 5, 
        // size 21
        5, 5, 5, 5, 
        // size 22
        5, 5, 5, 5, 
        // size 23
        5, 5, 5, 5, 
        // size 24
        6, 6, 6, 6, 
        // size 25
        6, 6, 6, 6, 
        // size 26
        6, 6, 6, 6, 
        // size 27
        6, 6, 6, 6, 
        // size 28
        6, 6, 6, 6, 
        // size 29
        7, 7, 7, 7, 
        // size 30
        7, 7, 7, 7, 
        // size 31
        7, 7, 7, 7, 
        // size 32
        7, 7, 7, 7, 
        // size 33
        7, 7, 7, 7, 
        // size 34
        8, 8, 8, 8, 
        // size 35
        8, 8, 8, 8, 
        // size 36
        8, 8, 8, 8, 
        // size 37
        8, 8, 8, 8, 
        // size 38
        9, 8, 9, 8, 
        // size 39
        9, 9, 9, 9, 
        // Monospaced
        // size 0
        0, 0, 0, 0, 
        // size 1
        1, 1, 1, 1, 
        // size 2
        1, 1, 1, 1, 
        // size 3
        1, 1, 1, 1, 
        // size 4
        1, 1, 1, 1, 
        // size 5
        2, 2, 2, 2, 
        // size 6
        2, 2, 2, 2, 
        // size 7
        2, 2, 2, 2, 
        // size 8
        2, 2, 2, 2, 
        // size 9
        2, 2, 2, 2, 
        // size 10
        3, 3, 3, 3, 
        // size 11
        3, 3, 3, 3, 
        // size 12
        3, 3, 3, 3, 
        // size 13
        3, 3, 3, 3, 
        // size 14
        3, 3, 3, 3, 
        // size 15
        4, 4, 4, 4, 
        // size 16
        4, 4, 4, 4, 
        // size 17
        4, 4, 4, 4, 
        // size 18
        4, 4, 4, 4, 
        // size 19
        5, 4, 5, 4, 
        // size 20
        5, 5, 5, 5, 
        // size 21
        5, 5, 5, 5, 
        // size 22
        5, 5, 5, 5, 
        // size 23
        5, 5, 5, 5, 
        // size 24
        6, 6, 6, 6, 
        // size 25
        6, 6, 6, 6, 
        // size 26
        6, 6, 6, 6, 
        // size 27
        6, 6, 6, 6, 
        // size 28
        6, 6, 6, 6, 
        // size 29
        7, 7, 7, 7, 
        // size 30
        7, 7, 7, 7, 
        // size 31
        7, 7, 7, 7, 
        // size 32
        7, 7, 7, 7, 
        // size 33
        7, 7, 7, 7, 
        // size 34
        8, 8, 8, 8, 
        // size 35
        8, 8, 8, 8, 
        // size 36
        8, 8, 8, 8, 
        // size 37
        8, 8, 8, 8, 
        // size 38
        9, 8, 9, 8, 
        // size 39
        9, 9, 9, 9, 
        // Serif
        // size 0
        0, 0, 0, 0, 
        // size 1
        1, 1, 1, 1, 
        // size 2
        1, 1, 1, 1, 
        // size 3
        1, 1, 1, 1, 
        // size 4
        1, 1, 1, 1, 
        // size 5
        2, 2, 2, 2, 
        // size 6
        2, 2, 2, 2, 
        // size 7
        2, 2, 2, 2, 
        // size 8
        2, 2, 2, 2, 
        // size 9
        2, 2, 2, 2, 
        // size 10
        3, 3, 3, 3, 
        // size 11
        3, 3, 3, 3, 
        // size 12
        3, 3, 3, 3, 
        // size 13
        3, 3, 3, 3, 
        // size 14
        3, 3, 3, 3, 
        // size 15
        4, 4, 4, 4, 
        // size 16
        4, 4, 4, 4, 
        // size 17
        4, 4, 4, 4, 
        // size 18
        4, 4, 4, 4, 
        // size 19
        5, 5, 5, 5, 
        // size 20
        5, 5, 5, 5, 
        // size 21
        5, 5, 5, 5, 
        // size 22
        5, 5, 5, 5, 
        // size 23
        5, 5, 5, 5, 
        // size 24
        6, 6, 6, 6, 
        // size 25
        6, 6, 6, 6, 
        // size 26
        6, 6, 6, 6, 
        // size 27
        6, 6, 6, 6, 
        // size 28
        6, 6, 6, 6, 
        // size 29
        7, 7, 7, 7, 
        // size 30
        7, 7, 7, 7, 
        // size 31
        7, 7, 7, 7, 
        // size 32
        7, 7, 7, 7, 
        // size 33
        7, 7, 7, 7, 
        // size 34
        8, 8, 8, 8, 
        // size 35
        8, 8, 8, 8, 
        // size 36
        8, 8, 8, 8, 
        // size 37
        8, 8, 8, 8, 
        // size 38
        9, 9, 9, 9, 
        // size 39
        9, 9, 9, 9, 
        // SansSerif
        // size 0
        0, 0, 0, 0, 
        // size 1
        1, 1, 1, 1, 
        // size 2
        1, 1, 1, 1, 
        // size 3
        1, 1, 1, 1, 
        // size 4
        1, 1, 1, 1, 
        // size 5
        2, 2, 2, 2, 
        // size 6
        2, 2, 2, 2, 
        // size 7
        2, 2, 2, 2, 
        // size 8
        2, 2, 2, 2, 
        // size 9
        2, 2, 2, 2, 
        // size 10
        3, 3, 3, 3, 
        // size 11
        3, 3, 3, 3, 
        // size 12
        3, 3, 3, 3, 
        // size 13
        3, 3, 3, 3, 
        // size 14
        3, 3, 3, 3, 
        // size 15
        4, 4, 4, 4, 
        // size 16
        4, 4, 4, 4, 
        // size 17
        4, 4, 4, 4, 
        // size 18
        4, 4, 4, 4, 
        // size 19
        5, 4, 5, 4, 
        // size 20
        5, 5, 5, 5, 
        // size 21
        5, 5, 5, 5, 
        // size 22
        5, 5, 5, 5, 
        // size 23
        5, 5, 5, 5, 
        // size 24
        6, 6, 6, 6, 
        // size 25
        6, 6, 6, 6, 
        // size 26
        6, 6, 6, 6, 
        // size 27
        6, 6, 6, 6, 
        // size 28
        6, 6, 6, 6, 
        // size 29
        7, 7, 7, 7, 
        // size 30
        7, 7, 7, 7, 
        // size 31
        7, 7, 7, 7, 
        // size 32
        7, 7, 7, 7, 
        // size 33
        7, 7, 7, 7, 
        // size 34
        8, 8, 8, 8, 
        // size 35
        8, 8, 8, 8, 
        // size 36
        8, 8, 8, 8, 
        // size 37
        8, 8, 8, 8, 
        // size 38
        9, 8, 9, 8, 
        // size 39
        9, 9, 9, 9,
      };

    if (GENERATE_METRICS_DATA)
      {
        try
          {
            ascentsWriter = new BufferedWriter (new FileWriter (new File ("ascents.txt")));
            descentsWriter = new BufferedWriter (new FileWriter (new File ("descents.txt")));
            leadingWriter = new BufferedWriter (new FileWriter (new File ("leading.txt")));
          }
        catch (IOException e)
          {
            System.err.println ("error creating output files.");
          }
      }

    Panel p = new Panel()
      {
    	public void paint (Graphics g)
    	{
          if (!paintedOnce)
            {
              for (int i = 0; i < fontNames.length; i++)
                {
                  outputMetrics ("// " + fontNames[i] + "\n",
                                 "// " + fontNames[i] + "\n",
                                 "// " + fontNames[i] + "\n");

                  for (int j = 0; j < NUM_SIZES; j++)
                    {
                      outputMetrics ("// size " + j + "\n",
                                     "// size " + j + "\n",
                                     "// size " + j + "\n");

                      index = i * NUM_SIZES * NUM_STYLES + j * NUM_STYLES;

                      // PLAIN
                      g.setFont(new Font (fontNames[i], Font.PLAIN, j));
                      checkMetrics (g, fontNames[i], "PLAIN", j, index);

                      index++;

                      // BOLD
                      g.setFont(new Font (fontNames[i], Font.BOLD, j));
                      checkMetrics (g, fontNames[i], "BOLD", j, index);

                      index++;

                      // ITALIC
                      g.setFont(new Font (fontNames[i], Font.ITALIC, j));
                      checkMetrics (g, fontNames[i], "ITALIC", j, index);

                      index++;

                      // BOLD + ITALIC
                      g.setFont(new Font (fontNames[i], Font.BOLD | Font.ITALIC, j));
                      checkMetrics (g, fontNames[i], "BOLD + ITALIC", j, index);

                      outputMetrics ("\n", "\n", "\n");
                    }
                }
              paintedOnce = true;
            }
    	}
      };
    p.setBackground(Color.blue);

    f.setSize(300, 100);
    f.add(p);
    
    f.show();

    while (!paintedOnce)
      {
	try
	  {
	    java.lang.Thread.sleep (2000);
	  }
	catch (java.lang.InterruptedException e)
	  {
          }
      }

    if (GENERATE_METRICS_DATA)
      {
        try
          {
            if (ascentsWriter != null)
              ascentsWriter.close();
            if (descentsWriter != null)
              descentsWriter.close();
            if (leadingWriter != null)
              leadingWriter.close();
          }
        catch (IOException e)
          {
            System.err.println ("error closing output files.");
          }
      }
  }

  public void outputMetrics (String ascentString, String descentString, String leadingString)
  {
    if (GENERATE_METRICS_DATA)
      {
        try
          {
            ascentsWriter.write(ascentString);
            descentsWriter.write(descentString);
            leadingWriter.write(leadingString);
          }
        catch (IOException e)
          {
            System.err.println ("error writing to output files.");
          }
      }
  }

  public void checkMetrics (Graphics g, String name, String style, int size, int index)
  {
    FontMetrics f = g.getFontMetrics();

    outputMetrics (f.getAscent() + ", ",
                   f.getDescent() + ", ",
                   f.getLeading() + ", ");

    int ascent = f.getAscent();
    if (ascent < ascents[index] - getTolerance(size) || ascent > ascents[index] + getTolerance(size))
      harness.fail ("ascent: " + name + " " + style + " " + size
                    + " expected: " + ascents[index] + " got: " + ascent);

    int descent = f.getDescent();
    if (descent < descents[index] - getTolerance(size) || descent > descents[index] + getTolerance(size))
      harness.fail ("descent: " + name + " " + style + " " + size
                    + " expected: " + descents[index] + " got: " + descent);

    int leading = f.getLeading();
    // Leading is always 0.
    if (leading != 0)
      harness.fail ("leading: " + name + " " + style + " " + size
                    + " expected: 0 got: " + leading);
  }
}
