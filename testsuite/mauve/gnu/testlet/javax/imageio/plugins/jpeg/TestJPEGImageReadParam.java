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
import javax.imageio.plugins.jpeg.JPEGHuffmanTable;
import javax.imageio.plugins.jpeg.JPEGImageReadParam;
import javax.imageio.plugins.jpeg.JPEGQTable;

/**
 * Test JPEGImageReadParam.
 */
public class TestJPEGImageReadParam implements Testlet
{
  public void test(TestHarness harness)
  {
    JPEGImageReadParam param = new JPEGImageReadParam();

    // valid data for testing.

    JPEGQTable[] qTables = new JPEGQTable[]
      {
        JPEGQTable.K1Luminance,
        JPEGQTable.K1Div2Luminance,
        JPEGQTable.K2Chrominance,
        JPEGQTable.K2Div2Chrominance
      };

    JPEGHuffmanTable[] DCHuffmanTables = new JPEGHuffmanTable[]
      {
        JPEGHuffmanTable.StdDCLuminance,
        JPEGHuffmanTable.StdDCChrominance
      };

    JPEGHuffmanTable[] ACHuffmanTables = new JPEGHuffmanTable[]
      {
        JPEGHuffmanTable.StdACLuminance,
        JPEGHuffmanTable.StdACChrominance
      };

    // check that tables are not set after construction.
    harness.check(!param.areTablesSet());

    // check that returned tables are null.
    harness.check(param.getQTables() == null);
    harness.check(param.getDCHuffmanTables() == null);
    harness.check(param.getACHuffmanTables() == null);

    // check failure modes for table setting
    // null argument
    boolean settingFailed = false;
    try
      {
        param.setDecodeTables(qTables, null, ACHuffmanTables);
      }
    catch (IllegalArgumentException e)
      {
        settingFailed = true;
      }
    harness.check(settingFailed);

    // invalid length for an array argument
    settingFailed = false;
    try
      {
        param.setDecodeTables(new JPEGQTable[]
            {
              JPEGQTable.K1Luminance,
              JPEGQTable.K1Div2Luminance,
              JPEGQTable.K2Chrominance,
              JPEGQTable.K2Div2Chrominance,
              JPEGQTable.K1Luminance
            }, DCHuffmanTables, ACHuffmanTables);
      }
    catch (IllegalArgumentException e)
      {
        settingFailed = true;
      }
    harness.check(settingFailed);

    // differing lengths for Huffman table array arguments
    settingFailed = false;
    try
      {
        param.setDecodeTables(qTables, new JPEGHuffmanTable[]
            {
              JPEGHuffmanTable.StdDCLuminance
            }, ACHuffmanTables);
      }
    catch (IllegalArgumentException e)
      {
        settingFailed = true;
      }
    harness.check(settingFailed);

    // check valid setting
    settingFailed = false;
    try
      {
        param.setDecodeTables(qTables, DCHuffmanTables, ACHuffmanTables);
      }
    catch (IllegalArgumentException e)
      {
        settingFailed = true;
      }
    harness.check(!settingFailed);

    // check tables set
    harness.check(param.areTablesSet());

    // check getting tables
    JPEGQTable[] gotQTables = param.getQTables();
    harness.check(gotQTables != null);
    harness.check(Arrays.equals(qTables, gotQTables));

    JPEGHuffmanTable[] gotDCHuffmanTables = param.getDCHuffmanTables();
    harness.check(gotDCHuffmanTables != null);
    harness.check(Arrays.equals(DCHuffmanTables, gotDCHuffmanTables));

    JPEGHuffmanTable[] gotACHuffmanTables = param.getACHuffmanTables();
    harness.check(gotACHuffmanTables != null);
    harness.check(Arrays.equals(ACHuffmanTables, gotACHuffmanTables));

    // check clearing tables
    param.unsetDecodeTables();

    // check getting null tables
    gotQTables = param.getQTables();
    harness.check(gotQTables == null);

    gotDCHuffmanTables = param.getDCHuffmanTables();
    harness.check(gotDCHuffmanTables == null);

    gotACHuffmanTables = param.getACHuffmanTables();
    harness.check(gotACHuffmanTables == null);

    // check that areTablesSet returns false
    harness.check(!param.areTablesSet());
  }
}
