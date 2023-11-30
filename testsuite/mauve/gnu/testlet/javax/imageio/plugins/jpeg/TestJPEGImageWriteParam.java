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
import java.util.Locale;
import javax.imageio.ImageWriteParam;
import javax.imageio.plugins.jpeg.JPEGHuffmanTable;
import javax.imageio.plugins.jpeg.JPEGImageWriteParam;
import javax.imageio.plugins.jpeg.JPEGQTable;

/**
 * Test JPEGImageWriteParam.
 */
public class TestJPEGImageWriteParam implements Testlet
{
  public void test(TestHarness harness)
  {
    // use the english locale so that we know what string to expect
    // for descriptions
    JPEGImageWriteParam param = new JPEGImageWriteParam(Locale.ENGLISH);

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

    // check that tables are not set after construction
    harness.check(!param.areTablesSet());

    // check that optimize is false after construction
    harness.check(!param.getOptimizeHuffmanTables());

    // check that returned tables are null
    harness.check(param.getQTables() == null);
    harness.check(param.getDCHuffmanTables() == null);
    harness.check(param.getACHuffmanTables() == null);

    // check that tiling is not supported
    harness.check(!param.canWriteTiles());

    // check that progressive encoding is supported
    harness.check(param.canWriteProgressive());

    // check the default progressive mode
    harness.check(param.getProgressiveMode() == ImageWriteParam.MODE_DISABLED);

    // check that compression is supported
    harness.check(param.canWriteCompressed());

    // check that a single compression type is supported
    harness.check(param.getCompressionTypes().length == 1);

    // check that the single compression type is called "JPEG"
    harness.check(param.getCompressionTypes()[0].equals("JPEG"));

    // set explicit compression mode so that compression mode tests
    // will work
    param.setCompressionMode(ImageWriteParam.MODE_EXPLICIT);

    // check that default compression type is "JPEG"
    harness.check(param.getCompressionType().equals("JPEG"));

    // check that the default compression quality is 0.75f
    harness.check(param.getCompressionQuality() == 0.75f);

    // check that compression is not lossless
    harness.check(!param.isCompressionLossless());

    // check compression descriptions
    String[] descriptions = param.getCompressionQualityDescriptions();
    harness.check(descriptions.length == 3);
    String[] expectedDescriptions =
      {
        "Minimum useful",
        "Visually lossless",
        "Maximum useful"
      };
    harness.check(Arrays.equals(descriptions, expectedDescriptions));

    // check compression quality values
    float[] values = param.getCompressionQualityValues();
    // Sun's API docs say that the length of the quality values array
    // will always be one greater than the length of the descriptions
    // array but testing shows the the lengths to be equal for
    // JPEGImageWriteParam.  This implies that rather than specifying
    // quality value interval endpoints, the returned array specifies
    // actual quality values, each of which correspond directly to a
    // quality description.
    harness.check(values.length == descriptions.length);
    float[] expectedValues = { 0.05f, 0.75f, 0.95f };
    harness.check(Arrays.equals(values, expectedValues));

    // check setting optimize
    param.setOptimizeHuffmanTables(true);
    harness.check(param.getOptimizeHuffmanTables());

    // check clearing optimize
    param.setOptimizeHuffmanTables(false);
    harness.check(!param.getOptimizeHuffmanTables());

    // check setting compression quality
    param.setCompressionQuality(0.31f);
    harness.check(param.getCompressionQuality() == 0.31f);

    // check that clearing compression quality resets to 0.75f
    param.unsetCompression();
    harness.check(param.getCompressionQuality() == 0.75f);

    // check failure modes for table setting
    // null argument
    boolean settingFailed = false;
    try
      {
        param.setEncodeTables(qTables, null, ACHuffmanTables);
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
        param.setEncodeTables(new JPEGQTable[]
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
        param.setEncodeTables(qTables, new JPEGHuffmanTable[]
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
        param.setEncodeTables(qTables, DCHuffmanTables, ACHuffmanTables);
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
    param.unsetEncodeTables();

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
