// Tags: not-a-test

// Copyright (C) 2004 Sascha Brawer <brawer@dandelis.ch>

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

package gnu.testlet.javax.imageio.spi.ImageReaderWriterSpi;

import java.util.Locale;
import javax.imageio.spi.ImageReaderWriterSpi;

class TestProvider
  extends ImageReaderWriterSpi
{
  public static final String VENDOR_NAME
    = "Free Software Foundation, Inc.";

  public static final String VERSION = "1.0";

  public static final String[] NAMES = new String[]
    { "Tagged Image File Format" };

  public static final String[] SUFFIXES = new String[]
    { "tiff", "tif" };

  public static final String[] MIME_TYPES = new String[]
    { "image/tiff" };

  public static final String PLUGIN_CLASS_NAME
    = "gnu.javax.imageio.plugin.tiff.Reader";

  public static final String NATIVE_STREAM_METADATA_FORMAT_NAME
    = "TIFF File Metadata";

  public static final String NATIVE_STREAM_METADATA_FORMAT_CLASS_NAME
    = "gnu.javax.imageio.plugin.tiff.StreamMetadata";

  public static final String[] EXTRA_STREAM_METADATA_FORMAT_NAMES
    = new String[] { "Stream Metadata" };

  public static final String[] EXTRA_STREAM_METADATA_FORMAT_CLASS_NAMES
    = new String[] { "gnu.javax.imageio.plugin.tiff.StreamMetadata" };

  public static final String NATIVE_IMAGE_METADATA_FORMAT_NAME
    = "TIFF Image Metadata";

  public static final String NATIVE_IMAGE_METADATA_FORMAT_CLASS_NAME
    = "gnu.javax.imageio.plugin.tiff.ImageMetadata";

  public static final String[] EXTRA_IMAGE_METADATA_FORMAT_NAMES
    = new String[] { "GeoTIFF" };

  public static final String[] EXTRA_IMAGE_METADATA_FORMAT_CLASS_NAMES
    = new String[] { "gnu.javax.imageio.plugin.tiff.GeoTIFFMetadata" };

  private TestProvider(boolean b)
  {
    super(VENDOR_NAME, VERSION, NAMES, SUFFIXES, MIME_TYPES,
          PLUGIN_CLASS_NAME,

          true, NATIVE_STREAM_METADATA_FORMAT_NAME,
          NATIVE_STREAM_METADATA_FORMAT_CLASS_NAME,
          EXTRA_STREAM_METADATA_FORMAT_NAMES,
          EXTRA_STREAM_METADATA_FORMAT_CLASS_NAMES,

          true, NATIVE_IMAGE_METADATA_FORMAT_NAME,
          NATIVE_IMAGE_METADATA_FORMAT_CLASS_NAME,
          EXTRA_IMAGE_METADATA_FORMAT_NAMES,
          EXTRA_IMAGE_METADATA_FORMAT_CLASS_NAMES);
  }

  public static TestProvider createProvider()
  {
    return new TestProvider(true);
  }

  public TestProvider()
  {
  }

  public String getDescription(Locale locale)
  {
    return "desc";
  }
}
