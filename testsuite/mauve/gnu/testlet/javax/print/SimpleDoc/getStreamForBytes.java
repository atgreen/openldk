//Tags: JDK1.4

//Copyright (C) 2006 Free Software Foundation, Inc.
//Written by Wolfgang Baer (WBaer@gmx.de)

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
//the Free Software Foundation, 51 Franklin Street, Fifth Floor,
//Boston, MA, 02110-1301 USA.


package gnu.testlet.javax.print.SimpleDoc;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;

import javax.print.DocFlavor;
import javax.print.SimpleDoc;

/**
 * Checks getStreamForBytes
 */
public class getStreamForBytes implements Testlet
{
  public void test(TestHarness harness)
  {
    SimpleDoc doc = new SimpleDoc(new byte[]{'2','3'},
        DocFlavor.BYTE_ARRAY.GIF, null);

    try
      {
        InputStream stream1 = doc.getStreamForBytes();
        InputStream stream2 = doc.getStreamForBytes();
        harness.check(stream1 == stream2);
        harness.check(stream1 instanceof ByteArrayInputStream);
      }
    catch (IOException e)
      {
        harness.check(false);
      }

    SimpleDoc doc1 = new SimpleDoc(
        new ByteArrayInputStream(new byte[] { 'A', 'b' }),
        DocFlavor.INPUT_STREAM.GIF, null);

    try
      {
        InputStream stream3 = doc1.getStreamForBytes();
        InputStream stream4 = doc1.getStreamForBytes();
        harness.check(stream3 == stream4);
        harness.check(stream3 == doc1.getPrintData());
      }
    catch (IOException e)
      {
        harness.check(false);
      }
  }
}
