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

import java.io.CharArrayReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;

import javax.print.DocFlavor;
import javax.print.SimpleDoc;

/**
 * Checks getReaderForText
 */
public class getReaderForText implements Testlet
{
  public void test(TestHarness harness)
  {      
    SimpleDoc doc = new SimpleDoc(
      new String("Text to print"), DocFlavor.STRING.TEXT_PLAIN, null);
    
    try
      {
        Reader reader1 = doc.getReaderForText();
        Reader reader2 = doc.getReaderForText();
        harness.check(reader1 == reader2);
        harness.check(reader1 instanceof StringReader);
      }
    catch (IOException e)
      {
        harness.check(false);
      }    
    
    SimpleDoc doc1 = new SimpleDoc(
      new char[]{'A','b'}, DocFlavor.CHAR_ARRAY.TEXT_PLAIN, null);
                                
    try
      {
        Reader reader2 = doc1.getReaderForText();
        Reader reader3 = doc1.getReaderForText();
        harness.check(reader2 == reader3);
        harness.check(reader2 instanceof CharArrayReader);
      }
    catch (IOException e)
      {
        harness.check(false);
      }
    
    SimpleDoc doc2 = new SimpleDoc(
      new CharArrayReader(new char[]{'A','b'}), DocFlavor.READER.TEXT_PLAIN, null);
                                                             
    try
      {
        Reader reader4 = doc2.getReaderForText();
        Reader reader5 = doc2.getReaderForText();
        harness.check(reader4 == reader5);
        harness.check(reader4 == doc2.getPrintData());
      }
    catch (IOException e)
      {
        harness.check(false);
      }   
    
    SimpleDoc doc3 = new SimpleDoc(
      new byte[]{'A','b'}, DocFlavor.BYTE_ARRAY.AUTOSENSE, null);
                                                                                          
    try
      {
        Reader reader6 = doc3.getReaderForText();
        harness.check(reader6 == null);
      }
    catch (IOException e)
      {
        harness.check(false);
      }   
  }

}
