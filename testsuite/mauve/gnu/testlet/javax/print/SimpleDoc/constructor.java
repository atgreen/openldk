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

import java.io.CharArrayReader;

import javax.print.DocFlavor;
import javax.print.SimpleDoc;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Simple constructor tests.
 */
public class constructor implements Testlet
{
  public void test(TestHarness harness)
  {    
    try
      {
        // printdata matches flavor
        new SimpleDoc(new byte[100], DocFlavor.BYTE_ARRAY.GIF, null);
        harness.check(true);
      }
    catch (RuntimeException e)
      {
        e.printStackTrace();
        harness.check(false);
      }
    
    try
      {
        // check for sublcasses of reader
        new SimpleDoc(new CharArrayReader(new char[]{'A','b'}), 
          DocFlavor.READER.TEXT_PLAIN, null);
        harness.check(true);
      }
    catch (RuntimeException e)
      {
        e.printStackTrace();
        harness.check(false);
      }
    
    try
      {
        // printdata does not match flavor
        new SimpleDoc(new byte[100], DocFlavor.CHAR_ARRAY.TEXT_PLAIN, null);
        harness.check(false);
      }
    catch (IllegalArgumentException e)
      {
        harness.check(true);
      }

    try
      {
        new SimpleDoc(null, DocFlavor.CHAR_ARRAY.TEXT_PLAIN, null);
        harness.check(false);
      }
    catch (IllegalArgumentException e)
      {
        harness.check(true);
      }

    try
      {
        new SimpleDoc(new String("kk"), null, null);
        harness.check(false);
      }
    catch (IllegalArgumentException e)
      {
        harness.check(true);
      }
  }
}
