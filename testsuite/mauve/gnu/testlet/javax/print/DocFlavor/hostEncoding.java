//Tags: JDK1.5

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

package gnu.testlet.javax.print.DocFlavor;

import java.nio.charset.Charset;

import javax.print.DocFlavor;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests that the _HOST doc flavor types are initialized
 * with the host encoding.
 */
public class hostEncoding implements Testlet
{

  public void test(TestHarness h)
  {    
    h.check(DocFlavor.hostEncoding.equals(Charset.defaultCharset().name()));
    
    String value = null;    
    value = DocFlavor.URL.TEXT_HTML_HOST.getParameter("charset");
    h.check(value.equals(DocFlavor.hostEncoding.toLowerCase()));    
    value = DocFlavor.URL.TEXT_PLAIN_HOST.getParameter("charset");
    h.check(value.equals(DocFlavor.hostEncoding.toLowerCase()));
  }

}
