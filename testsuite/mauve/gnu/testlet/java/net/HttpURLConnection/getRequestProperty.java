//Tags: JDK1.1

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


package gnu.testlet.java.net.HttpURLConnection;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.net.HttpURLConnection;
import java.net.URL;

/**
 * Tests the getRequestProperty method
 */
public class getRequestProperty implements Testlet
{
  public void test(TestHarness h)
  {
    try
      {
        URL url = new URL("http://sources.redhat.com/mauve/testarea/index.html");
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();

        conn.setRequestMethod("GET");
               
        String str = conn.getRequestProperty(null);     
        h.check(str == null);
      }
    catch (Exception e)
      {
        h.debug(e);
        h.fail("Unexpected error: " + e.getMessage ());
      }
  }
}
