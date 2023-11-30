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

import java.io.FileNotFoundException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;

/**
 * Tests that a 404 condition.
 */
public class fileNotFound implements Testlet
{
   
  public void test(TestHarness h)
  { 
    try
      {
        URL url = new URL("http://www.redhat.com/mauve/testarea/edeltraut.html");        
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        
        conn.setRequestMethod("GET");
        
        try 
          {
            // connect does not throw a FNFE
            conn.connect();
            int code = conn.getResponseCode();
            h.check(code == 404);
          }
        catch (FileNotFoundException e)
          {       
            h.check(false);
          }
        
        try 
          {
            // FNFE is thrown by calling getInputStream
            conn.getInputStream();
            h.check(false);
          }
        catch (FileNotFoundException e)
          {
            h.check(true);
          }
        
        // the errorstream must be set (at least our
        // URL returns an error page
        InputStream error = conn.getErrorStream();
        h.check(error != null);
        
        conn.disconnect();        
      }  
    catch (Exception e)
    {       
      h.debug(e);
      h.fail("Unexpected error: " + e.getMessage ());
    }
  }

}
