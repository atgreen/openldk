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

import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.ProtocolException;
import java.net.URL;

/**
 * Test calling behaviour of getOutputStream. Implicit
 * opening of connection, failing if doOutput is false ...
 */
public class getOutputStream implements Testlet
{   
  public void test(TestHarness h)
  { 
    try
      {
        URL url = new URL("http://sources.redhat.com/mauve/testarea/index.html");        
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        
        conn.setRequestMethod("POST");
        conn.setDoOutput(false);
        
        try
          {
            // doOutput must be true to succeed
            conn.getOutputStream();
            h.check(false);
          }
        catch (ProtocolException e1)
          {
            h.check(true);
          }
        
        conn.setDoOutput(true);
        // getOutputStream must implicit open the connection
        OutputStream stream = conn.getOutputStream();
        
        try
          {
            // and therefore throw this expection
            // no other way to test if we are connected
            conn.getRequestProperties();
            h.check(false);
          }
        catch (IllegalStateException e)
          {
            h.check(true);            
          }
        
        // subsequent calls to getOutputStream must be ignored
        // and the identical stream returned
        OutputStream stream2 = conn.getOutputStream();              
        h.check(stream == stream2);              
        
      }   
    catch (Exception e)
      {       
        h.debug(e);
        h.fail("Unexpected error: " + e.getMessage ());
      }
  }
}
