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
 * Test correct throwing of IllegalStateExceptions
 * for the connected connection.
 */
public class illegalStateException implements Testlet
{
  public void test(TestHarness h)
  {
    try
      {
        URL url = new URL("http://sources.redhat.com/mauve/testarea/index.html");
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();

        conn.setRequestMethod("GET");
        conn.connect();

        try
          {
            conn.getRequestProperties();
            h.check(false);
          }
        catch (IllegalStateException e)
          {
            h.check(true);
          }

        // For some reasons SUN does not whats defined
        // in the API and does NOT throw a IllegalStateException
        // for this method - Tested with JDK1.4/1.5
        // to be compatible with SUN we also don't do it
        try
          {
            conn.getRequestProperty("accept");
            h.check(true);
          }
        catch (IllegalStateException e)
          {
            h.check(false);
          }

        try
          {
            conn.setDoInput(true);
            h.check(false);
          }
        catch (IllegalStateException e)
          {
            h.check(true);
          }

        try
          {
            conn.setDoOutput(true);
            h.check(false);
          }
        catch (IllegalStateException e)
          {
            h.check(true);
          }

        try
          {
            conn.setAllowUserInteraction(true);
            h.check(false);
          }
        catch (IllegalStateException e)
          {
            h.check(true);
          }

        try
          {
            conn.setUseCaches(true);
            h.check(false);
          }
        catch (IllegalStateException e)
          {
            h.check(true);
          }

        try
          {
            conn.setIfModifiedSince(100000L);
            h.check(false);
          }
        catch (IllegalStateException e)
          {
            h.check(true);
          }

        try
          {
            conn.setRequestProperty("ssss", "dddd");
            h.check(false);
          }
        catch (IllegalStateException e)
          {
            h.check(true);
          }

        try
          {
            // if already connected the IllegalStateException
            // must take precedence over the NPE
            conn.setRequestProperty(null, "dddd");
            h.check(false);
          }
        catch (IllegalStateException e)
          {
            h.check(true);
          }
        catch (NullPointerException e)
          {
            h.check(false);
          }

        try
          {
            conn.addRequestProperty("ssss", "dddd");
            h.check(false);
          }
        catch (IllegalStateException e)
          {
            h.check(true);
          }

        try
          {
            // if already connected the IllegalStateException
            // must take precedence over the NPE
            conn.addRequestProperty(null, "dddd");
            h.check(false);
          }
        catch (IllegalStateException e)
          {
            h.check(true);
          }
        catch (NullPointerException e)
          {
            h.check(false);
          }

      }
    catch (Exception e)
      {
        h.debug(e);
        h.fail("Unexpected error: " + e.getMessage ());
      }
  }
}
