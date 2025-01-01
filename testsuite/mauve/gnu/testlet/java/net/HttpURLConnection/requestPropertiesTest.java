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

package gnu.testlet.java.net.HttpURLConnection;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLConnection;
import java.util.List;
import java.util.Map;

/**
 * Various test for request properties.
 */
public class requestPropertiesTest implements Testlet
{

  public void test(TestHarness harness)
  {
    test_DefaultProperties(harness);
    test_Properties(harness);
    test_LowerUpperCaseProperties(harness);
    
  }
  
  // tests that nothing is done in the deprecated methods 
  public void test_DefaultProperties(TestHarness h)
  {    
    h.checkPoint("Default properties");
    URLConnection.setDefaultRequestProperty("Key", "Value");
    h.check(URLConnection.getDefaultRequestProperty("Key"), null);
  }  
 
  // test the various request properties methods
  public void test_Properties(TestHarness h)
  {    
    try
      {        
        URL url = new URL("http://localhost:8080/");
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod("GET");
     
        h.checkPoint("Standard properties tests");
        
        // defaults to nothing - null returned
        h.check(conn.getRequestProperty("Key") == null);
        
        // set does actually add if nothing is there
        conn.setRequestProperty("Key", "value");        
        h.check(conn.getRequestProperty("Key").equals("value"));
        
        // replace value
        conn.setRequestProperty("Key", "value2");
        h.check(conn.getRequestProperty("Key").equals("value2"));
        
        // add some stuff
        conn.addRequestProperty("Anotherkey", "value");
        conn.addRequestProperty("Anotherkey", "value2");
        
        // the last is returned
        h.check(conn.getRequestProperty("Anotherkey").equals("value2"));
        
        
        h.checkPoint("Map properties tests");
        Map props = conn.getRequestProperties();
        
        // must be 2 items
        h.check(props.size(), 2);
        
        Object obj = props.get("Anotherkey");
        if (obj instanceof List)
          {
            h.check(true);
            List list = (List) obj;
            h.check(list.size(), 2);
            h.check(list.get(0), "value2");
          }
        else
          h.check(false);    
        
      }
    catch (IOException e)
    {
      h.debug("Unexpected IOException");
      h.debug(e);
    }
    catch (Exception e)
      {
        h.debug("Unexpected Exception");
        h.debug(e);
      }    
  } 
  
  // test the case sensitiveness for request properties
  public void test_LowerUpperCaseProperties(TestHarness h)
  {    
    try
      {        
        URL url = new URL("http://localhost:8080/");
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod("GET");
     
        h.checkPoint("LowerUpperCase request properties tests");
        
        conn.addRequestProperty("Key", "value");
        
        // getRequestProperty is case insensitiv
        h.check(conn.getRequestProperty("key").equals("value"));
        
        // replacement of value does not replace the key name
        // however replacement is case insensitiv
        conn.setRequestProperty("key", "value2");
        h.check(conn.getRequestProperty("key").equals("value2"));
           
        // add some stuff
        conn.addRequestProperty("Anotherkey", "value");
        conn.addRequestProperty("anotherkey", "value2");
        conn.addRequestProperty("anotherkey", "value3");
        
        // get is case insensitiv
        h.check(conn.getRequestProperty("Anotherkey").equals("value3"));
              
        Map props = conn.getRequestProperties();            
        h.check(props.size(), 3);
        
        List l = (List) props.get("anotherkey");
        h.check(l.size(), 2);
        h.check(l.get(0).equals("value3"));
        
        l = (List) props.get("Key");
        h.check(l.size(), 1);
        h.check(l.get(0).equals("value2"));
        
        // if more values exist only the last one is replaced
        conn.setRequestProperty("anotherkey", "XXXX");
        h.check(conn.getRequestProperty("Anotherkey").equals("XXXX"));
    
        // only the last one is replaced !
        props = conn.getRequestProperties();
        l = (List) props.get("anotherkey");
        h.check(l.get(1).equals("value2"));
        
      }
    catch (IOException e)
    {
      h.debug("Unexpected IOException");
      h.debug(e);
    }
    catch (Exception e)
      {
        h.debug("Unexpected Exception");
        h.debug(e);
      }    
  }
}
