// Tags: JDK1.4

// Copyright (C) 2004 Michael Koch <konqueror@gmx.de>
// Copyright (C) 2006 Wolfgang Baer <WBaer@gmx.de>

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

package gnu.testlet.javax.print.DocFlavor;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import javax.print.DocFlavor;

/**
 * Tests the mime type parsing behaviour of DocFlavor.
 * 
 * @author Michael Koch (konqueror@gmx.de)
 * @author Wolfgang Baer (WBaer@gmx.de) 
 */
public class parseMimeType implements Testlet
{
  public void test(TestHarness h)
  {
    // Check simple mimetype
    DocFlavor simple = new DocFlavor("text/plain; charset=us-ascii", 
                                     "java.io.InputStream");

    h.checkPoint("Simple mimetype");
    h.check(simple.getMediaType().equals("text"));
    h.check(simple.getMediaSubtype().equals("plain"));
    h.check(simple.getParameter("charset").equals("us-ascii"));
    h.check(simple.getRepresentationClassName().equals("java.io.InputStream"));
    // Check if mimetype can be correctly built together again.
    h.check(simple.getMimeType().equals("text/plain; charset=\"us-ascii\""));
    h.check(simple.toString().equals("text/plain; charset=\"us-ascii\"; " +
                                     "class=\"java.io.InputStream\""));

    // Check for mimetype with quoted parameter value
    DocFlavor quoted = new DocFlavor("text/plain; charset=\"us-ascii\"", 
                                     "java.io.InputStream");

    h.checkPoint("Mimetype with quoted param values");
    h.check(quoted.getParameter("charset").equals("us-ascii"));
    // Check if mimetype can be correctly built together again.
    h.check(quoted.getMimeType().equals("text/plain; charset=\"us-ascii\""));
    h.check(simple.toString().equals("text/plain; charset=\"us-ascii\"; " +
                                     "class=\"java.io.InputStream\""));

    // Check for mimetype with multiple parameters
    DocFlavor multipleParam = new DocFlavor("text/plain; " +
      "charset=\"us-ascii\"; param=paramValue", "java.io.InputStream");

    h.checkPoint("Mimetype with multiple parameters");
    h.check(multipleParam.getParameter("charset").equals("us-ascii"));
    h.check(multipleParam.getParameter("param").equals("paramValue"));
    // Check if mimetype can be correctly built together again.
    h.check(multipleParam.getMimeType().equals("text/plain; " +
      "charset=\"us-ascii\"; param=\"paramValue\""));
    h.check(multipleParam.toString().equals("text/plain; charset=\"us-ascii\";" +
      " param=\"paramValue\"; class=\"java.io.InputStream\""));

    // Check natural order for mimetype with multiple parameters
    DocFlavor paramOrder = new DocFlavor("text/plain; " +
      "charset=\"us-ascii\"; another=paramValue; charset3=something", 
      "java.io.InputStream");

    h.checkPoint("Multiple parameters output order");
    // parameters are returned in natural key order 
    // therefore another -> charset -> charset3
    h.check(paramOrder.getMimeType().equals("text/plain; " +
      "another=\"paramValue\"; charset=\"us-ascii\"; charset3=\"something\""));

    // Check charset treatment
    DocFlavor charset = new DocFlavor("text/plain; charset=US-ascii; " +
      "nocharset=UoUo", "java.io.InputStream");

    h.checkPoint("Test charset treatment");
    h.check(charset.getParameter("charset").equals("us-ascii"));
    h.check(charset.getParameter("nocharset").equals("UoUo"));

    // Check for mimetype with comments
    DocFlavor comments = new DocFlavor("text/plain(Comment); " +
    "charset=\"us-ascii\" (Comment2)(Comment1)", "java.io.InputStream");

    h.checkPoint("Mimetype with comments");
    h.check(comments.getMediaSubtype().equals("plain"));
    h.check(comments.getParameter("charset").equals("us-ascii"));

    // Syntax checks
    h.checkPoint("Syntax checks");
    
    // Lowercase treatment of media type and media subtype
    DocFlavor lowercase = new DocFlavor("teXt/Plain; charset=US-ascii; " +
      "nocharset=UoUo", "java.io.InputStream");
    
    h.check(lowercase.getMediaType().equals("text"));
    h.check(lowercase.getMediaSubtype().equals("plain"));
    
    try
      {
        // wrongly quoted value
        new DocFlavor("text/plain; charset=us-ascii\"", "java.io.InputStream");
        h.check(false);
      }
    catch (IllegalArgumentException e)
      {
        h.check(true);
      }
    try
      {
        // wrong character
        new DocFlavor(" te,xt/plain; charset=us-ascii", "java.io.InputStream");
        h.check(false);
      }
    catch (IllegalArgumentException e)
      {        
        h.check(true);
      }
    try
      {
        // only values may be quoted
        new DocFlavor("text/plain; \"charset\"=us-ascii", "java.io.InputStream");
        h.check(false);
      }
    catch (IllegalArgumentException e)
      {
        h.check(true);
      }
    try
      {
        // ' is an allowed character
        new DocFlavor(" text/plain; charset=us-asc'ii", "java.io.InputStream");
        h.check(true);
      }
    catch (IllegalArgumentException e)
      {
        h.check(false);
      }
    try
      {
        // wrongly character in unqouted value
        new DocFlavor("text/plain; charset=?us-ascii", "java.io.InputStream");
        h.check(false);
      }
    catch (IllegalArgumentException e)
      {
        h.check(true);
      }
    try
      {
        // character in qouted value
        DocFlavor syntax = new DocFlavor("text/plain; param=\"?value.\"",
            "java.io.InputStream");
        h.check(syntax.getParameter("param").equals("?value."));
      }
    catch (IllegalArgumentException e)
      {
        h.check(false);
      }
    try
      {
        // character in qouted value
        new DocFlavor("text/plain; param=\"?vöal ue.\"", "java.io.InputStream");
        h.check(true);
      }
    catch (IllegalArgumentException e)
      {
        h.check(false);
      }
    try
      {
        // special characters in mime type
        DocFlavor syntax = new DocFlavor("application/vnd.cups-command",
            "java.io.InputStream");
        h.check(syntax.getMediaSubtype().equals("vnd.cups-command"));
      }
    catch (IllegalArgumentException e)
      {
        h.check(false);
      }
     
  }
}
