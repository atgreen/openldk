// Tags: JDK1.2

// Copyright (C) 2006 Robert Schuster <robertschuster@fsfe.org>

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

package gnu.testlet.javax.xml.parsers.DocumentBuilder;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.w3c.dom.DOMException;

import org.xml.sax.SAXException;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

/**
 * A helper class to print the contents of the DOM HTML tree.
 * @author Audrius Meskauskas (AudriusA@Bluewin.ch) 
 */
public class PR27864 implements Testlet
{

  String xml = "  <main>"
               + "  <element>"
               + "    <alwaysThere/>"
               + "  </element>"
               + "  <element>"
               + "    <alwaysThere>"
               + "      <sometimesThere>foo</sometimesThere>"
               + "    </alwaysThere>"
               + "  </element>"
               + "  <element>"
               + "    <alwaysThere>"
               + "      <sometimesThere>bar</sometimesThere>"
               + "    </alwaysThere>"
               + "  </element>"
               + "</main>";

  public void test(TestHarness harness)
  {
    harness.checkPoint("getElementsByTagName");
    try
      {
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();

        DocumentBuilder b = dbf.newDocumentBuilder();

        Document d = b.parse((new ByteArrayInputStream(xml.getBytes())));

	Element element0 = (Element) d.getElementsByTagName("element").item(0);
        Element always = (Element) element0.getElementsByTagName("alwaysThere").item(0);
        NodeList st_nl = always.getElementsByTagName("sometimesThere");

        harness.check(st_nl.getLength(), 0);
      }
     catch (IOException ioe)
      {
        harness.fail("IOException occured");
      }
     catch (DOMException de)
      {
        harness.fail("DOMException occured");
      }
     catch (SAXException saxe)
      {
        harness.fail("SAXException occured");
      }
     catch (ParserConfigurationException pce)
      {
        harness.fail("ParserConfigurationException occured");
      }
	
  }

}
