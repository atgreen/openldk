// Tags: not-a-test
// Copyright (C) 2004 Audrius Meskauskas <audriusa@bluewin.ch>

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

import gnu.testlet.TestHarness;

import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.io.ByteArrayInputStream;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

/**
 * A helper class to print the contents of the DOM HTML tree.
 * @author Audrius Meskauskas (AudriusA@Bluewin.ch) 
 */
class Verifyer
{
  // Change these parameters for getting more human-readable output.
  String EOLN = " ";
  TestHarness harness;
  boolean IDENTATION = false;

  String getImage(Node node)
  {
    StringBuffer b = new StringBuffer();
    print(b, node, 0);
    return b.toString().trim();
  }

  void print(StringBuffer stream, Node node, int ident)
  {
    StringBuffer tab = new StringBuffer();
    stream.append(EOLN);
    if (IDENTATION)
      for (int i = 0; i < ident; i++)
        {
          tab.append(' ');
        }

    if (node == null)
      {
        stream.append(tab + " null node");
        return;
      }

    String nn = node.getNodeName();

    if (node.getNamespaceURI() != null)
      nn = node.getNamespaceURI() + ":" + nn;

    stream.append(tab + nn);
    if (node.getNodeValue() != null)
      {
        stream.append(EOLN);
        stream.append(tab + " = '" + node.getNodeValue() + "'");
      }

    NamedNodeMap attributes = node.getAttributes();
    if (attributes != null && attributes.getLength() != 0)
      {
        stream.append(' ');
        for (int i = 0; i < attributes.getLength(); i++)
          {
            Node a = attributes.item(i);
            stream.append(a.getNodeName() + "='" + a.getNodeValue() + "'");
          }
      }

    ident += 4;

    NodeList childs = node.getChildNodes();
    if (childs != null)
      for (int i = 0; i < childs.getLength(); i++)
        {
          print(stream, childs.item(i), ident);
        }
  }

  void verify(String xml, String image, String message)
  {
    harness.checkPoint(message);
    try
      {
        boolean validation = false;
        boolean ignoreWhitespace = false; // false throws exception with saxon
        boolean ignoreComments = false;
        boolean putCDATAIntoText = false;
        boolean createEntityRefs = false;
        boolean namespaces = true;

        // Step 1: create a DocumentBuilderFactory and configure it
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();

        // Optional: set various configuration options
        dbf.setValidating(validation);
        dbf.setIgnoringComments(ignoreComments);
        dbf.setIgnoringElementContentWhitespace(ignoreWhitespace);
        dbf.setCoalescing(putCDATAIntoText);
        dbf.setNamespaceAware(namespaces);

        // The opposite of creating entity ref nodes is expanding them inline
        dbf.setExpandEntityReferences(!createEntityRefs);

        DocumentBuilder b = dbf.newDocumentBuilder();

        Document d = b.parse((new ByteArrayInputStream(xml.getBytes())));

        String result = getImage(d);

        if (!result.equals(image))
          {
            System.out.println("Exp: " + image);
            System.out.println("Rez: " + result);
            harness.check(result, image, message);
          }
      }
    catch (Exception ex)
      {
        if (ex != null)
          harness.fail(message + ":" + ex.getClass().getName() + ":" +
                       ex.getMessage()
                      );
        else
          harness.fail(message + ": null exception");
      }
  }
}
