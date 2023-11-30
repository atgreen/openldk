// Tags: JDK1.4

// Tests some Node kinds if method .getChildNodes().getLength() return correctly.
// By: Pedro Izecksohn & Mark Wielaard

// Part of the Mauve project.

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
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor
// Boston, MA  02110-1301, USA.

package gnu.testlet.org.w3c.dom;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import gnu.testlet.ResourceNotFoundException;
import java.io.InputStream;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.*;

public class childNodesLength implements Testlet
{

  TestHarness harness;

  private void checkNode (Node node)
  {
    int nChilds = node.getChildNodes().getLength();

    if (
         (node instanceof CDATASection)||
         (node instanceof Comment)||
         (node instanceof DocumentType)||
         (node instanceof Notation)||
         (node instanceof ProcessingInstruction)||
         (node instanceof Text)
       )
    {
      harness.check (nChilds==0, node.getClass().getName());
    }
  }

  private void recurse (NodeList nl)
  {
    for (int i=0; i<nl.getLength(); i++)
    {

      Node node = nl.item (i);
      if (node==null) {continue;}

      checkNode (node);

      if (node instanceof DocumentType)
      {
        DocumentType dt = (DocumentType) node;
        NamedNodeMap nnm = dt.getNotations();

        for (int j=0; j<nnm.getLength(); j++)
        {
          checkNode((Notation)nnm.item(j));
        }
      }

      NodeList nl2 = node.getChildNodes();
      int nChilds = nl2.getLength();
      if (nChilds>0) {recurse (nl2);}
    }
  }

  public void test (TestHarness harness)
  {

    this.harness=harness;

    DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();

    DocumentBuilder db = null;
    try
    {
      db = dbf.newDocumentBuilder();
    }
    catch (javax.xml.parsers.ParserConfigurationException pce)
    {
      harness.debug (pce);
      harness.check(false);
      return;
    }

    // I need a xml file to parse.
    InputStream input = null;

    try
    {
      input = harness.getResourceStream ("gnu#testlet#org#w3c#dom#test.xml");
    }
    catch (ResourceNotFoundException rnfe)
    {
      harness.debug (rnfe);
      harness.check(false);
      return;
    }

    Document document = null;
    try
    {
      document = db.parse(input);
    }
    catch (Exception e)
    {
      harness.debug (e);
      harness.check(false);
      return;
    }

    recurse (document.getChildNodes());

  }

}
