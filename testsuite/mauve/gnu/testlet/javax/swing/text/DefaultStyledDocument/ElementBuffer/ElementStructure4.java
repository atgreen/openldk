// Tags: JDK1.2

// Copyright (C) 2006 Red Hat.

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
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
// Boston, MA 02110-1301 USA.

package gnu.testlet.javax.swing.text.DefaultStyledDocument.ElementBuffer;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.text.AttributeSet;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.Element;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;

public class ElementStructure4 extends DefaultStyledDocument implements Testlet
{
  /**
   * Starts the test run.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    try
      {
        h2 = harness;
        ElementStructure4 doc = new ElementStructure4();
        SimpleAttributeSet atts = new SimpleAttributeSet();
        Element root = doc.getDefaultRootElement();

        // Add strike trough text.
        atts.addAttribute(StyleConstants.StrikeThrough, Boolean.TRUE);
        doc.insertString(0, "Strike through text.\n", atts);
        atts.removeAttributes(atts);
        harness.checkPoint("after first insertion");
        harness.check(root.getElementCount(), 2);
        harness.check(root.getElement(0).getStartOffset(), 0);
        harness.check(root.getElement(0).getEndOffset(), 21);
        harness.check(root.getElement(1).getStartOffset(), 21);
        harness.check(root.getElement(1).getEndOffset(), 22);

        // Add plain text in front.
        doc.insertString(0, "a", null);
        harness.checkPoint("after second insertion");
        harness.check(root.getElement(0).getElementCount() == 2);
        harness.check(root.getElement(1).getElementCount() == 1);

        doc.insertString(1, "b", null);
        harness.checkPoint("after second insertion");
        harness.check(root.getElement(0).getElementCount() == 2);
        harness.check(root.getElement(1).getElementCount() == 1);

        harness.checkPoint("final structure");
        Element first = root.getElement(0).getElement(0);
        harness.check(first.getStartOffset() == 0);
        harness.check(first.getEndOffset() == 2);

        Element second = root.getElement(0).getElement(1);
        harness.check(second.getStartOffset() == 2);
        harness.check(second.getEndOffset() == 23);
        // printElements(doc.getDefaultRootElement(), 0);
      }
    catch (Exception t)
      {
        // t.printStackTrace();
        harness.debug(t);
      }
    catch (AssertionError e)
      {
        // e.printStackTrace();
        harness.debug(e);
      }
  }

  static TestHarness h2;

  static int numInserts = 0;

  static int numLeaves = 0;

  static int numBranches = 0;

  // We override the constructor so we can explicitly set the type of the
  // buffer to be our ElementBuffer2, allowing us to test some internals.
  public ElementStructure4()
  {
    super();
    buffer = new ElementBuffer2(createDefaultRoot());
  }

  // Overriding this method allows us to check that the right number
  // of newLines was encountered and that the event has the proper
  // offset and length.
  protected void insertUpdate(DefaultDocumentEvent ev, AttributeSet attr)
  {
    int l = ev.getLength();
    int o = ev.getOffset();
    if (numInserts == 0)
      {
        h2.checkPoint("first doc event");
        h2.check(o == 0);
        h2.check(l == 21);
      }
    else if (numInserts == 1)
    {
      h2.checkPoint("second doc event");
      h2.check(o == 0);      
      h2.check(l == 1);
    } 
    else if (numInserts == 2)
      {
        h2.checkPoint("third doc event");
        h2.check(o == 1);
        h2.check(l == 1);
      }
    else
      h2.fail ("too many calls to DefaultStyledDocument.insertUpdate");

    super.insertUpdate(ev, attr);
  }

  // A class to be the buffer of the styled document.
  // This allows us to check that some values are correct internally within
  // the ElementBuffer.
  public class ElementBuffer2 extends ElementBuffer
  {
    public ElementBuffer2(Element root)
    {
      super(root);
    }

    // This method allows us to check that the ElementSpecs generated by 
    // DefaultStyledDocument.insertUpdate are correct.
    protected void insertUpdate(ElementSpec[] data)
    {
      numInserts++;
      if (numInserts == 1)
        {
          h2.checkPoint("ElementBuffer insertUpdate: first insertion");
          h2.check (data.length == 3);
          h2.check(data[0].getType() == ElementSpec.ContentType);
          h2.check(data[0].getDirection() == ElementSpec.OriginateDirection);
          h2.check(data[0].getOffset() == 0);
          h2.check(data[0].getLength() == 21);

          h2.check(data[1].getType() == ElementSpec.EndTagType);
          h2.check(data[1].getDirection() == ElementSpec.OriginateDirection);
          h2.check(data[1].getOffset() == 0);
          h2.check(data[1].getLength() == 0);

          h2.check(data[2].getType() == ElementSpec.StartTagType);
          h2.check
            (data[2].getDirection() == ElementSpec.JoinFractureDirection);
          h2.check(data[2].getOffset() == 0);
          h2.check(data[2].getLength() == 0);
        }
      else if (numInserts == 2)
        {
          h2.checkPoint("ElementBuffer insertUpdate: second insertion");
          h2.check (data.length == 1);
          h2.check(data[0].getType() == ElementSpec.ContentType);
          h2.check(data[0].getDirection() == ElementSpec.OriginateDirection);
          h2.check(data[0].getOffset() == 0);
          h2.check(data[0].getLength() == 1);          
        }
      else if (numInserts == 3)
        {
          h2.checkPoint("ElementBuffer insertUpdate: third insertion");
          h2.check (data.length == 1);
          h2.check(data[0].getType() == ElementSpec.ContentType);
          h2.check
            (data[0].getDirection() == ElementSpec.JoinPreviousDirection);
          h2.check(data[0].getOffset() == 0);
          h2.check(data[0].getLength() == 1);          
        }
      else
        h2.fail("too many ElementSpecs created");
      super.insertUpdate(data);
    }
  }
}