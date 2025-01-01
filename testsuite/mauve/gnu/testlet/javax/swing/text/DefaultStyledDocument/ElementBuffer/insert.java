// Tags: JDK1.2

// Copyright (C) 2005 Roman Kennke (kennke@aicas.com)

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

package gnu.testlet.javax.swing.text.DefaultStyledDocument.ElementBuffer;

import java.util.EmptyStackException;

import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.Element;
import javax.swing.text.SimpleAttributeSet;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Extensive tests for the ElementBuffer.insert() method. In order to check
 * this method, we make use of the DefaultStyledDocument.insert() method, which
 * passes an array of ElementSpecs to the element buffer. We then check for
 * different scenarios the resulting document structure and the document event
 * that is fired.
 * 
 * @author Roman Kennke (kennke@aicas.com)
 */
// TODO: Add more tests for EndTagType and StartTagType.
public class insert implements Testlet, DocumentListener
{

  /**
   * The document event that is fired in response to an element change. We
   * want to check that to see if the correct event is fired.
   */
  private DocumentEvent documentEvent;

  /**
   * A document for testing. It overrides some protected method for public
   * access.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  static class TestDocument extends DefaultStyledDocument
  {
    /**
     * Overridden to provide public access.
     *
     * @param offset the offset at which to insert the data
     * @param data the data spec to insert
     */
    public void insert(int offset, ElementSpec[] data)
    {
      try
        {
          super.insert(offset, data);
        }
      catch (BadLocationException e)
        {
          throw new RuntimeException(e);
        }
    }
  }

  /**
   * Starts the test run.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    testOriginate1(harness);
    testOriginate2(harness);
    testOriginate3(harness);
    testJoinPrevious1(harness);
    testJoinPrevious2(harness);
    testJoinPrevious3(harness);
    testJoinNext1(harness);
    testJoinNext2(harness);
    testJoinNext3(harness);
    testEndTag1(harness);
    testEndTag2(harness);
    testEndTag3(harness);
    testEndTag4(harness);
    testEndTag5(harness);

    testNewlines(harness);
    testNewlines2(harness);
  }

  /**
   * Tests content insertion with OriginateDirection. This test inserts
   * a content element between two existing elements.
   *
   * @param h the test harness to use
   */
  private void testOriginate1(TestHarness h)
  {
    h.checkPoint("testOriginate1");
    TestDocument doc = new TestDocument();
    doc.addDocumentListener(this);
    char[] text = new char[] {'H', 'e', 'l', 'l', 'o'};
    SimpleAttributeSet atts = new SimpleAttributeSet();
    TestDocument.ElementSpec spec =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.ContentType,
                                   text, 0, 5);
    spec.setDirection(TestDocument.ElementSpec.OriginateDirection);

    TestDocument.ElementSpec[] specs = new TestDocument.ElementSpec[]{ spec };

    // Create the precondition, two elements [Hello], one at 0 and one at 5.
    doc.insert(0, specs);
    doc.insert(5, specs);

    // Check precondition. (3 child elements).
    Element root = doc.getDefaultRootElement();
    Element par = root.getElement(0);
    h.check(par.getElementCount(), 3);
    
    // Now check what comes out when we insert one element at 5.
    doc.insert(5, specs);

    // We have one paragraph in the root element.
    root = doc.getDefaultRootElement();
    h.check(root.getElementCount(), 1);

    // We should now have 4 children in the paragraph.
    par = root.getElement(0);
    h.check(par.getElementCount(), 4);
    Element el1 = par.getElement(0);
    h.check(el1.getStartOffset(), 0);
    h.check(el1.getEndOffset(), 5);
    Element el2 = par.getElement(1);
    h.check(el2.getStartOffset(), 5);
    h.check(el2.getEndOffset(), 10);
    Element el3 = par.getElement(2);
    h.check(el3.getStartOffset(), 10);
    h.check(el3.getEndOffset(), 15);

    // Now check the document event that was fired.
    // No changes for the root.
    DocumentEvent.ElementChange ec = documentEvent.getChange(root);
    h.check(ec, null);
    // Two removals and one add for the paragraph.
    ec = documentEvent.getChange(par);
    h.check(ec.getChildrenAdded().length, 2);
    h.check(ec.getChildrenRemoved().length, 1);
    h.check(ec.getIndex(), 0);
  }

  /**
   * Tests content insertion with OriginateDirection. This test inserts
   * a content element in between the first existing element.
   *
   * @param h the test harness to use
   */
  private void testOriginate2(TestHarness h)
  {
    h.checkPoint("testOriginate2");
    TestDocument doc = new TestDocument();
    doc.addDocumentListener(this);
    char[] text = new char[] {'H', 'e', 'l', 'l', 'o'};
    SimpleAttributeSet atts = new SimpleAttributeSet();
    TestDocument.ElementSpec spec =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.ContentType,
                                   text, 0, 5);
    spec.setDirection(TestDocument.ElementSpec.OriginateDirection);

    TestDocument.ElementSpec[] specs = new TestDocument.ElementSpec[]{ spec };

    // Create the precondition, two elements [Hello], one at 0 and one at 5.
    doc.insert(0, specs);
    doc.insert(5, specs);

    // Now check what comes out when we insert one element at 2.
    doc.insert(2, specs);

    // We have one paragraph in the root element.
    Element root = doc.getDefaultRootElement();
    h.check(root.getElementCount(), 1);

    // We should now have 5 children in the paragraph.
    Element par = root.getElement(0);
    h.check(par.getElementCount(), 5);
    Element el1 = par.getElement(0);
    h.check(el1.getStartOffset(), 0);
    h.check(el1.getEndOffset(), 2);
    Element el2 = par.getElement(1);
    h.check(el2.getStartOffset(), 2);
    h.check(el2.getEndOffset(), 7);
    Element el3 = par.getElement(2);
    h.check(el3.getStartOffset(), 7);
    h.check(el3.getEndOffset(), 10);
    Element el4 = par.getElement(3);
    h.check(el4.getStartOffset(), 10);
    h.check(el4.getEndOffset(), 15);

    // Now check the document event that was fired.
    // No changes for the root.
    DocumentEvent.ElementChange ec = documentEvent.getChange(root);
    h.check(ec, null);
    // Two removals and one add for the paragraph.
    ec = documentEvent.getChange(par);
    h.check(ec.getChildrenAdded().length, 3);
    h.check(ec.getChildrenRemoved().length, 1);
    h.check(ec.getIndex(), 0);
  }

  /**
   * Tests content insertion with OriginateDirection. This test inserts
   * a content element in between the second existing element.
   *
   * @param h the test harness to use
   */
  private void testOriginate3(TestHarness h)
  {
    h.checkPoint("testOriginate3");
    TestDocument doc = new TestDocument();
    doc.addDocumentListener(this);
    char[] text = new char[] {'H', 'e', 'l', 'l', 'o'};
    SimpleAttributeSet atts = new SimpleAttributeSet();
    TestDocument.ElementSpec spec =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.ContentType,
                                   text, 0, 5);
    spec.setDirection(TestDocument.ElementSpec.OriginateDirection);

    TestDocument.ElementSpec[] specs = new TestDocument.ElementSpec[]{ spec };

    // Create the precondition, two elements [Hello], one at 0 and one at 5.
    doc.insert(0, specs);
    doc.insert(5, specs);

    // Now check what comes out when we insert one element at 7.
    doc.insert(7, specs);

    // We have one paragraph in the root element.
    Element root = doc.getDefaultRootElement();
    h.check(root.getElementCount(), 1);

    // We should now have 5 children in the paragraph.
    Element par = root.getElement(0);
    h.check(par.getElementCount(), 5);
    Element el1 = par.getElement(0);
    h.check(el1.getStartOffset(), 0);
    h.check(el1.getEndOffset(), 5);
    Element el2 = par.getElement(1);
    h.check(el2.getStartOffset(), 5);
    h.check(el2.getEndOffset(), 7);
    Element el3 = par.getElement(2);
    h.check(el3.getStartOffset(), 7);
    h.check(el3.getEndOffset(), 12);
    Element el4 = par.getElement(3);
    h.check(el4.getStartOffset(), 12);
    h.check(el4.getEndOffset(), 15);

    // Now check the document event that was fired.
    // No changes for the root.
    DocumentEvent.ElementChange ec = documentEvent.getChange(root);
    h.check(ec, null);
    // Two removals and one add for the paragraph.
    ec = documentEvent.getChange(par);
    h.check(ec.getChildrenAdded().length, 3);
    h.check(ec.getChildrenRemoved().length, 1);
    h.check(ec.getIndex(), 1);
  }

  /**
   * Tests content insertion with JoinPreviousDirection. This test inserts
   * a content element between two existing elements.
   *
   * @param h the test harness to use
   */
  private void testJoinPrevious1(TestHarness h)
  {
    h.checkPoint("testJoinPrevious1");
    TestDocument doc = new TestDocument();
    doc.addDocumentListener(this);
    char[] text = new char[] {'H', 'e', 'l', 'l', 'o'};
    SimpleAttributeSet atts = new SimpleAttributeSet();
    TestDocument.ElementSpec spec =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.ContentType,
                                   text, 0, 5);
    spec.setDirection(TestDocument.ElementSpec.OriginateDirection);

    TestDocument.ElementSpec[] specs = new TestDocument.ElementSpec[]{ spec };

    // Create the precondition, two elements [Hello], one at 0 and one at 5.
    doc.insert(0, specs);
    doc.insert(5, specs);

    // Now check what comes out when we insert one element at 5.
    spec.setDirection(TestDocument.ElementSpec.JoinPreviousDirection);
    documentEvent = null;
    doc.insert(5, specs);

    // We have one paragraph in the root element.
    Element root = doc.getDefaultRootElement();
    h.check(root.getElementCount(), 1);

    // We should now have 3 children in the paragraph.
    Element par = root.getElement(0);
    h.check(par.getElementCount(), 3);
    Element el1 = par.getElement(0);
    h.check(el1.getStartOffset(), 0);
    h.check(el1.getEndOffset(), 10);
    Element el2 = par.getElement(1);
    h.check(el2.getStartOffset(), 10);
    h.check(el2.getEndOffset(), 15);

    // No changes for the document.
    // TODO: The document event here is an InsertUndo from GapContent.
    // We currently don't have such thing and return null.
    // h.check(documentEvent, null);
  }

  /**
   * Tests content insertion with JoinPreviousDirection. This test inserts
   * a content element in between the first existing element.
   *
   * @param h the test harness to use
   */
  private void testJoinPrevious2(TestHarness h)
  {
    h.checkPoint("testJoinPrevious2");
    TestDocument doc = new TestDocument();
    doc.addDocumentListener(this);
    char[] text = new char[] {'H', 'e', 'l', 'l', 'o'};
    SimpleAttributeSet atts = new SimpleAttributeSet();
    TestDocument.ElementSpec spec =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.ContentType,
                                   text, 0, 5);
    spec.setDirection(TestDocument.ElementSpec.OriginateDirection);

    TestDocument.ElementSpec[] specs = new TestDocument.ElementSpec[]{ spec };

    // Create the precondition, two elements [Hello], one at 0 and one at 5.
    doc.insert(0, specs);
    doc.insert(5, specs);

    // Now check what comes out when we insert one element at 2.
    spec.setDirection(TestDocument.ElementSpec.JoinPreviousDirection);
    documentEvent = null;
    doc.insert(2, specs);

    // We have one paragraph in the root element.
    Element root = doc.getDefaultRootElement();
    h.check(root.getElementCount(), 1);

    // We should now have 3 children in the paragraph.
    Element par = root.getElement(0);
    h.check(par.getElementCount(), 3);
    Element el1 = par.getElement(0);
    h.check(el1.getStartOffset(), 0);
    h.check(el1.getEndOffset(), 10);
    Element el2 = par.getElement(1);
    h.check(el2.getStartOffset(), 10);
    h.check(el2.getEndOffset(), 15);

    // No changes for the document.
    // TODO: The document event here is an InsertUndo from GapContent.
    // We currently don't have such thing and return null.
    // h.check(documentEvent, null);
  }

  /**
   * Tests content insertion with JoinPreviousDirection. This test inserts
   * a content element in between the second existing element.
   *
   * @param h the test harness to use
   */
  private void testJoinPrevious3(TestHarness h)
  {
    h.checkPoint("testJoinPrevious3");
    TestDocument doc = new TestDocument();
    doc.addDocumentListener(this);
    char[] text = new char[] {'H', 'e', 'l', 'l', 'o'};
    SimpleAttributeSet atts = new SimpleAttributeSet();
    TestDocument.ElementSpec spec =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.ContentType,
                                   text, 0, 5);
    spec.setDirection(TestDocument.ElementSpec.OriginateDirection);

    TestDocument.ElementSpec[] specs = new TestDocument.ElementSpec[]{ spec };

    // Create the precondition, two elements [Hello], one at 0 and one at 5.
    doc.insert(0, specs);
    doc.insert(5, specs);

    // Now check what comes out when we insert one element at 7.
    spec.setDirection(TestDocument.ElementSpec.JoinPreviousDirection);
    documentEvent = null;
    doc.insert(7, specs);

    // We have one paragraph in the root element.
    Element root = doc.getDefaultRootElement();
    h.check(root.getElementCount(), 1);

    // We should now have 5 children in the paragraph.
    Element par = root.getElement(0);
    h.check(par.getElementCount(), 3);
    Element el1 = par.getElement(0);
    h.check(el1.getStartOffset(), 0);
    h.check(el1.getEndOffset(), 5);
    Element el2 = par.getElement(1);
    h.check(el2.getStartOffset(), 5);
    h.check(el2.getEndOffset(), 15);

    // No changes for the document.
    // TODO: The document event here is an InsertUndo from GapContent.
    // We currently don't have such thing and return null.
    // h.check(documentEvent, null);
  }

  /**
   * Tests content insertion with JoinNextDirection. This test inserts
   * a content element between two existing elements.
   *
   * @param h the test harness to use
   */
  private void testJoinNext1(TestHarness h)
  {
    h.checkPoint("testJoinNext1");
    TestDocument doc = new TestDocument();
    doc.addDocumentListener(this);
    char[] text = new char[] {'H', 'e', 'l', 'l', 'o'};
    SimpleAttributeSet atts = new SimpleAttributeSet();
    TestDocument.ElementSpec spec =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.ContentType,
                                   text, 0, 5);
    spec.setDirection(TestDocument.ElementSpec.OriginateDirection);

    TestDocument.ElementSpec[] specs = new TestDocument.ElementSpec[]{ spec };

    // Create the precondition, two elements [Hello], one at 0 and one at 5.
    doc.insert(0, specs);
    doc.insert(5, specs);

    // Now check what comes out when we insert one element at 5.
    spec.setDirection(TestDocument.ElementSpec.JoinNextDirection);
    doc.insert(5, specs);

    // We have one paragraph in the root element.
    Element root = doc.getDefaultRootElement();
    h.check(root.getElementCount(), 1);

    // We should now have 3 children in the paragraph.
    Element par = root.getElement(0);
    h.check(par.getElementCount(), 3);
    Element el1 = par.getElement(0);
    h.check(el1.getStartOffset(), 0);
    h.check(el1.getEndOffset(), 5);
    Element el2 = par.getElement(1);
    h.check(el2.getStartOffset(), 5);
    h.check(el2.getEndOffset(), 15);

    // Now check the document event that was fired.
    // No changes for the root.
    DocumentEvent.ElementChange ec = documentEvent.getChange(root);
    h.check(ec, null);
    // No structural change for the paragraph.
    ec = documentEvent.getChange(par);
    h.check(ec.getChildrenAdded().length, 2);
    h.check(ec.getChildrenRemoved().length, 2);
    h.check(ec.getIndex(), 0);
  }

  /**
   * Tests content insertion with JoinPreviousDirection. This test inserts
   * a content element in between the first existing element.
   *
   * @param h the test harness to use
   */
  private void testJoinNext2(TestHarness h)
  {
    h.checkPoint("testJoinNext2");
    TestDocument doc = new TestDocument();
    doc.addDocumentListener(this);
    char[] text = new char[] {'H', 'e', 'l', 'l', 'o'};
    SimpleAttributeSet atts = new SimpleAttributeSet();
    TestDocument.ElementSpec spec =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.ContentType,
                                   text, 0, 5);
    spec.setDirection(TestDocument.ElementSpec.OriginateDirection);

    TestDocument.ElementSpec[] specs = new TestDocument.ElementSpec[]{ spec };

    // Create the precondition, two elements [Hello], one at 0 and one at 5.
    doc.insert(0, specs);
    doc.insert(5, specs);

    // Now check what comes out when we insert one element at 2.
    spec.setDirection(TestDocument.ElementSpec.JoinNextDirection);
    doc.insert(2, specs);

    // We have one paragraph in the root element.
    Element root = doc.getDefaultRootElement();
    h.check(root.getElementCount(), 1);

    // We should now have 3 children in the paragraph.
    Element par = root.getElement(0);
    h.check(par.getElementCount(), 3);
    Element el1 = par.getElement(0);
    h.check(el1.getStartOffset(), 0);
    h.check(el1.getEndOffset(), 2);
    Element el2 = par.getElement(1);
    h.check(el2.getStartOffset(), 2);
    h.check(el2.getEndOffset(), 15);

    // Now check the document event that was fired.
    // No changes for the root.
    DocumentEvent.ElementChange ec = documentEvent.getChange(root);
    h.check(ec, null);
    // No structural change for the paragraph.
    ec = documentEvent.getChange(par);
    h.check(ec.getChildrenAdded().length, 2);
    h.check(ec.getChildrenRemoved().length, 2);
    h.check(ec.getIndex(), 0);
  }

  /**
   * Tests content insertion with JoinPreviousDirection. This test inserts
   * a content element in between the second existing element.
   *
   * @param h the test harness to use
   */
  private void testJoinNext3(TestHarness h)
  {
    h.checkPoint("testJoinNext3");
    TestDocument doc = new TestDocument();
    doc.addDocumentListener(this);
    char[] text = new char[] {'H', 'e', 'l', 'l', 'o'};
    SimpleAttributeSet atts = new SimpleAttributeSet();
    TestDocument.ElementSpec spec =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.ContentType,
                                   text, 0, 5);
    spec.setDirection(TestDocument.ElementSpec.OriginateDirection);

    TestDocument.ElementSpec[] specs = new TestDocument.ElementSpec[]{ spec };

    // Create the precondition, two elements [Hello], one at 0 and one at 5.
    doc.insert(0, specs);
    doc.insert(5, specs);

    // Now check what comes out when we insert one element at 7.
    spec.setDirection(TestDocument.ElementSpec.JoinNextDirection);
    doc.insert(7, specs);

    // We have one paragraph in the root element.
    Element root = doc.getDefaultRootElement();
    h.check(root.getElementCount(), 1);

    // We should now have 3 children in the paragraph.
    Element par = root.getElement(0);
    h.check(par.getElementCount(), 3);
    Element el1 = par.getElement(0);
    h.check(el1.getStartOffset(), 0);
    h.check(el1.getEndOffset(), 5);
    Element el2 = par.getElement(1);
    h.check(el2.getStartOffset(), 5);
    h.check(el2.getEndOffset(), 7);
    Element el3 = par.getElement(2);
    h.check(el3.getStartOffset(), 7);
    h.check(el3.getEndOffset(), 16);

    // Now check the document event that was fired.
    // No changes for the root.
    DocumentEvent.ElementChange ec = documentEvent.getChange(root);
    h.check(ec, null);
    // No structural change for the paragraph.
    ec = documentEvent.getChange(par);
    h.check(ec.getChildrenAdded().length, 2);
    h.check(ec.getChildrenRemoved().length, 2);
    h.check(ec.getIndex(), 1);
  }

  /**
   * Tests insertion of some simple EndTags. This test inserts 3
   * EndTag elements in between the second existing element. This is more
   * then the element stack has to offer. Still, this action alone does
   * nothing. Obviously the real insertion is only performed when it is
   * followed by a content insertion. For this case, see the other EndTag
   * tests.
   *
   * @param h the test harness to use
   */
  private void testEndTag1(TestHarness h)
  {
    h.checkPoint("testEndTag1");
    TestDocument doc = new TestDocument();
    doc.addDocumentListener(this);
    char[] text = new char[] {'H', 'e', 'l', 'l', 'o'};
    SimpleAttributeSet atts = new SimpleAttributeSet();
    TestDocument.ElementSpec spec =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.ContentType,
                                   text, 0, 5);
    spec.setDirection(TestDocument.ElementSpec.OriginateDirection);

    TestDocument.ElementSpec[] specs = new TestDocument.ElementSpec[]{ spec };

    // Create the precondition, two elements [Hello], one at 0 and one at 5.
    doc.insert(0, specs);
    doc.insert(5, specs);

    // Now check what comes out when we insert one element at 7.
    spec = new TestDocument.ElementSpec(atts,
                                        TestDocument.ElementSpec.EndTagType);
    specs =  new TestDocument.ElementSpec[]{ spec, spec, spec };
    documentEvent = null;
    doc.insert(7, specs);

    // We have one paragraph in the root element.
    Element root = doc.getDefaultRootElement();
    h.check(root.getElementCount(), 1);

    // We should now have 3 children in the paragraph.
    Element par = root.getElement(0);
    h.check(par.getElementCount(), 3);
    Element el1 = par.getElement(0);
    h.check(el1.getStartOffset(), 0);
    h.check(el1.getEndOffset(), 5);
    Element el2 = par.getElement(1);
    h.check(el2.getStartOffset(), 5);
    h.check(el2.getEndOffset(), 10);
    Element el3 = par.getElement(2);
    h.check(el3.getStartOffset(), 10);
    h.check(el3.getEndOffset(), 11);

    // Now check the document event that was fired.
    // No changes for the root.
    h.check(documentEvent, null);
  }

  /**
   * Tests insertion of some simple EndTags. This test inserts 3
   * EndTag elements in between the second existing element. This is more
   * then the element stack has to offer. Still, this action alone does
   * nothing. Therefore this insertion is followed by a StartTag insertion.
   * Obviously the real insertion is only performed when it is
   * followed by a content insertion. For this case, see the other EndTag
   * tests.
   *
   * @param h the test harness to use
   */
  private void testEndTag2(TestHarness h)
  {
    h.checkPoint("testEndTag2");
    TestDocument doc = new TestDocument();
    doc.addDocumentListener(this);
    char[] text = new char[] {'H', 'e', 'l', 'l', 'o'};
    SimpleAttributeSet atts = new SimpleAttributeSet();
    TestDocument.ElementSpec spec =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.ContentType,
                                   text, 0, 5);
    spec.setDirection(TestDocument.ElementSpec.OriginateDirection);

    TestDocument.ElementSpec[] specs = new TestDocument.ElementSpec[]{ spec };

    // Create the precondition, two elements [Hello], one at 0 and one at 5.
    doc.insert(0, specs);
    doc.insert(5, specs);

    // Now check what comes out when we insert one element at 7.
    TestDocument.ElementSpec spec1 = new TestDocument.ElementSpec(atts,
                                        TestDocument.ElementSpec.EndTagType);
    TestDocument.ElementSpec spec2 = new TestDocument.ElementSpec(atts,
                                        TestDocument.ElementSpec.StartTagType);
    specs =  new TestDocument.ElementSpec[]{ spec1, spec1, spec1, spec2};
    documentEvent = null;
    doc.insert(7, specs);

    // We have one paragraph in the root element.
    Element root = doc.getDefaultRootElement();
    h.check(root.getElementCount(), 1);

    // We should now have 3 children in the paragraph.
    Element par = root.getElement(0);
    h.check(par.getElementCount(), 3);
    Element el1 = par.getElement(0);
    h.check(el1.getStartOffset(), 0);
    h.check(el1.getEndOffset(), 5);
    Element el2 = par.getElement(1);
    h.check(el2.getStartOffset(), 5);
    h.check(el2.getEndOffset(), 10);
    Element el3 = par.getElement(2);
    h.check(el3.getStartOffset(), 10);
    h.check(el3.getEndOffset(), 11);

    // No structural change for the paragraph.
    h.check(documentEvent, null);
  }

  /**
   * Tests insertion of some simple EndTags. This test inserts 3
   * EndTag elements in between the second existing element. This is more
   * then the element stack has to offer. Still, this action alone does
   * nothing. Therefore this insertion is followed by a content insertion.
   * This triggers the insertion of the end tags and since we have inserted
   * more end tags than the stack has to offer, this throws an
   * EmptyStackException.
   *
   * @param h the test harness to use
   */
  private void testEndTag3(TestHarness h)
  {
    h.checkPoint("testEndTag3");
    TestDocument doc = new TestDocument();
    doc.addDocumentListener(this);
    char[] text = new char[] {'H', 'e', 'l', 'l', 'o'};
    SimpleAttributeSet atts = new SimpleAttributeSet();
    TestDocument.ElementSpec spec =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.ContentType,
                                   text, 0, 5);
    spec.setDirection(TestDocument.ElementSpec.OriginateDirection);

    TestDocument.ElementSpec[] specs = new TestDocument.ElementSpec[]{ spec };

    // Create the precondition, two elements [Hello], one at 0 and one at 5.
    doc.insert(0, specs);
    doc.insert(5, specs);

    // Now check what comes out when we insert one element at 7.
    TestDocument.ElementSpec spec1 = new TestDocument.ElementSpec(atts,
                                        TestDocument.ElementSpec.EndTagType);
    specs =  new TestDocument.ElementSpec[]{ spec1, spec1, spec};
    documentEvent = null;
    try
      {
        doc.insert(7, specs);
        h.fail("EmptyStackException must be thrown");
      }
    catch (EmptyStackException ex)
      {
        h.check(true);
      }

    // We have one paragraph in the root element.
    Element root = doc.getDefaultRootElement();
    h.check(root.getElementCount(), 1);

    // We should now have 3 children in the paragraph.
    Element par = root.getElement(0);
    h.check(par.getElementCount(), 3);
    Element el1 = par.getElement(0);
    h.check(el1.getStartOffset(), 0);
    h.check(el1.getEndOffset(), 5);
    Element el2 = par.getElement(1);
    h.check(el2.getStartOffset(), 5);
    h.check(el2.getEndOffset(), 15);
    Element el3 = par.getElement(2);
    h.check(el3.getStartOffset(), 15);
    h.check(el3.getEndOffset(), 16);

    // No structural change for the paragraph.
    h.check(documentEvent, null);
  }

  /**
   * Tests insertion of some simple EndTags. This test inserts 3
   * EndTag elements in between the second existing element. This is more
   * then the element stack has to offer. Still, this action alone does
   * nothing. Therefore this insertion is followed by a content insertion.
   *
   * @param h the test harness to use
   */
  private void testEndTag4(TestHarness h)
  {
    h.checkPoint("testEndTag4");
    TestDocument doc = new TestDocument();
    doc.addDocumentListener(this);
    char[] text = new char[] {'H', 'e', 'l', 'l', 'o'};
    SimpleAttributeSet atts = new SimpleAttributeSet();
    TestDocument.ElementSpec spec =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.ContentType,
                                   text, 0, 5);
    spec.setDirection(TestDocument.ElementSpec.OriginateDirection);

    TestDocument.ElementSpec[] specs = new TestDocument.ElementSpec[]{ spec };

    // Create the precondition, two elements [Hello], one at 0 and one at 5.
    doc.insert(0, specs);
    doc.insert(5, specs);

    // Now check what comes out when we insert one element at 7.
    TestDocument.ElementSpec spec1 = new TestDocument.ElementSpec(atts,
                                        TestDocument.ElementSpec.EndTagType);
    specs =  new TestDocument.ElementSpec[]{ spec1, spec};
    documentEvent = null;
    doc.insert(7, specs);

    // We have one paragraph in the root element.
    Element root = doc.getDefaultRootElement();
    h.check(root.getElementCount(), 3);

    // We should now have 2 children in the first paragraph.
    Element par1 = root.getElement(0);
    h.check(par1.getElementCount(), 2);
    Element el = par1.getElement(0);
    h.check(el.getStartOffset(), 0);
    h.check(el.getEndOffset(), 5);
    el = par1.getElement(1);
    h.check(el.getStartOffset(), 5);
    h.check(el.getEndOffset(), 7);

    // We should now have 1 leaf element between the first and second
    // paragraph.
    el = root.getElement(1);
    h.check(el.getElementCount(), 0);
    h.check(el.getStartOffset(), 7);
    h.check(el.getEndOffset(), 12);

    // We should now have 2 children in the first paragraph.
    Element par2 = root.getElement(2);
    h.check(par2.getElementCount(), 2);
    el = par2.getElement(0);
    h.check(el.getStartOffset(), 12);
    h.check(el.getEndOffset(), 15);
    el = par2.getElement(1);
    h.check(el.getStartOffset(), 15);
    h.check(el.getEndOffset(), 16);

    // Some structural changes for the root.
    DocumentEvent.ElementChange ec = documentEvent.getChange(root);
    h.check(ec.getChildrenRemoved().length, 0);
    h.check(ec.getChildrenAdded().length, 2);
    h.check(ec.getIndex(), 1);

    // Check changes for paragraph 1
    ec = documentEvent.getChange(par1);
    h.check(ec.getChildrenRemoved().length, 2);
    h.check(ec.getChildrenAdded().length, 1);
    h.check(ec.getIndex(), 1);

    // Check changes for paragraph 2
    ec = documentEvent.getChange(par2);
    h.check(ec, null);
  }

  /**
   * Tests insertion of some simple EndTags. This test inserts 3
   * EndTag elements in between the second existing element. This is more
   * then the element stack has to offer. Still, this action alone does
   * nothing. Therefore this insertion is followed by a content insertion.
   *
   * @param h the test harness to use
   */
  private void testEndTag5(TestHarness h)
  {
    h.checkPoint("testEndTag5");
    TestDocument doc = new TestDocument();
    doc.addDocumentListener(this);
    char[] text = new char[] {'H', 'e', 'l', 'l', 'o'};
    SimpleAttributeSet atts = new SimpleAttributeSet();
    TestDocument.ElementSpec spec =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.ContentType,
                                   text, 0, 5);
    spec.setDirection(TestDocument.ElementSpec.OriginateDirection);

    TestDocument.ElementSpec[] specs = new TestDocument.ElementSpec[]{ spec };

    // Create the precondition, two elements [Hello], one at 0 and one at 5.
    doc.insert(0, specs);
    doc.insert(5, specs);

    // Now check what comes out when we insert one element at 5.
    TestDocument.ElementSpec spec1 = new TestDocument.ElementSpec(atts,
                                        TestDocument.ElementSpec.EndTagType);
    TestDocument.ElementSpec spec2 = new TestDocument.ElementSpec(atts,
                                        TestDocument.ElementSpec.StartTagType);
    specs =  new TestDocument.ElementSpec[]{ spec, spec1, spec2};
    documentEvent = null;
    doc.insert(5, specs);

    // We have one paragraph in the root element.
    Element root = doc.getDefaultRootElement();
    h.check(root.getElementCount(), 2);

    // We should now have 2 children in the first paragraph.
    Element par1 = root.getElement(0);
    h.check(par1.getElementCount(), 2);
    Element el = par1.getElement(0);
    h.check(el.getStartOffset(), 0);
    h.check(el.getEndOffset(), 5);
    el = par1.getElement(1);
    h.check(el.getStartOffset(), 5);
    h.check(el.getEndOffset(), 10);

    // We should now have 2 children in the first paragraph.
    Element par2 = root.getElement(1);
    h.check(par2.getElementCount(), 2);
    el = par2.getElement(0);
    h.check(el.getStartOffset(), 10);
    h.check(el.getEndOffset(), 15);
    el = par2.getElement(1);
    h.check(el.getStartOffset(), 15);
    h.check(el.getEndOffset(), 16);

    // Some structural changes for the root.
    DocumentEvent.ElementChange ec = documentEvent.getChange(root);
    h.check(ec.getChildrenRemoved().length, 0);
    h.check(ec.getChildrenAdded().length, 1);
    h.check(ec.getIndex(), 1);

    // Check changes for paragraph 1
    ec = documentEvent.getChange(par1);
    h.check(ec.getChildrenRemoved().length, 3);
    h.check(ec.getChildrenAdded().length, 2);
    h.check(ec.getIndex(), 0);

    // Check changes for paragraph 2
    ec = documentEvent.getChange(par2);
    h.check(ec, null);
  }

  /**
   * Inserts 'a\na\n' and checks the results. The characters are inserted
   * one by one, as if somebody typed it in by keyboard.
   *
   * @param h the test harness to use
   */
  private void testNewlines(TestHarness h)
  {
    h.checkPoint("testNewlines");
    TestDocument doc = new TestDocument();
    doc.addDocumentListener(this);

    // The first 'a'
    char[] text = new char[] {'a'};
    SimpleAttributeSet atts = new SimpleAttributeSet();
    TestDocument.ElementSpec[] specs = new TestDocument.ElementSpec[1];
    specs[0] =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.ContentType,
                                   text, 0, 1);
    specs[0].setDirection(TestDocument.ElementSpec.JoinPreviousDirection);
    doc.insert(0, specs);

    // The first '\n'
    text = new char[] {'\n'};
    specs = new TestDocument.ElementSpec[3];
    specs[0] =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.ContentType,
                                   text, 0, 1);
    specs[0].setDirection(TestDocument.ElementSpec.JoinPreviousDirection);
    specs[1] =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.EndTagType);
    specs[1].setDirection(TestDocument.ElementSpec.OriginateDirection);
    specs[2] =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.StartTagType);
    specs[2].setDirection(TestDocument.ElementSpec.JoinFractureDirection);
    doc.insert(1, specs);

    // The second 'a'
    text = new char[] {'a'};
    specs = new TestDocument.ElementSpec[3];
    specs[0] =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.EndTagType);
    specs[0].setDirection(TestDocument.ElementSpec.OriginateDirection);
    specs[1] =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.StartTagType);
    specs[1].setDirection(TestDocument.ElementSpec.JoinNextDirection);
    specs[2] =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.ContentType,
                                   text, 0, 1);
    specs[2].setDirection(TestDocument.ElementSpec.OriginateDirection);
    doc.insert(2, specs);

    // The second '\n'
    text = new char[] {'\n'};
    specs = new TestDocument.ElementSpec[3];
    specs[0] =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.ContentType,
                                   text, 0, 1);
    specs[0].setDirection(TestDocument.ElementSpec.JoinPreviousDirection);
    specs[1] =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.EndTagType);
    specs[1].setDirection(TestDocument.ElementSpec.OriginateDirection);
    specs[2] =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.StartTagType);
    specs[2].setDirection(TestDocument.ElementSpec.JoinFractureDirection);
    doc.insert(3, specs);

    // We have one paragraph in the root element.
    Element root = doc.getDefaultRootElement();
    h.check(root.getElementCount(), 3);

    // We should now have 1 child in the 1st paragraph.
    Element par = root.getElement(0);
    h.check(par.getElementCount(), 1);
    Element el = par.getElement(0);
    h.check(el.getStartOffset(), 0);
    h.check(el.getEndOffset(), 2);

    // We should now have 1 child in the 2nd paragraph.
    par = root.getElement(1);
    h.check(par.getElementCount(), 1);
    el = par.getElement(0);
    h.check(el.getStartOffset(), 2);
    h.check(el.getEndOffset(), 4);

    // We should now have 1 child in the 3rd paragraph.
    par = root.getElement(2);
    h.check(par.getElementCount(), 1);
    el = par.getElement(0);
    h.check(el.getStartOffset(), 4);
    h.check(el.getEndOffset(), 5);

  }


  /**
   * Inserts 'a\na\na' and checks the results. The characters are inserted
   * one by one, as if somebody typed it in by keyboard.
   *
   * @param h the test harness to use
   */
  private void testNewlines2(TestHarness h)
  {
    h.checkPoint("testNewlines2");
    TestDocument doc = new TestDocument();
    doc.addDocumentListener(this);

    // The first 'a'
    char[] text = new char[] {'a'};
    SimpleAttributeSet atts = new SimpleAttributeSet();
    TestDocument.ElementSpec[] specs = new TestDocument.ElementSpec[1];
    specs[0] =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.ContentType,
                                   text, 0, 1);
    specs[0].setDirection(TestDocument.ElementSpec.JoinPreviousDirection);
    doc.insert(0, specs);

    // The first '\n'
    text = new char[] {'\n'};
    specs = new TestDocument.ElementSpec[3];
    specs[0] =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.ContentType,
                                   text, 0, 1);
    specs[0].setDirection(TestDocument.ElementSpec.JoinPreviousDirection);
    specs[1] =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.EndTagType);
    specs[1].setDirection(TestDocument.ElementSpec.OriginateDirection);
    specs[2] =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.StartTagType);
    specs[2].setDirection(TestDocument.ElementSpec.JoinFractureDirection);
    doc.insert(1, specs);

    // The second 'a'
    text = new char[] {'a'};
    specs = new TestDocument.ElementSpec[3];
    specs[0] =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.EndTagType);
    specs[0].setDirection(TestDocument.ElementSpec.OriginateDirection);
    specs[1] =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.StartTagType);
    specs[1].setDirection(TestDocument.ElementSpec.JoinNextDirection);
    specs[2] =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.ContentType,
                                   text, 0, 1);
    specs[2].setDirection(TestDocument.ElementSpec.OriginateDirection);
    doc.insert(2, specs);

    // The second '\n'
    text = new char[] {'\n'};
    specs = new TestDocument.ElementSpec[3];
    specs[0] =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.ContentType,
                                   text, 0, 1);
    specs[0].setDirection(TestDocument.ElementSpec.JoinPreviousDirection);
    specs[1] =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.EndTagType);
    specs[1].setDirection(TestDocument.ElementSpec.OriginateDirection);
    specs[2] =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.StartTagType);
    specs[2].setDirection(TestDocument.ElementSpec.JoinFractureDirection);
    doc.insert(3, specs);

    // The 3rd 'a'
    text = new char[] {'a'};
    specs = new TestDocument.ElementSpec[3];
    specs[0] =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.EndTagType);
    specs[0].setDirection(TestDocument.ElementSpec.OriginateDirection);
    specs[1] =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.StartTagType);
    specs[1].setDirection(TestDocument.ElementSpec.JoinNextDirection);
    specs[2] =
      new TestDocument.ElementSpec(atts, TestDocument.ElementSpec.ContentType,
                                   text, 0, 1);
    specs[2].setDirection(TestDocument.ElementSpec.OriginateDirection);
    doc.insert(4, specs);

    // We have 3 paragraph in the root element.
    Element root = doc.getDefaultRootElement();
    h.check(root.getElementCount(), 3);

    // We should now have 1 child in the 1st paragraph.
    Element par = root.getElement(0);
    h.check(par.getElementCount(), 1);
    Element el = par.getElement(0);
    h.check(el.getStartOffset(), 0);
    h.check(el.getEndOffset(), 2);

    // We should now have 1 child in the 2nd paragraph.
    par = root.getElement(1);
    h.check(par.getElementCount(), 1);
    el = par.getElement(0);
    h.check(el.getStartOffset(), 2);
    h.check(el.getEndOffset(), 4);

    // We should now have 2 children in the 3rd paragraph.
    par = root.getElement(2);
    h.check(par.getElementCount(), 2);
    el = par.getElement(0);
    h.check(el.getStartOffset(), 4);
    h.check(el.getEndOffset(), 5);
    el = par.getElement(1);
    h.check(el.getStartOffset(), 5);
    h.check(el.getEndOffset(), 6);

  }

  /**
   * Receives notification when some text attributes have changed.
   *
   * @param event the document event
   */
  public void changedUpdate(DocumentEvent event)
  {
    // Do nothing here.
  }

  /**
   * Receives notification when some text has been inserted into the document.
   *
   * @param event the document event
   */
  public void insertUpdate(DocumentEvent event)
  {
    documentEvent = event;
  }

  /**
   * Receives notification when some text has been removed from the document.
   *
   * @param event the document event
   */
  public void removeUpdate(DocumentEvent event)
  {
    // Do nothing here.
  }
}
