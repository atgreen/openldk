// Tags: JDK1.4
// Uses: DecoderTestHelper EqualityChecker IntArrayChecker PointArrayChecker DoubleArrayChecker

//Copyright (C) 2004 Robert Schuster <theBohemian@gmx.net>

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
//the Free Software Foundation, 59 Temple Place - Suite 330,
//Boston, MA 02111-1307, USA.


package gnu.testlet.java.beans.XMLDecoder;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Point;
import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

/** Tests the main decoding functionality of java.beans.XMLDecoder.
 *
 * @author Robert Schuster
 *
 */
public class jdk14 implements Testlet
{
  private List testHelpers = new LinkedList();

  public jdk14()
  {
    /* The following lines repeatedly create a DecoderTestHelper instance, initialize it with
     * a specific object sequence and add it to the list of known DecoderTestHelpers. This
     * sets up all the testcases for the XMLDecoder.
     */
    DecoderTestHelper testHelper;

    testHelper = new DecoderTestHelper("booleanTest",
                                       "gnu#testlet#java#beans#XMLDecoder#data#boolean.xml");

    // keep the following synchronized with the XML file!
    testHelper.addObject(new Boolean(true));
    testHelper.addObject(new Boolean(false));
    addHelper(testHelper);

    // Tests decoding of characteristic byte values.
    testHelper = new DecoderTestHelper("byteTest",
                                       "gnu#testlet#java#beans#XMLDecoder#data#byte.xml");

    // keep the following synchronized with the XML file!
    testHelper.addObject(new Byte((byte) -120));

    testHelper.addObject(new Byte((byte) 0));

    testHelper.addObject(new Byte(Byte.MIN_VALUE));

    testHelper.addObject(new Byte(Byte.MAX_VALUE));
    addHelper(testHelper);

    // Tests decoding of characteristic short values.
    testHelper = new DecoderTestHelper("shortTest",
                                       "gnu#testlet#java#beans#XMLDecoder#data#short.xml");

    // keep the following synchronized with the XML file!
    testHelper.addObject(new Short((short) 16000));

    testHelper.addObject(new Short((short) -16000));

    testHelper.addObject(new Short(Short.MIN_VALUE));

    testHelper.addObject(new Short(Short.MAX_VALUE));
    addHelper(testHelper);

    // Tests decoding of characteristic int values.
    testHelper = new DecoderTestHelper("intTest",
                                       "gnu#testlet#java#beans#XMLDecoder#data#int.xml");

    // keep the following synchronized with the XML file!
    testHelper.addObject(new Integer(54321));

    testHelper.addObject(new Integer(-54321));

    testHelper.addObject(new Integer(Integer.MIN_VALUE));

    testHelper.addObject(new Integer(Integer.MAX_VALUE));
    addHelper(testHelper);

    // Tests decoding of characteristic long values.
    testHelper = new DecoderTestHelper("longTest",
                                       "gnu#testlet#java#beans#XMLDecoder#data#long.xml");

    // keep the following synchronized with the XML file!
    testHelper.addObject(new Long(5432154321L));

    testHelper.addObject(new Long(-5432154321L));

    testHelper.addObject(new Long(Long.MIN_VALUE));

    testHelper.addObject(new Long(Long.MAX_VALUE));
    addHelper(testHelper);

    // Tests decoding of characteristic float values.
    testHelper = new DecoderTestHelper("floatTest",
                                       "gnu#testlet#java#beans#XMLDecoder#data#float.xml");

    // keep the following synchronized with the XML file!
    testHelper.addObject(new Float((float) Math.PI));

    testHelper.addObject(new Float((float) Math.E));

    testHelper.addObject(new Float((float) Math.pow(2, -16)));

    testHelper.addObject(new Float(0f));

    testHelper.addObject(new Float(Float.NaN));

    testHelper.addObject(new Float(Float.POSITIVE_INFINITY));

    testHelper.addObject(new Float(Float.NEGATIVE_INFINITY));
    addHelper(testHelper);

    // Tests decoding of characteristic double values.
    testHelper = new DecoderTestHelper("doubleTest",
                                       "gnu#testlet#java#beans#XMLDecoder#data#double.xml");

    // keep the following synchronized with the XML file!
    testHelper.addObject(new Double(Math.PI));

    testHelper.addObject(new Double(Math.E));

    testHelper.addObject(new Double(Math.pow(2, -16)));

    testHelper.addObject(new Double(Double.MIN_VALUE));

    testHelper.addObject(new Double(Double.MAX_VALUE));

    testHelper.addObject(new Double(Double.NaN));

    testHelper.addObject(new Double(Double.POSITIVE_INFINITY));

    testHelper.addObject(new Double(Double.NEGATIVE_INFINITY));
    addHelper(testHelper);

    // Tests decoding of elements that cannot contain sub elements or have IDs.
    testHelper = new DecoderTestHelper("simpleElementsTest",
                                       "gnu#testlet#java#beans#XMLDecoder#data#simpleElements.xml");

    // keep the following synchronized with the XML file!
    testHelper.addObject(new Boolean(true));
    testHelper.addObject(new Boolean(false));

    testHelper.addObject(new Character('j'));
    testHelper.addObject(new Character('a'));
    testHelper.addObject(new Character('v'));
    testHelper.addObject(new Character('a'));

    testHelper.addObject(new Byte((byte) 8));

    testHelper.addObject(new Short((short) 16));

    testHelper.addObject(new Integer(32));

    testHelper.addObject(new Long(64));

    testHelper.addObject(new Float(32.0f));

    testHelper.addObject(new Double(64.0));

    testHelper.addObject("Hello World");

    testHelper.addObject(Object.class);

    testHelper.addObject(null);
    addHelper(testHelper);

    // Tests decoding of two lists and their comparison result which is
    // computed while decoding.
    testHelper = new DecoderTestHelper("listTest",
                                       "gnu#testlet#java#beans#XMLDecoder#data#list.xml");

    // keep the following synchronized with the XML file!
    List expectedList = new ArrayList();
    expectedList.add(new Integer(100));
    expectedList.add("Hello World");
    expectedList.add(WeakReference.class);

    // XML data contains an ArrayList and a LinkedList with the same content
    testHelper.addObject(expectedList);

    testHelper.addObject(expectedList);

    // comparison result of the two lists it contains
    testHelper.addObject(new Boolean(true));
    addHelper(testHelper);

    // Tests decoding a fixed-size int array.
    testHelper = new DecoderTestHelper("intArray",
                                       "gnu#testlet#java#beans#XMLDecoder#data#intArray.xml");

    // keep the following synchronized with the XML file!
    testHelper.addObject(new int[] { 1, 2, 3 }, new IntArrayChecker());

    // Tests decoding a growable int array (the growing is performed while decoding).
    testHelper = new DecoderTestHelper("growableIntArray",
                                       "gnu#testlet#java#beans#XMLDecoder#data#growableIntArray.xml");

    // keep the following synchronized with the XML file!
    testHelper.addObject(new int[] { 0, 1, 2, 3, 4, 5 }, new IntArrayChecker());
    addHelper(testHelper);

    /* Tests decoding a growable int array that has non-int subelements (that is byte and
     * short).
     */
    testHelper = new DecoderTestHelper("growableIntArray2",
                                       "gnu#testlet#java#beans#XMLDecoder#data#growableIntArray2.xml");
    
    // keep the following synchronized with the XML file!
    testHelper.addObject(new int[] { 0, 1, 2 }, new IntArrayChecker());
    addHelper(testHelper);

    // Tests decoding a growable int array (the growing is performed while decoding).
    testHelper = new DecoderTestHelper("growableDoubleArray",
                                       "gnu#testlet#java#beans#XMLDecoder#data#growableDoubleArray.xml");

    // keep the following synchronized with the XML file!
    testHelper.addObject(new double[] { 0, 1, 2, 3, 4, 5 }, new DoubleArrayChecker());
    addHelper(testHelper);

    // Tests decoding a fixed-size java.awt.Point array.
    testHelper = new DecoderTestHelper("pointArrayTest",
                                       "gnu#testlet#java#beans#XMLDecoder#data#pointArray.xml");

    // keep the following synchronized with the XML file!
    testHelper.addObject(new Point[]
                         {
                           new Point(0, 0), new Point(1, 1), new Point(2, 2)
                         }, new PointArrayChecker());
    addHelper(testHelper);

    // Tests decoding a growable java.awt.Point array (the growing is
    // performed while decoding).
    testHelper = new DecoderTestHelper("growablePointArrayTest",
                                       "gnu#testlet#java#beans#XMLDecoder#data#growablePointArray.xml");

    // keep the following synchronized with the XML file!
    testHelper.addObject(new Point[]
                         {
                           new Point(0, 0), new Point(1, 1), new Point(2, 2),
                           new Point(3, 3), new Point(4, 4), new Point(5, 5)
                         }, new PointArrayChecker());
    addHelper(testHelper);
  }

  private void addHelper(DecoderTestHelper helper)
  {
    testHelpers.add(helper);
  }

  public void test(TestHarness harness)
  {
    // simply iterates over all DecoderTestHelper instances
    // and does their comparison
    Iterator ite = testHelpers.iterator();

    while (ite.hasNext())
      ((DecoderTestHelper) ite.next()).doComparison(harness);
  }
}
