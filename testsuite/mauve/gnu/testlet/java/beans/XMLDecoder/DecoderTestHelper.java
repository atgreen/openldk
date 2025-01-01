//Tags: not-a-test

//Uses: EqualityChecker

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

import gnu.testlet.ResourceNotFoundException;
import gnu.testlet.TestHarness;

import java.beans.ExceptionListener;
import java.beans.XMLDecoder;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/** Helper class for tests involving the XMLDecoder. It provides a simple environment to set up
 * a sequence of objects which is expected to be decoded and allows the registration of an
 * <code>EqualityChecker</code> instance for objects which do not provide a proper implementation
 * of <code>Object.equals(Object)</code>.
 *
 * Note that the XMLDecoder commonly relies on a SAX parser being available.
 *
 * @author Robert Schuster
 *
 */
class DecoderTestHelper
{
  private List expectedObjects = new ArrayList();
  private Map checkers = new HashMap();
  private String xmlFile;
  private String name;

  /** Creates the environment for a testcase of the XMLDecoder. The argument string must
   * be pointing to a XML file which can be loaded as a resource from the classpath. See
   * <code>TestHarness.getResourceStream()</code> for details.
   *
   * @param name A name that is displayed in case of errors
   * @param xmlResourceFilename A filename pointing to a XML file which can be loaded as a resource.
   */
  DecoderTestHelper(String name, String xmlResourceFilename)
  {
    if (name == null || name.length() == 0)
      throw new IllegalArgumentException("Please provide a name for this test.");
    
    this.name = name;

    if (xmlResourceFilename == null)
      throw new IllegalArgumentException("No filename to a XML resource file provided.");
    xmlFile = xmlResourceFilename;
  }

  /** Adds an object to the sequence of expected objects to be decoded.
   *
   * @param obj Object which is expected to be decoded.
   */
  protected final void addObject(Object obj)
  {
    expectedObjects.add(obj);
  }

  /** Adds an object to the sequence of expectedd objects to be an decoded and provides a specialized
   * <code>EqualityChecker</code> for the instance. Providing an <code>EqualityChecker</code> is useful
   * if the class of the given object does not properly support the <code>Object.equals(Object)</code>
   * method and cannot be changed.
   *
   * Note that only once <code>EqualityChecker</code> per expected object instance is allowed. If you
   * need the same object more than once in the sequence use the one-parameter form of
   * <code>addObject()</code> after the first registration only.
   *
   * @param obj Object which is expected to be decoded.
   * @param eqChecker A specialized <code>EqualityChecker</code> implementation for the given object.
   */
  protected final void addObject(Object obj, EqualityChecker eqChecker)
  {
    // prevents registering an EqualityChecker more than once because such behaviour is not supported 
    if (checkers.containsKey(obj))
      throw new IllegalStateException("Already provided an EqualityChecker instance for object '"
                                      + obj + "'.");

    checkers.put(obj, eqChecker);

    addObject(obj);
  }

  final void doComparison(final TestHarness harness)
  {
    // creates an ExceptionListener implementation that lets the test fail
    // if one occurs
    ExceptionListener exListener = new ExceptionListener()
      {
	public void exceptionThrown(Exception e)
	{
	  harness.fail(name
	               + " - decode.readObject(): unexpected exception occured during decoding: "
	               + e);
	}
      };

    XMLDecoder decoder = null;

    // tries to initialize the XMLDecoder
    try
      {
	decoder = new XMLDecoder(harness.getResourceStream(xmlFile), null,
	                         exListener);
      }
    catch (ResourceNotFoundException e)
      {
	harness.fail(name
	             + " - create XMLDecoder: unable to load resource from classpath: "
	             + xmlFile);
	return;
      }

    /* compares each object of the XMLDecoder with the one that is expected either using
     * Object.equals() or using a specialized EqualityChecker instance
     */
    Iterator ite = expectedObjects.iterator();
    while (ite.hasNext())
      {
	Object expectedObject = ite.next();

	try
	  {
	    Object decodedObject = decoder.readObject();

	    if (checkers.containsKey(expectedObject))
	      {
		EqualityChecker eq = (EqualityChecker) checkers.get(expectedObject);

		harness.check(eq.areEqual(decodedObject, expectedObject),
		              name + " - decoder.readObject()-loop");
	      }
	    else
	      harness.check(decodedObject, expectedObject,
	                    name + " - decoder.readObject()-loop");
	  }
	catch (ArrayIndexOutOfBoundsException aioobe)
	  {
	    decoder.close();

	    // that means that not enough objects where decoded
	    harness.fail(name + " - decoder.close()");

	    harness.verbose(name
	                    + "decoder.close(): no more objects provided by XMLDecoder "
	                    + "although at least one more was expected");

	    return;
	  }
      }

    /* After all expected objects have been compared there should not be any object left in the decoder.
     * Since there is no query method for this we have to invoke readObject() which should throw an
     * ArrayIndexOutOfBoundsException if there is no object left.
     */
    try
      {
	decoder.readObject();
      }
    catch (ArrayIndexOutOfBoundsException aioobe)
      {
	// this exception is expected
	decoder.close();

	return;
      }

    // there was at least one object left if we reach this code
    harness.fail(name
                 + " - readObject()-final: at least one unexpected object was left in the decoder");

    decoder.close();
  }
}
