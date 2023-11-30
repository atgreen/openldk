// Tags: not-a-test

// Copyright (C) 2004 Robert Schuster <theBohemian@gmx.net>

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


package gnu.testlet.java.beans.XMLDecoder;

import java.awt.Point;

/** Simply expects two java.awt.Point arrays and compares their content
 * for equality.
 * This is used for the pointArrayTest and the growablePointArrayTest.
 * See <code>IntArrayChecker</code> for a reason why this
 * <code>EqualityChecker</code> implementation is neccessary.
 *
 * Note: This implementation assumes that the array contains <b>only</b>
 * non-null values.
 *
 * @author Robert Schuster
 */
class PointArrayChecker implements EqualityChecker
{
  public boolean areEqual(Object firstObject, Object secondObject)
  {
    try
      {
	// assure that the decoded object is really a java.awt.Point array
	// using reflection alchemy :) 
	if (! Class.forName("[Ljava.awt.Point;").isInstance(firstObject))
	  return false;
      }
    catch (ClassNotFoundException cnfe)
      {
	throw new InternalError("VM was unable to return the class representing an java.awt.Point array.");
      }

    Point[] decodedArray = (Point[]) firstObject;
    Point[] expectedArray = (Point[]) secondObject;

    if (decodedArray.length != expectedArray.length)
      return false;

    int size = decodedArray.length;
    for (int i = 0; i < size; i++)
      {
    	
	if (!decodedArray[i].equals(expectedArray[i]))
	  return false;
      }

    return true;
  }
}
