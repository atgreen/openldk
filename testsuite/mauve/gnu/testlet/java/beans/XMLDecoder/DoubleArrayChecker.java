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


/** Simply expects two double arrays and compares their content for equality.
 * This is used for the growableDoubleArrayTest and is needed because the
 * following comparison results to <code>false</code> in Java
 * <code>new double[] { 1, 2, 3 }.equals(new double[] { 1, 2, 3 })</code>.
 *
 * @author Robert Schuster
 */
class DoubleArrayChecker implements EqualityChecker
{
  public boolean areEqual(Object firstObject, Object secondObject)
  {
    try
      {
	if (! Class.forName("[D").isInstance(firstObject))
	  return false;
      }
    catch (ClassNotFoundException cnfe)
      {
	throw new InternalError("VM was unable to return the class representing an double array.");
      }

    double[] decodedArray = (double[]) firstObject;
    double[] expectedArray = (double[]) secondObject;

    if (decodedArray.length != expectedArray.length)
      return false;

    int size = decodedArray.length;
    for (int i = 0; i < size; i++)
      {
	if (decodedArray[i] != expectedArray[i])
	  return false;
      }

    return true;
  }
}
