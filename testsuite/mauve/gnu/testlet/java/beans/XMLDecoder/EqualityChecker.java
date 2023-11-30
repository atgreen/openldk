//Tags: not-a-test

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


/** Simple interface which can be implemented to provide the functionality
 * of <code>Object.equals(Object)</code> for classes which cannot be changed
 * and do not provide this feature.
 *
 * @author Robert Schuster
 *
 */
interface EqualityChecker
{
  /** Returns <code>true</code> if the two given objects are semantically
   * equal.
   *
   * @param firstObject The first object used in the check.
   * @param secondObject The second object used in the check.
   * @return Whether the first and second object are semantically equal.
   */
  boolean areEqual(Object firstObject, Object secondObject);
}
