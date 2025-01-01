//Tags: not-a-test

//Copyright (C) 2004 Robert Schuster <thebohemian@gmx.net>

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
//Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.beans.Beans;

/** If this bean class is instantiated with
 * <code>java.beans.Beans.instantiate</code> the operation
 * will fail with a <code>RuntimeException</code> wrapped in a
 * <code>ClassNotFoundException</code>.
 * <p>This is true for >=1.5 only.</p>
 * 
 * @author Robert Schuster
 */
public class TestBean2
{
	
	public TestBean2() {
		/* Throwing a RuntimeException will let every
		 * reflective instantantiation fail with an
		 * InstantiationException.
		 */
		throw new RuntimeException();
	}


}
