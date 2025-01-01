//Tags: not-a-test

//Copyright (C) 2004  Robert Schuster <theBohemian@gmx.net>

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

package gnu.testlet.java.beans.Introspector;

/** This class is used to test the Introspector in the test:
 * <code>gnu.testlet.java.beans.getBeanInfo2_2</code>.
 *
 * <p>See each method's documentation to see what is expected for the
 * test.</p>
 *
 * @author Robert Schuster <theBohemian@gmx.net>
 */
public class getBeanInfo2_2TestClass
{

	/** This method has a setter like method name but its signature
	 * does not conform the Bean pattern. This method is expected
	 * to be available in a <code>MethodDescriptor</code> retrieved
	 * from a <code>BeanInfo</code> instance of this class.
	 * 
	 * @param arg0 Unused.
	 * @param arg1 Unused.
	 * @param arg2 Unused.
	 */
	public void setSomething1(int arg0, int arg1, int arg2) {
	}

	/** This method has a getter like method name but its signature
	 * is not conforming to the Bean pattern. However, this method
	 * is expected to be available in a <code>MethodDescriptor</code>
	 * retrieved from a <code>BeanInfo</code> instance of this class.<br/>
	 * <br/>
	 * This mimics a "get" style getter. 
	 * 
	 * @param foo Unused.
	 * @param baz Unused.
	 * @param bar Unused.
	 * @return <code>null</code>.
	 */
	public String[] getSomething2(String foo, String baz, String bar) {
		return null;
	}

	/** This method has a getter like method name but its signature
	 * is not conforming to the Bean pattern. However, this method
	 * is expected to be available in a <code>MethodDescriptor</code>
	 * retrieved from a <code>BeanInfo</code> instance of this class.<br/>
	 * <br/>
	 * This mimics an "is" style getter. The return value is explicitly chosen
	 * because "is" getters should return a <code>boolean</code>. 
	 * 
	 * @param foo Unused.
	 * @param baz Unused.
	 * @param bar Unused.
	 * @return <code>false</code>.
	 */
	public boolean isSomething3(String foo, String baz, String bar) {
		return false;
	}
	   
}
