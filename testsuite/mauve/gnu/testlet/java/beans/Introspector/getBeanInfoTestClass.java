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
 * <code>gnu.testlet.java.beans.getBeanInfo</code> and
 * <code>gnu.testlet.java.beans.getBeanInfo2</code>.
 *
 * <p>See each method's documentation to see what is expected for the
 * tests.</p>
 *
 * @author Robert Schuster <theBohemian@gmx.net>
 */
public class getBeanInfoTestClass
{
  /** This method is expected to be the read method of the property
   * 'correctProperty' and should show up as in the method descriptors.
   *
   * @param i Unused.
   */
  public void setCorrectProperty(int i)
  {
  }

  /** This method is expected to be the write method of the property
   * 'correctProperty' and should show up in the method descriptors.
   *
   * @return 0
   */
  public int getCorrectProperty()
  {
    return 0;
  }

  /** This method is expected to be the read method of the read-only
   * property 'correctReadOnlyProperty'  and should show up in the
   * method descriptors.
   *
   * @return 0.
   */
  public int getCorrectReadOnlyProperty()
  {
    return 0;
  }

  /** This method is expected to be the write method of the write-only
   * property 'correctWriteOnlyProperty' and should show up in the
   * method descriptors.
   *
   * @param i Unused.
   */
  public void setCorrectWriteOnlyProperty(int i)
  {
  }

  /** This method should not show up as part of a property or method
   * descriptor because the method is not public.
   *
   * @param i Unused.
   */
  void setSomeValue(int i)
  {
  }

  /** This method should not show up as part of a property or method
   * descriptor because the method is not public.
   *
   * @return 0.
   */
  int getSomeValue()
  {
    return 0;
  }

  /** This method should not show up as part of a property because the
   * method is <code>static</code> but it should show up in the
   * method descriptors.
   *
   * @param i Unused.
   */
  public static void setSomeStaticValue(int i)
  {
  }

  /** This method should not show up as part of a property because the
   * method is <code>static</code> but it should show up in the
   * method descriptors.
   * 
   * @return 0.
   */
  public static int getSomeStaticValue()
  {
    return 0;
  }
  
}
