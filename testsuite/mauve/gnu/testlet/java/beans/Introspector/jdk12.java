// Tags: JDK1.2

//Copyright (C) 2002  C. Brian Jones <cbj@gnu.org>
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
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.beans.*;

/** This is a simple test that checks that the constants
 * <code>Introspector.USE_ALL_BEANINFO</code>
 * <code>Introspector.IGNORE_IMMEDIATE_BEANINFO</code>
 * <code>Introspector.IGNORE_ALL_BEANINFO</code>
 * are defined correctly.
 *  
 * @author C. Brian Jones <cbj@gnu.org>
 * @author Robert Schuster <theBohemian@gmx.net>
 */
public class jdk12 implements Testlet
{
    public void test(TestHarness harness)
    {
        harness.checkPoint("USE_ALL_BEANINFO");
        harness.debug(
            "USE_ALL_BEANINFO value: " + Introspector.USE_ALL_BEANINFO);
        harness.check(Introspector.USE_ALL_BEANINFO, 1);

        harness.checkPoint("IGNORE_IMMEDIATE_BEANINFO");
        harness.debug(
            "IGNORE_IMMEDIATE_BEANINFO value: "
                + Introspector.IGNORE_IMMEDIATE_BEANINFO);
        harness.check(Introspector.IGNORE_IMMEDIATE_BEANINFO, 2);

        harness.checkPoint("IGNORE_ALL_BEANINFO");
        harness.debug(
            "IGNORE_ALL_BEANINFO value: " + Introspector.IGNORE_ALL_BEANINFO);
        harness.check(Introspector.IGNORE_ALL_BEANINFO, 3);
    }
    
}
