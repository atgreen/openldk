// Tags: JDK1.1

//Copyright (C) 2003 Stephen Crawley <crawley@dstc.edu.au>
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

package gnu.testlet.java.beans.EventSetDescriptor;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.beans.EventSetDescriptor;
import java.beans.IntrospectionException;

/** This tests the <code>EventSetDescriptor</code> using classes
 * of the Java API.
 * 
 * @author Stephen Crawley <crawley@dstc.edu.au>
 * @author Robert Schuster <thebohemian@gmx.net>
 */
public class constructorTest1 implements Testlet
{
    public void test(TestHarness harness)
    {
        boolean ok;
 
        ok = true;
        try
        {
            new EventSetDescriptor(
                java.awt.Button.class,
                "action",
                java.awt.event.ActionListener.class,
                "actionPerformed");
        }
        catch (IntrospectionException e)
        {
            harness.debug(e);
            ok = false;
        }
        harness.check(ok);

    }
    
}
