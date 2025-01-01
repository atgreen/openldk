/* getChildBeanContextServicesListener.java -- some checks for the 
       getChildBeanContextServicesListener() method in the
       BeanContextServicesSupport class.
   Copyright (C) 2006 David Gilbert <david.gilbert@object-refinery.com>
This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/

// Tags: JDK1.2
// Uses: MyBeanContextServicesSupport

package gnu.testlet.java.beans.beancontext.BeanContextServicesSupport;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.beancontext.BeanContextServicesSupport;

public class getChildBeanContextServicesListener implements Testlet
{
  public void test(TestHarness harness)
  {
    // try null
    harness.check(MyBeanContextServicesSupport.getChildBeanContextServicesListenerX(null), null);
    
    // try an object that doesn't implement the listener interface
    harness.check(MyBeanContextServicesSupport.getChildBeanContextServicesListenerX("XYZ"), null);
    
    // try an object that does implement the interface
    BeanContextServicesSupport bcss = new BeanContextServicesSupport();
    harness.check(MyBeanContextServicesSupport.getChildBeanContextServicesListenerX(bcss), bcss);
  }
}
