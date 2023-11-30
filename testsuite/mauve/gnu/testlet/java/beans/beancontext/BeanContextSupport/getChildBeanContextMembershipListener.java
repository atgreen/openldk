/* getChildBeanContextMembershipListener.java -- some checks for the 
       getChildBeanContextMembershipListener() method in the 
       BeanContextSupport class.
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
// Uses: MyBeanContextSupport

package gnu.testlet.java.beans.beancontext.BeanContextSupport;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.beancontext.BeanContextChild;
import java.beans.beancontext.BeanContextMembershipEvent;
import java.beans.beancontext.BeanContextMembershipListener;
import java.beans.beancontext.BeanContextProxy;

public class getChildBeanContextMembershipListener implements Testlet
{
  static class MyBeanContextProxy implements BeanContextProxy, BeanContextMembershipListener
  {
    public void childrenAdded(BeanContextMembershipEvent bcme) 
    {
      // ignore   
    }
    public void childrenRemoved(BeanContextMembershipEvent bcme) {
      // ignore   
    }

    BeanContextChild bcs;
    public MyBeanContextProxy(BeanContextChild bcs)
    {
      this.bcs = bcs; 
    }
    public BeanContextChild getBeanContextProxy() 
    {
      return bcs;
    }
  }
  
  public void test(TestHarness harness)
  {
    // try a regular BeanContextSupport
    MyBeanContextSupport bcs1 = new MyBeanContextSupport();
    harness.check(MyBeanContextSupport.getChildBeanContextMembershipListenerX(bcs1), null);
    
    // try a proxy
    MyBeanContextSupport bcs2 = new MyBeanContextSupport();
    MyBeanContextProxy proxy = new MyBeanContextProxy(bcs2);
    harness.check(MyBeanContextSupport.getChildBeanContextMembershipListenerX(proxy), proxy);
    
    // try null
    harness.check(MyBeanContextSupport.getChildBeanContextMembershipListenerX(null), null);
  }
}
