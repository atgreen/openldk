/* constructors.java -- some checks for the constructors in the 
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

package gnu.testlet.java.beans.beancontext.BeanContextSupport;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.beancontext.BeanContext;
import java.beans.beancontext.BeanContextSupport;
import java.util.Locale;

public class constructors implements Testlet
{
  public void test(TestHarness harness)
  {
    testConstructor1(harness);
    testConstructor2(harness);
    testConstructor3(harness);
    testConstructor4(harness);
    testConstructor5(harness);
  }
  
  public void testConstructor1(TestHarness harness)
  {
    harness.checkPoint("()");
    BeanContextSupport bcs = new BeanContextSupport();
    harness.check(bcs.getBeanContext(), null);
    harness.check(bcs.getBeanContextPeer(), bcs);
    harness.check(bcs.beanContextChildPeer, bcs);
    harness.check(!bcs.needsGui());
    harness.check(!bcs.isDesignTime());
    harness.check(!bcs.avoidingGui());
    harness.check(bcs.getLocale(), Locale.getDefault());
    harness.check(bcs.size(), 0);
  }
  
  public void testConstructor2(TestHarness harness)
  {
    harness.checkPoint("(BeanContext)");
    BeanContext bc = new BeanContextSupport();
    BeanContextSupport bcs = new BeanContextSupport(bc);
    harness.check(bcs.getBeanContext(), null);
    harness.check(bcs.getBeanContextPeer(), bc);
    harness.check(bcs.beanContextChildPeer, bc);
    harness.check(!bcs.needsGui());
    harness.check(!bcs.isDesignTime());
    harness.check(!bcs.avoidingGui());
    harness.check(bcs.getLocale(), Locale.getDefault());
    harness.check(bcs.size(), 0);
    
    // try null
    bcs = new BeanContextSupport(null);
    harness.check(bcs.getBeanContext(), null);
    harness.check(bcs.getBeanContextPeer(), bcs);
    harness.check(bcs.beanContextChildPeer, bcs);
    harness.check(!bcs.needsGui());
    harness.check(!bcs.isDesignTime());
    harness.check(!bcs.avoidingGui());
    harness.check(bcs.getLocale(), Locale.getDefault());
    harness.check(bcs.size(), 0);
  }
  
  public void testConstructor3(TestHarness harness)
  {
    harness.checkPoint("(BeanContext, Locale)");
    BeanContext bc = new BeanContextSupport();
    BeanContextSupport bcs = new BeanContextSupport(bc, Locale.FRANCE);
    harness.check(bcs.getBeanContext(), null);
    harness.check(bcs.getBeanContextPeer(), bc);
    harness.check(bcs.beanContextChildPeer, bc);
    harness.check(!bcs.needsGui());
    harness.check(!bcs.isDesignTime());
    harness.check(!bcs.avoidingGui());
    harness.check(bcs.getLocale(), Locale.FRANCE);
    harness.check(bcs.size(), 0);
    
    // try null
    bcs = new BeanContextSupport(null, null);
    harness.check(bcs.getBeanContext(), null);
    harness.check(bcs.getBeanContextPeer(), bcs);
    harness.check(!bcs.needsGui());
    harness.check(!bcs.isDesignTime());
    harness.check(!bcs.avoidingGui());
    harness.check(bcs.getLocale(), Locale.getDefault());
    harness.check(bcs.size(), 0);
  }
  
  public void testConstructor4(TestHarness harness)
  {
    harness.checkPoint("(BeanContext, Locale, boolean)");
    BeanContext bc = new BeanContextSupport();
    BeanContextSupport bcs = new BeanContextSupport(bc, Locale.FRANCE, true);
    harness.check(bcs.getBeanContext(), null);
    harness.check(bcs.getBeanContextPeer(), bc);
    harness.check(bcs.beanContextChildPeer, bc);
    harness.check(!bcs.needsGui());
    harness.check(bcs.isDesignTime());
    harness.check(!bcs.avoidingGui());
    harness.check(bcs.getLocale(), Locale.FRANCE);
      
    // try null
    bcs = new BeanContextSupport(null, null, true);
    harness.check(bcs.getBeanContext(), null);
    harness.check(bcs.getBeanContextPeer(), bcs);
    harness.check(bcs.beanContextChildPeer, bcs);
    harness.check(!bcs.needsGui());
    harness.check(bcs.isDesignTime());
    harness.check(!bcs.avoidingGui());
    harness.check(bcs.getLocale(), Locale.getDefault());
    harness.check(bcs.size(), 0);
  }
  
  public void testConstructor5(TestHarness harness)
  {
    harness.checkPoint("()");
    harness.checkPoint("(BeanContext, Locale, boolean, boolean)");
    BeanContext bc = new BeanContextSupport();
    BeanContextSupport bcs = new BeanContextSupport(bc, Locale.FRANCE, true, true);
    harness.check(bcs.getBeanContext(), null);
    harness.check(bcs.getBeanContextPeer(), bc);
    harness.check(bcs.beanContextChildPeer, bc);
    harness.check(!bcs.needsGui());
    harness.check(bcs.isDesignTime());
    harness.check(!bcs.avoidingGui());
    harness.check(bcs.getLocale(), Locale.FRANCE);
      
    // try null
    bcs = new BeanContextSupport(null, null, true, true);
    harness.check(bcs.getBeanContext(), null);
    harness.check(bcs.getBeanContextPeer(), bcs);
    harness.check(bcs.beanContextChildPeer, bcs);
    harness.check(!bcs.needsGui());
    harness.check(bcs.isDesignTime());
    harness.check(!bcs.avoidingGui());
    harness.check(bcs.getLocale(), Locale.getDefault());
    harness.check(bcs.size(), 0);
  }
}
