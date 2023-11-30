// Tags: JDK1.2

/*
   Copyright (C) 2006 Andrew John Hughes (gnu_andrew@member.fsf.org)

   This file is part of Mauve.

   Mauve is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Mauve is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Mauve; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.
*/

package gnu.testlet.java.beans.beancontext;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.beans.PropertyVetoException;

import java.beans.beancontext.BeanContext;
import java.beans.beancontext.BeanContextChild;
import java.beans.beancontext.BeanContextChildSupport;
import java.beans.beancontext.BeanContextSupport;

public class Remove
  implements Testlet
{

  private static TestBeanContextSupport context = new TestBeanContextSupport();
  private static BeanContextChild child = new BeanContextChildSupport();
  private static BeanContextChild brokenChild = new BeanContextChildSupport()
    {
      public void setBeanContext(BeanContext c)
	throws PropertyVetoException
      {
	if (c == null)
	  throw new PropertyVetoException("I don't like you.", null);
      }
    };

  public void test(TestHarness h)
  {
    /* Setup */
    h.check(context.add(child), "Child addition check");
    /* Check child is added to context */
    h.check(context.isEmpty() == false, "Non-empty check");
    h.check(context.size() == 1, "Size of 1 check");
    /* Check child now knows about context */
    h.check(child.getBeanContext() == context, "Correct context check");
    /* Check that context catches null */
    try
      {
	context.remove(null);
	h.fail("Failed to catch null child");
      }
    catch (Exception e)
      {
	h.check(e instanceof IllegalArgumentException, "Caught null child");
      }
    /* Check handling of non-existant child */
    h.check(context.remove(brokenChild) == false, "Remove non-existant child check");
    h.check(context.add(brokenChild), "Add broken child check");
    /* Check correct handling of veto from child */
    try
      {
	context.remove(brokenChild);
	h.fail("Failed to catch veto by child");
      }
    catch (Exception e)
      {
	h.check(e instanceof IllegalStateException, "Caught veto from child");
	/* Check that child has not been removed */
	h.check(context.size() == 2, "Same size after veto check");
      }	
    /* Check correct handling of broken child with no veto */
    context.removeTest(h);
    /* Check the child was removed */
    h.check(context.size() == 1, "Size of 1 check");
    /* Check remove first child */
    h.check(context.remove(child), "Remove child check");
    /* Add child back */
    h.check(context.add(child), "Child re-addition check");
    /* Change child context from elsewhere */
    try
      {
	child.setBeanContext(null);
      }
    catch (PropertyVetoException e)
      {
	h.debug(e);
      }
    h.check(context.size() == 0, "Empty context check");
  }

  private static class TestBeanContextSupport
    extends BeanContextSupport
  {
    public void removeTest(TestHarness h)
    {
      /* Check correct handling of broken child with no veto */
      try
	{
	  h.check(remove(brokenChild, false),
		  "Correctly avoided notification of child");
	}
      catch (Exception e)
	{
	  if (e instanceof IllegalStateException)
	    h.fail("Wrongly threw veto when not asked to notify child");
	  else
	    h.debug(e);
	}	
    }
  }

}

