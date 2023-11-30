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

import java.beans.beancontext.BeanContextChild;
import java.beans.beancontext.BeanContextChildSupport;
import java.beans.beancontext.BeanContextSupport;

import java.util.Arrays;

public class Array
  implements Testlet
{

  private static BeanContextSupport context = new BeanContextSupport();

  public void test(TestHarness h)
  {
    BeanContextChild[] children = new BeanContextChild[5];

    /* Create and add five children */
    for (int a = 0; a < 5; ++a)
      {
	children[a] = new BeanContextChildSupport();
	context.add(children[a]);
	h.check(context.contains(children[a]), "Child " + a + " present.");
      }
    /* Check size */
    h.check(context.size() == 5, "Size of 5 check");
    /* Compare arrays */
    Object[] addedChildren = context.toArray();
    /* Order will differ, so we must find a matching one somewhere */
    for (int a = 0; a < 5; ++a)
      {
	boolean flag = false;
	for (int b = 0; b < 5; ++b)
	  if (addedChildren[b] == children[a])
	    flag = true;
	h.check(flag, "Check for child " + a + " in array.");
      }
  }

}

