// Tags: JDK1.5

// Copyright (C) 2006 Andrew John Hughes <gnu_andrew@member.fsf.org>

// This file is part of Mauve.

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

package gnu.testlet.javax.management.MBeanServerPermission;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.management.MBeanServerPermission;

/**
 * Tests creation of an
 * {@link MBeanServerPermission}.
 *
 * @author <a href="mailto:gnu_andrew@member.fsf.org">Andrew John Hughes</a>
 */
public class Constructor
  implements Testlet
{
  
  public void test(TestHarness h)
  {
    Exception caught = null;

    // Check null name throws NullPointerException
    try
      {
	new MBeanServerPermission(null);
      }
    catch (Exception ex)
      {
	caught = ex;
      }
    h.check(caught instanceof NullPointerException, "Null name");
   
    caught = null;
    // Check "*" is a valid name
    try
      {
	new MBeanServerPermission("*");
      }
    catch (Exception ex)
      {
	caught = ex;
      }
    h.check(caught, null, "* is valid");
    
    // Check valid values are allowed
    String[] valid = new String[] { "createMBeanServer", "newMBeanServer",
				    "findMBeanServer", "releaseMBeanServer" };
    // 1
    for (int a = 0; a < valid.length; ++a)
      {
	caught = null;
	try
	  {
	    new MBeanServerPermission(valid[a]);
	  }
	catch (Exception ex)
	  {
	    caught = ex;
	  }
	h.check(caught, null, valid[a] + " is valid");
      } 
    // 2
    for (int a = 0; a < valid.length; ++a)
      for (int b = 0; b < valid.length; ++b)
	{
	  caught = null;
	  String permit = valid[a] + "," + valid[b];
	  try
	    {
	      new MBeanServerPermission(permit);
	    }
	  catch (Exception ex)
	    {
	      caught = ex;
	    }
	  h.check(caught, null, permit + " is valid");
	} 
    // 3
    for (int a = 0; a < valid.length; ++a)
      for (int b = 0; b < valid.length; ++b)
	for (int c = 0; c < valid.length; ++c)
	  {
	    caught = null;
	    String permit = valid[a] + "," + valid[b] + "," + valid[c];
	    try
	      {
		new MBeanServerPermission(permit);
	      }
	    catch (Exception ex)
	      {
		caught = ex;
	      }
	    h.check(caught, null, permit + " is valid");
	  } 
    // 4
    for (int a = 0; a < valid.length; ++a)
      for (int b = 0; b < valid.length; ++b)
	for (int c = 0; c < valid.length; ++c)
	  for (int d = 0; d < valid.length; ++d)
	    {
	      caught = null;
	      String permit = valid[a] + "," + valid[b] + "," + valid[c]
		+ "," + valid[d];
	      try
		{
		  new MBeanServerPermission(permit);
		}
	      catch (Exception ex)
		{
		  caught = ex;
		}
	      h.check(caught, null, permit + " is valid");
	    }
    
    caught = null;
    // Check with spaces
    try
      {
	new MBeanServerPermission("     createMBeanServer   , newMBeanServer  ");
      }
    catch (Exception ex)
      {
	caught = ex;
      }
    h.check(caught, null, "spaces are valid");

    caught = null;
    // Check random stuff gets thrown out
    try
      {
	new MBeanServerPermission("fjafjlskjflka");
      }
    catch (Exception ex)
      {
	caught = ex;
      }
    h.check(caught instanceof IllegalArgumentException,
	    "other names are invalid");
    
    caught = null;
    // Check non-null non-empty actions are caught
    try
      {
	new MBeanServerPermission("*","fishcakes");
      }
    catch (Exception ex)
      {
	caught = ex;
      }
    h.check(caught instanceof IllegalArgumentException,
	    "non-null non-empty actions are invalid");

  }

}

