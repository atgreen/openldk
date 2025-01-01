// Copyright (C) 2008 Andrew John Hughes <gnu_andrew@member.fsf.org>

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

// Tags: JDK1.5

package gnu.testlet.javax.management.remote;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import javax.management.Notification;

import javax.management.remote.TargetedNotification;

public class TargetedNotificationTest
  implements Testlet
{
  public void test(TestHarness h)
  {
    TargetedNotification tn;
    Notification n = new Notification("", this, 1);
    h.checkPoint("Constructor tests");
    try
      {
	tn = new TargetedNotification(null, 3);
	h.fail("Failed to catch null notification");
      }
    catch (Exception e)
      {
	if (e instanceof IllegalArgumentException)
	  h.check(true, "Caught null notification.");
	else
	  {
	    h.debug(e);
	    h.fail("Unknown exception");
	  }
      }
    try
      {
	tn = new TargetedNotification(n, null);
	h.fail("Failed to catch null identifier");
      }
    catch (Exception e)
      {
	if (e instanceof IllegalArgumentException)
	  h.check(true, "Caught null identifier.");
	else
	  {
	    h.debug(e);
	    h.fail("Unknown exception");
	  }
      }
    try
      {
	tn = new TargetedNotification(n, 3);
	h.check(true, "Successfully created notification");
	h.check(n == tn.getNotification(), "Check notification retrieval");
	h.check(3 == tn.getListenerID(), "Check ID retrieval");
      }
    catch (Exception e)
      {
	if (e instanceof IllegalArgumentException)
	  {
	    h.debug(e);
	    h.check(false, "Wrongly threw IllegalArgumentException.");
	  }
	else
	  {
	    h.debug(e);
	    h.fail("Unknown exception");
	  }
      }
    
  }
}

