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

import javax.management.remote.NotificationResult;
import javax.management.remote.TargetedNotification;

public class NotificationResultTest
  implements Testlet
{
  public void test(TestHarness h)
  {
    NotificationResult nr;
    TargetedNotification[] array = new TargetedNotification[]{};
    h.checkPoint("Constructor tests");
    try
      {
	nr = new NotificationResult(-1, 0, array);
	h.fail("Failed to catch negative earliest sequence number");
      }
    catch (Exception e)
      {
	if (e instanceof IllegalArgumentException)
	  h.check(true, "Caught negative earliest sequence number.");
	else
	  {
	    h.debug(e);
	    h.fail("Unknown exception");
	  }
      }
    try
      {
	nr = new NotificationResult(0, -1, array);
	h.fail("Failed to catch negative next sequence number");
      }
    catch (Exception e)
      {
	if (e instanceof IllegalArgumentException)
	  h.check(true, "Caught negative next sequence number.");
	else
	  {
	    h.debug(e);
	    h.fail("Unknown exception");
	  }
      }
    try
      {
	nr = new NotificationResult(0, 1, null);
	h.fail("Failed to catch null result array");
      }
    catch (Exception e)
      {
	if (e instanceof IllegalArgumentException)
	  h.check(true, "Caught null result array.");
	else
	  {
	    h.debug(e);
	    h.fail("Unknown exception");
	  }
      }
    try
      {
	nr = new NotificationResult(0, 1, array);
	h.check(true, "NotificationResult successfully created.");
	h.check(nr.getEarliestSequenceNumber() == 0,
		"Retrieved earliest sequence number.");
	h.check(nr.getNextSequenceNumber() == 1,
		"Retrieved next sequence number.");
 	h.check(nr.getTargetedNotifications() == array,
		"Retrieved array.");
      }
    catch (Exception e)
      {
	if (e instanceof IllegalArgumentException)
	  h.fail("Wrongly threw IllegalArgumentException.");
	else
	  {
	    h.debug(e);
	    h.fail("Unknown exception");
	  }
      }
    
  }
}

