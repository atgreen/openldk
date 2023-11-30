// Copyright (C) 2005, 2006 Red Hat, Inc.
//
// This file is part of Mauve.
//
// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.
//
// Tags: JDK1.2

package gnu.testlet.java.lang.SecurityManager;

import java.security.Permission;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import gnu.testlet.TestSecurityManager;

public class thread implements Testlet
{
  public void test(TestHarness harness)
  {
    // The 1.4.2 javadoc for SecurityManager.checkAccess(Thread) says
    // that checkAccess will check RuntimePermission("modifyThread")
    // for system threads, and do nothing for other threads. It
    // defines a system thread as one belonging to a thread group
    // with a null parent.  SecurityManager.checkAccess(ThreadGroup)
    // similarly performs checks only when the group's parent is null.

    harness.checkPoint("checkAccess");

    // Get ourselves a system thread and group
    Thread thread = Thread.currentThread();

    ThreadGroup group = thread.getThreadGroup();
    harness.check(group != null);

    while (group.getParent() != null)
      group = group.getParent();
    if (thread.getThreadGroup() != group)
      thread = new Thread(group, "dummy");

    // Check we're checking
    TestSecurityManager sm = new TestSecurityManager(harness);

    try
      {
        sm.install();
        
        sm.prepareChecks(new Permission[] {
                           new RuntimePermission("modifyThread")});
        sm.checkAccess(thread);
        sm.checkAllChecked();
        
        sm.prepareChecks(new Permission[] {
                           new RuntimePermission("modifyThreadGroup")});
        sm.checkAccess(group);
        sm.checkAllChecked();
      }
    finally
      {
        sm.uninstall();
      }
  }
}
