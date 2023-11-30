// Tags: not-a-test

// Copyright (C) 2004 Sascha Brawer <brawer@dandelis.ch>

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

package gnu.testlet.java.util.logging.Logger;

import java.security.Permission;
import java.security.AccessControlException;
import java.util.logging.LoggingPermission;
import java.util.logging.LogManager;

/**
 * A SecurityManager that can be told whether or not to grant
 * LoggingPermission.
 *
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
public class TestSecurityManager
  extends SecurityManager
{
  private boolean grantLogging = false;

  private final Permission controlPermission
    = new LoggingPermission("control", null);

  private SecurityManager oldManager;
  
  public void checkPermission(Permission perm)
  {
    if (controlPermission.implies(perm) && !grantLogging)
      throw new AccessControlException("access denied: " + perm, perm);
  }


  public void setGrantLoggingControl(boolean grant)
  {
    grantLogging = grant;
  }


  public void install()
  {
    // Make sure the LogManager is fully installed first.
    LogManager lm = LogManager.getLogManager();

    SecurityManager oldsm = System.getSecurityManager();
    
    if (oldsm == this)
      throw new IllegalStateException("already installed");

    oldManager = oldsm;
    System.setSecurityManager(this);
  }


  public void uninstall()
  {
    System.setSecurityManager(oldManager);
  }
}
