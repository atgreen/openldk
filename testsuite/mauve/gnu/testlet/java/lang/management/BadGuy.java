// Tags: not-a-test

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

package gnu.testlet.java.lang.management;

import java.lang.management.ManagementPermission;

import java.security.Permission;
import java.security.AccessControlException;

import java.util.PropertyPermission;

/**
 * An evil {@link java.lang.SecurityManager} which disallows use
 * of the management beans.
 *
 * @author <a href="mailto:gnu_andrew@member.fsf.org">Andrew John Hughes</a>
 */
public class BadGuy
  extends SecurityManager
{

  private final Permission propertyPermission = new PropertyPermission("*", "read,write"); 
  private final Permission monitorPermission = new ManagementPermission("monitor");
  private final Permission controlPermission = new ManagementPermission("control");

  private SecurityManager oldManager;

  /**
   * Checks permissions and disallows property access or management
   * control and monitoring.
   *
   * @param p the request permission.
   * @throws AccessControlException if property access, management control,
   *                                or monitoring is requested.
   */
  public void checkPermission(Permission p)
  {
    if (propertyPermission.implies(p))
      throw new AccessControlException("Property access disallowed.", p);
    if (controlPermission.implies(p))
      throw new AccessControlException("Management control disallowed.", p);
    if (monitorPermission.implies(p))
      throw new AccessControlException("Monitoring disallowed.", p);
  }

  /**
   * Installs this security manager in place of the current one.
   */
  public void install()
  {
    SecurityManager oldsm = System.getSecurityManager();
    
    if (oldsm == this)
      throw new IllegalStateException("already installed");

    oldManager = oldsm;
    System.setSecurityManager(this);
  }

  /**
   * Reinstates the original security manager.
   */
  public void uninstall()
  {
    System.setSecurityManager(oldManager);
  }

}
