// Tags: JDK1.0

// Copyright (C) 1998, 2001 Cygnus Solutions

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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.lang.Integer;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.util.Properties;
import java.util.PropertyPermission;
import java.security.Permission;
import java.security.SecurityPermission;

public class getInteger extends SecurityManager implements Testlet
{
  public void test (TestHarness harness)
    {
      // Augment the System properties with the following.
      // Overwriting is bad because println needs the
      // platform-dependent line.separator property.
      Properties p = System.getProperties();
      p.put("e1", Integer.toString(Integer.MIN_VALUE));
      p.put("e2", Integer.toString(Integer.MAX_VALUE));
      p.put("e3", "0" + Integer.toOctalString(Integer.MIN_VALUE));
      p.put("e4", "0" + Integer.toOctalString(Integer.MAX_VALUE));
      p.put("e5", "0x" + Integer.toHexString(Integer.MIN_VALUE));
      p.put("e6", "0x" + Integer.toHexString(Integer.MAX_VALUE));
      p.put("e7", "0" + Integer.toString(Integer.MAX_VALUE, 8));
      p.put("e8", "#" + Integer.toString(Integer.MAX_VALUE, 16));
      p.put("e9", "");
      p.put("e10", " ");
      p.put("e11", "foo");
      p.put("e12", "-#1");

      harness.check (Integer.getInteger("e1").toString(), "-2147483648");
      harness.check (Integer.getInteger("e2").toString(), "2147483647");
      harness.check (Integer.getInteger("e3"), null);
      harness.check (Integer.getInteger("e4").toString(), "2147483647");
      harness.check (Integer.getInteger("e5", 12345).toString(), "12345");
      harness.check (Integer.getInteger("e6", new Integer(56789)).toString(),
		     "2147483647");
      harness.check (Integer.getInteger("e7", null).toString(), "2147483647");
      harness.check (Integer.getInteger("e8", 12345).toString(), "2147483647");
      harness.check (Integer.getInteger("e9", new Integer(56789)).toString(),
		     "56789");
      harness.check (Integer.getInteger("e10", null), null);
      harness.check (Integer.getInteger("e11"), null);
      harness.check (Integer.getInteger("e12"), new Integer(-1));
      harness.check (Integer.getInteger("junk", 12345).toString(), "12345");
      harness.check (Integer.getInteger("junk", new Integer(56789)).toString(),
		     "56789");
      harness.check (Integer.getInteger("junk", null), null);
      harness.check (Integer.getInteger("junk"), null);
      try
        {
          harness.check (Integer.getInteger(null), null);
	}
      catch (NullPointerException npe)
        {
          harness.check (false);
        }
      harness.check (Integer.getInteger(""), null);

      boolean ok = true;
      SecurityManager old_security_manager = System.getSecurityManager();
      try 
        {
          try
            {
              System.setSecurityManager(this);
            }
          catch (Throwable e)
            {
              harness.debug(e);
              ok = false; // can't run this test
            }
          if (ok)
            {
              try
                {
                  Integer.getInteger("secure");
                  ok = false;
                }
              catch (SecurityException se)
                {
                }
            }
        }
      finally 
        {
          // undo our change
          System.setSecurityManager(old_security_manager); 
        }
      harness.check(ok);
    }

  // Method needed for SecurityManager

  /**
   * Croak on checking a property named "secure"
   */
  public void checkPropertyAccess(String s)
  {
    if ("secure".equals(s))
      throw new SecurityException("'Croak'");
    else 
      super.checkPropertyAccess(s);
  }

  /**
   * Allow restoration of the existing security manager, and various other
   * things that happen under the hood in various VMs. (HACK!)
   */
  public void checkPermission(Permission p)
  {
    if (new RuntimePermission("setSecurityManager").implies(p))
      return;
    if (new SecurityPermission("getProperty.networkaddress.*").implies(p))
      return;
    if (new PropertyPermission("sun.net.inetaddr.ttl", "read").implies(p))
      return;
    super.checkPermission(p);
  }

}
