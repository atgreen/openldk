/* SimplePSKKeyManager.java -- simple PSK key manager for testing.
   Copyright (C) 2006  Casey Marshall <csm@gnu.org>

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

// Tags: not-a-test

package gnu.testlet.javax.net.ssl.SSLEngine;

import gnu.javax.net.ssl.PreSharedKeyManager;

import java.security.KeyManagementException;

import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;

/**
 * @author Casey Marshall (csm@gnu.org)
 */
public class SimplePSKKeyManager
  implements PreSharedKeyManager
{

  /* (non-Javadoc)
   * @see gnu.javax.net.ssl.PreSharedKeyManager#chooseIdentityHint()
   */
  public String chooseIdentityHint()
  {
    return "MAUVE";
  }

  /* (non-Javadoc)
   * @see gnu.javax.net.ssl.PreSharedKeyManager#getKey(java.lang.String)
   */
  public SecretKey getKey(String arg0) throws KeyManagementException
  {
    if (arg0.equals("MAUVE"))
      return KEY;
    return null;
  }

  static final SecretKey KEY
    = new SecretKeySpec("Mauve TLS PSK test key".getBytes(), "PSK");
}
