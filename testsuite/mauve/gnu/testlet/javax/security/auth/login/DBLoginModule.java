/* DBLoginModule.java -- Fake LoginModule for test purposes
   Copyright (C) 2006 Free Software Foundation, Inc.
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

package gnu.testlet.javax.security.auth.login;

import java.util.Map;

import javax.security.auth.Subject;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.login.LoginException;
import javax.security.auth.spi.LoginModule;

/**
 * Fake {@link LoginModule} implementation for test purposes.
 */
public class DBLoginModule implements LoginModule
{
  public boolean abort() throws LoginException
  {
    return true;
  }

  public boolean commit() throws LoginException
  {
    return true;
  }

  public void initialize(Subject subject, CallbackHandler handler,
                         Map sharedState, Map options)
  {
  }

  public boolean login() throws LoginException
  {
    return true;
  }

  public boolean logout() throws LoginException
  {
    return true;
  }
}
