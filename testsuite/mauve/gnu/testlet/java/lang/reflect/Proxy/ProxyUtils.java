/* ProxyUtils.java -- Utilities class for Proxy related operations
 Copyright (C) 2006 Olivier Jolly <olivier.jolly@pcedev.com>
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


package gnu.testlet.java.lang.reflect.Proxy;

import java.lang.reflect.Method;
import java.util.Arrays;

/**
 * Utility class with some proxy related methods
 * @author Olivier Jolly <olivier.jolly@pcedev.com>
 */
public final class ProxyUtils
{

  /**
   * Compare two methods excepted the declaring class equality test
   * @param lhs
   *          first method to test
   * @param rhs
   *          second method to test
   * @return whether the two methods are equals even if of different declaring
   *         class
   */
  static boolean compareMethodExceptedDeclaringClass(Method lhs, Method rhs)
  {
    if (!lhs.getName().equals(rhs.getName()))
      {
        return false;
      }
    if (lhs.getReturnType() != rhs.getReturnType())
      {
        return false;
      }
    if (!Arrays.equals(lhs.getParameterTypes(), rhs.getParameterTypes()))
      {
        return false;
      }
    return true;
  }

  /**
   * Compare two methods based only on their name and parameter
   * @param lhs
   *          first method to test
   * @param rhs
   *          second method to test
   * @return whether the name and parameter type are equal
   */
  static boolean compareMethodOnNameAndParameterTypes(Method lhs, Method rhs)
  {
    if (!lhs.getName().equals(rhs.getName()))
      {
        return false;
      }
    if (!Arrays.equals(lhs.getParameterTypes(), rhs.getParameterTypes()))
      {
        return false;
      }
    return true;

  }

  /**
   * Return a valid value for the given class, even if a primitive
   * @param returnType
   *          the expected class
   * @return a neutral value of the expected class
   * @throws InstantiationException
   *           in case of problem with the constructor invocation
   * @throws IllegalAccessException
   *           in case of problem with the constructor invocation
   */
  public static Object getNeutralValue(Class returnType)
      throws InstantiationException, IllegalAccessException
  {
    if (returnType.equals(boolean.class))
      {
        return Boolean.FALSE;
      }
    if (returnType.equals(int.class))
      {
        return new Integer(0);
      }
    if (returnType.equals(float.class))
      {
        return new Float(0);
      }
    if (returnType.equals(double.class))
      {
        return new Double(0);
      }
    if (returnType.equals(char.class))
      {
        return new Character((char) 0);
      }
    if (returnType.equals(short.class))
      {
        return new Short((short) 0);
      }
    if (returnType.equals(long.class))
      {
        return new Long(0);
      }
    if (returnType.equals(void.class))
      {
        return null;
      }
    return returnType.newInstance();
  }

}
