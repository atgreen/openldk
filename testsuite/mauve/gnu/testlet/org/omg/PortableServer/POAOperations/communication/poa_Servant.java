// Tags: not-a-test
// Uses: poa_comTesterPOA

// Copyright (C) 2005 Audrius Meskauskas (AudriusA@Bioinformatics.org)

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

package gnu.testlet.org.omg.PortableServer.POAOperations.communication;

import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.CompletionStatus;

/**
 * This class handles the actual server functionality in this test
 * application. When the client calls the remote method, this
 * finally results calling the method of this class.
 *
 * The parameters, passed to the server only, are just parameters of the
 * java methods. The parameters that shuld be returned to client
 * are wrapped into holder classes.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class poa_Servant
  extends poa_comTesterPOA
{
  /**
   * The field, that can be set and checked by remote client.
   */
  private int m_theField = 17;

  /**
   * Passes wide (UTF-16) string and narrow (ISO8859_1) string.
   * @see gnu.CORBA.GIOP.CharSets_OSF for supported and default
   * encodings. Returs they generalization as a wide string.
   */
  public String passCharacters(String wide, String narrow)
  {
    return "return '" + narrow + "' and '" + wide + "'";
  }

  /**
   * Just prints the hello message.
   */
  public String sayHello()
  {
    byte[] id = _get_delegate().object_id(this);
    StringBuffer b = new StringBuffer();
    for (int i = 0; i < id.length; i++)
      {
        b.append(Integer.toHexString(id [ i ]));
        b.append(' ');
      }
    return b + ":" + hashCode();
  }

  /**
   * Get the value of our field.
   */
  public int theField()
  {
    return m_theField;
  }

  /**
   * Set the value of our field.
   */
  public void theField(int a_field)
  {
    m_theField = a_field;
  }

  /**
   * Throw an exception.
   *
   * @param parameter specifies which exception will be thrown.
   *
   * @throws ourUserException for the non negative parameter.
   * @throws BAD_OPERATION for the negative parameter.
   */
  public void throwException(int parameter)
                      throws ourUserException
  {
    if (parameter > 0)
      {
        throw new ourUserException(parameter);
      }
    else
      {
        throw new BAD_OPERATION(456, CompletionStatus.COMPLETED_YES);
      }
  }
}