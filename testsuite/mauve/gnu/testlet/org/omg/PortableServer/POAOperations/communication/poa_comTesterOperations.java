// Tags: not-a-test
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


/**
 * The tester interface.
 */
public interface poa_comTesterOperations
{
  /**
   * The field that can be either set of checked.
   */
  int theField();

  /**
   * The field that can be either set of checked.
   */
  void theField(int newTheField);

  /**
   * Handles a simple message, returns string info about calling
   * circumstances.
   */
  String sayHello();

  /**
  * Passes wide string.
  */
  String passCharacters(String wide, String narrow);

  /**
   * Throws either 'ourUserException' with the 'ourField' field
   * initialised to the passed positive value
   * or system exception (if the parameter is zero or negative).
   */
  void throwException(int parameter)
               throws ourUserException;
}
