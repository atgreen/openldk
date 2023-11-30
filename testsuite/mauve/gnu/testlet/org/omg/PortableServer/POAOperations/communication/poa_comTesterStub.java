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

import org.omg.CORBA.TRANSIENT;
import org.omg.CORBA.portable.ObjectImpl;

/**
 * The tester interface.
 */
public class poa_comTesterStub
  extends org.omg.CORBA.portable.ObjectImpl
  implements poa_comTester
{
  /**
   * The field that can be either set of checked.
   */
  public int theField()
  {
    org.omg.CORBA.portable.InputStream in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream out =
          _request("_get_theField", true);
        in = _invoke(out);

        int result = in.read_long();
        return result;
      }
    catch (org.omg.CORBA.portable.ApplicationException ex)
      {
        in = ex.getInputStream();

        String _id = ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException remarsh)
      {
        return theField();
      }
    finally
      {
        _releaseReply(in);
      }
  }

  /**
   * The field that can be either set of checked.
   */
  public void theField(int newTheField)
  {
    org.omg.CORBA.portable.InputStream in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream out =
          _request("_set_theField", true);
        out.write_long(newTheField);
        in = _invoke(out);
        return;
      }
    catch (org.omg.CORBA.portable.ApplicationException ex)
      {
        in = ex.getInputStream();

        String _id = ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException remarsh)
      {
        theField(newTheField);
      }
    finally
      {
        _releaseReply(in);
      }
  }

  /**
   * Handles a simple message.
   */
  public String sayHello()
  {
    org.omg.CORBA.portable.InputStream in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream out = _request("sayHello", true);
        in = _invoke(out);
        return in.read_string();
      }
    catch (org.omg.CORBA.portable.ApplicationException ex)
      {
        in = ex.getInputStream();

        String _id = ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException remarsh)
      {
        return sayHello();
      }
    catch (TRANSIENT ex)
      {
        throw ex;
      }
    finally
      {
        _releaseReply(in);
      }
  }

  /**
  * Passes wide string.
  */
  public String passCharacters(String wide, String narrow)
  {
    org.omg.CORBA.portable.InputStream in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream out =
          _request("passCharacters", true);
        out.write_wstring(wide);
        out.write_string(narrow);
        in = _invoke(out);

        String result = in.read_wstring();
        return result;
      }
    catch (org.omg.CORBA.portable.ApplicationException ex)
      {
        in = ex.getInputStream();

        String _id = ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException remarsh)
      {
        return passCharacters(wide, narrow);
      }
    finally
      {
        _releaseReply(in);
      }
  } // passCharacters

  /**
   * Throws either 'ourUserException' with the 'ourField' field
   * initialised to the passed positive value
   * or system exception (if the parameter is zero or negative).
   */
  public void throwException(int parameter)
                      throws ourUserException
  {
    org.omg.CORBA.portable.InputStream in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream out =
          _request("throwException", true);
        out.write_long(parameter);
        in = _invoke(out);
        return;
      }
    catch (org.omg.CORBA.portable.ApplicationException ex)
      {
        in = ex.getInputStream();

        String _id = ex.getId();
        if (_id.equals(ourUserExceptionHelper.id()))
          throw ourUserExceptionHelper.read(in);
        else
          throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException remarsh)
      {
        throwException(parameter);
      }
    finally
      {
        _releaseReply(in);
      }
  }

  private static String[] ids = { poa_comTesterHelper.id() };

  public String[] _ids()
  {
    return ids;
  }
}