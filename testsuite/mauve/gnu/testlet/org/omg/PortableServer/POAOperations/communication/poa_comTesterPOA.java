// Tags: not-a-test
// Uses: poa_comTesterOperations ourUserExceptionHelper

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
public abstract class poa_comTesterPOA
  extends org.omg.PortableServer.Servant
  implements poa_comTesterOperations, org.omg.CORBA.portable.InvokeHandler
{
  // Constructors
  private static java.util.Hashtable methods = new java.util.Hashtable();

  static
  {
    methods.put("_get_theField", new Integer(0));
    methods.put("_set_theField", new Integer(1));
    methods.put("sayHello", new Integer(2));
    methods.put("throwException", new Integer(5));
    methods.put("passCharacters", new Integer(4));
  }

  public org.omg.CORBA.portable.OutputStream _invoke(String a_method,
                                                     org.omg.CORBA.portable.InputStream in,
                                                     org.omg.CORBA.portable.ResponseHandler handler
                                                    )
  {
    org.omg.CORBA.portable.OutputStream out = null;
    Integer method = (Integer) methods.get(a_method);
    if (method == null)
      throw new org.omg.CORBA.BAD_OPERATION(0,
                                            org.omg.CORBA.CompletionStatus.COMPLETED_MAYBE
                                           );

    switch (method.intValue())
      {
        /**
         * The field that can be either set of checked.
         */
        case 0 :
        {
          int result = this.theField();
          out = handler.createReply();
          out.write_long(result);
          break;
        }

        /**
         * The field that can be either set of checked.
         */
        case 1 :
        {
          int newTheField = in.read_long();
          this.theField(newTheField);
          out = handler.createReply();
          break;
        }

        /**
        * Handles a simple message.
        */
        case 2 :
        {
          String r = sayHello();
          out = handler.createReply();
          out.write_string(r);
          break;
        }

        /**
         * Passes wide string.
         */
        case 4 :
        {
          String wide = in.read_wstring();
          String narrow = in.read_string();
          String result = null;
          result = this.passCharacters(wide, narrow);
          out = handler.createReply();
          out.write_wstring(result);
          break;
        }

        /**
        * Throws either 'ourUserException' with the 'ourField' field
        * initialised to the passed positive value
        * or system exception (if the parameter is zero or negative).
        */
        case 5 :
        {
          try
            {
              int parameter = in.read_long();
              this.throwException(parameter);
              out = handler.createReply();
            }
          catch (ourUserException ex)
            {
              out = handler.createExceptionReply();
              ourUserExceptionHelper.write(out, ex);
            }
          break;
        }

        default :
          throw new org.omg.CORBA.BAD_OPERATION(10000 + method.intValue(),
                                                org.omg.CORBA.CompletionStatus.COMPLETED_MAYBE
                                               );
      }

    return out;
  }

  private static String[] ids = { poa_comTesterHelper.id() };

  public String[] _all_interfaces(org.omg.PortableServer.POA poa,
                                  byte[] objectId
                                 )
  {
    return ids;
  }

  public poa_comTester _this()
  {
    return poa_comTesterHelper.narrow(super._this_object());
  }

  public poa_comTester _this(org.omg.CORBA.ORB orb)
  {
    return poa_comTesterHelper.narrow(super._this_object(orb));
  }
}
