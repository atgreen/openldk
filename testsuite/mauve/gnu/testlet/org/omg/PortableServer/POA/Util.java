// Copyright (c) IONA Technologies, Inc. Waltham, MA, USA.

// Adapted for Mauve by Audrius Meskauskas <audriusa@bluewin.ch>

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

/*
This code originally came from the OMG's CORBA Open Source Testing project,
which lived at cost.omg.org. That site no longer exists.

All the contributing companies agreed to release their tests under the
terms of the GNU Lesser General Public License, available in the file
COPYING.LIB.

The code has been modified integrating into Mauve test environment and
removing tests that are not yet supported by Suns jre 1.4. Hence the license
is now GPL.

We downloaded the code from http://sourceforge.net/projects/corba-cost/,
administrated by Duncan Grisby.
*/

// **********************************************************************
//
// Copyright (c) 2001
// IONA Technologies, Inc.
// Waltham, MA, USA
//
// All Rights Reserved
//
// **********************************************************************


package gnu.testlet.org.omg.PortableServer.POA;

public final class Util
{
  //
  // Unmarshal a system exception
  //
  public static org.omg.CORBA.SystemException unmarshalSystemException(org.omg.CORBA.portable.InputStream in)
  {
    String id = in.read_string();
    int minor = in.read_ulong();
    org.omg.CORBA.CompletionStatus status =
      org.omg.CORBA.CompletionStatus.from_int(in.read_ulong());

    if (id.equals("IDL:omg.org/CORBA/BAD_PARAM:1.0"))
      {
        return new org.omg.CORBA.BAD_PARAM(minor, status);
      }
    else if (id.equals("IDL:omg.org/CORBA/NO_MEMORY:1.0"))
      {
        return new org.omg.CORBA.NO_MEMORY(minor, status);
      }
    else if (id.equals("IDL:omg.org/CORBA/IMP_LIMIT:1.0"))
      {
        return new org.omg.CORBA.IMP_LIMIT(minor, status);
      }
    else if (id.equals("IDL:omg.org/CORBA/COMM_FAILURE:1.0"))
      {
        return new org.omg.CORBA.COMM_FAILURE(minor, status);
      }
    else if (id.equals("IDL:omg.org/CORBA/INV_OBJREF:1.0"))
      {
        return new org.omg.CORBA.INV_OBJREF(minor, status);
      }
    else if (id.equals("IDL:omg.org/CORBA/NO_PERMISSION:1.0"))
      {
        return new org.omg.CORBA.NO_PERMISSION(minor, status);
      }
    else if (id.equals("IDL:omg.org/CORBA/INTERNAL:1.0"))
      {
        return new org.omg.CORBA.INTERNAL(minor, status);
      }
    else if (id.equals("IDL:omg.org/CORBA/MARSHAL:1.0"))
      {
        return new org.omg.CORBA.MARSHAL(minor, status);
      }
    else if (id.equals("IDL:omg.org/CORBA/INITIALIZE:1.0"))
      {
        return new org.omg.CORBA.INITIALIZE(minor, status);
      }
    else if (id.equals("IDL:omg.org/CORBA/NO_IMPLEMENT:1.0"))
      {
        return new org.omg.CORBA.NO_IMPLEMENT(minor, status);
      }
    else if (id.equals("IDL:omg.org/CORBA/BAD_TYPECODE:1.0"))
      {
        return new org.omg.CORBA.BAD_TYPECODE(minor, status);
      }
    else if (id.equals("IDL:omg.org/CORBA/BAD_OPERATION:1.0"))
      {
        return new org.omg.CORBA.BAD_OPERATION(minor, status);
      }
    else if (id.equals("IDL:omg.org/CORBA/NO_RESOURCES:1.0"))
      {
        return new org.omg.CORBA.NO_RESOURCES(minor, status);
      }
    else if (id.equals("IDL:omg.org/CORBA/NO_RESPONSE:1.0"))
      {
        return new org.omg.CORBA.NO_RESPONSE(minor, status);
      }
    else if (id.equals("IDL:omg.org/CORBA/PERSIST_STORE:1.0"))
      {
        return new org.omg.CORBA.PERSIST_STORE(minor, status);
      }
    else if (id.equals("IDL:omg.org/CORBA/BAD_INV_ORDER:1.0"))
      {
        return new org.omg.CORBA.BAD_INV_ORDER(minor, status);
      }
    else if (id.equals("IDL:omg.org/CORBA/TRANSIENT:1.0"))
      {
        return new org.omg.CORBA.TRANSIENT(minor, status);
      }
    else if (id.equals("IDL:omg.org/CORBA/FREE_MEM:1.0"))
      {
        return new org.omg.CORBA.FREE_MEM(minor, status);
      }
    else if (id.equals("IDL:omg.org/CORBA/INV_IDENT:1.0"))
      {
        return new org.omg.CORBA.INV_IDENT(minor, status);
      }
    else if (id.equals("IDL:omg.org/CORBA/INV_FLAG:1.0"))
      {
        return new org.omg.CORBA.INV_FLAG(minor, status);
      }
    else if (id.equals("IDL:omg.org/CORBA/INTF_REPOS:1.0"))
      {
        return new org.omg.CORBA.INTF_REPOS(minor, status);
      }
    else if (id.equals("IDL:omg.org/CORBA/BAD_CONTEXT:1.0"))
      {
        return new org.omg.CORBA.BAD_CONTEXT(minor, status);
      }
    else if (id.equals("IDL:omg.org/CORBA/OBJ_ADAPTER:1.0"))
      {
        return new org.omg.CORBA.OBJ_ADAPTER(minor, status);
      }
    else if (id.equals("IDL:omg.org/CORBA/DATA_CONVERSION:1.0"))
      {
        return new org.omg.CORBA.DATA_CONVERSION(minor, status);
      }
    else if (id.equals("IDL:omg.org/CORBA/OBJECT_NOT_EXIST:1.0"))
      {
        return new org.omg.CORBA.OBJECT_NOT_EXIST(minor, status);
      }
    else if (id.equals("IDL:omg.org/CORBA/TRANSACTION_REQUIRED:1.0"))
      {
        return new org.omg.CORBA.TRANSACTION_REQUIRED(minor, status);
      }
    else if (id.equals("IDL:omg.org/CORBA/TRANSACTION_ROLLEDBACK:1.0"))
      {
        return new org.omg.CORBA.TRANSACTION_ROLLEDBACK(minor, status);
      }
    else if (id.equals("IDL:omg.org/CORBA/INVALID_TRANSACTION:1.0"))
      {
        return new org.omg.CORBA.INVALID_TRANSACTION(minor, status);
      }
    else if (id.equals("IDL:omg.org/CORBA/INV_POLICY:1.0"))
      {
        return new org.omg.CORBA.INV_POLICY(minor, status);
      }
    else if (id.equals("IDL:omg.org/CORBA/CODESET_INCOMPATIBLE:1.0"))
      {
        return new org.omg.CORBA.BAD_OPERATION(minor, status);
      }

    // Disabled since these are only supported by CORBA 2.4

    /*
            else if(id.equals("IDL:omg.org/CORBA/REBIND:1.0"))
      {
          return new org.omg.CORBA.REBIND(minor, status);
      }
            else if(id.equals("IDL:omg.org/CORBA/TIMEOUT:1.0"))
      {
          return new org.omg.CORBA.TIMEOUT(minor, status);
      }
            else if(id.equals("IDL:omg.org/CORBA/TRANSACTION_UNAVAILABLE:1.0"))
      {
          return new org.omg.CORBA.TRANSACTION_UNAVAILABLE(minor, status);
      }
            else if(id.equals("IDL:omg.org/CORBA/TRANSACTION_MODE:1.0"))
      {
          return new org.omg.CORBA.TRANSACTION_MODE(minor, status);
      }
            else if(id.equals("IDL:omg.org/CORBA/BAD_QOS:1.0"))
      {
          return new org.omg.CORBA.BAD_QOS(minor, status);
      }
    */

    //
    // Unknown exception
    //
    return new org.omg.CORBA.UNKNOWN(minor, status);
  }

  private static String[] sysExClassNames_ =
    {
      "org.omg.CORBA.BAD_CONTEXT", "org.omg.CORBA.BAD_INV_ORDER",
      "org.omg.CORBA.BAD_OPERATION", "org.omg.CORBA.BAD_PARAM",
      "org.omg.CORBA.BAD_QOS", "org.omg.CORBA.BAD_TYPECODE",
      "org.omg.CORBA.CODESET_INCOMPATIBLE", "org.omg.CORBA.COMM_FAILURE",
      "org.omg.CORBA.DATA_CONVERSION", "org.omg.CORBA.FREE_MEM",
      "org.omg.CORBA.IMP_LIMIT", "org.omg.CORBA.INITIALIZE",
      "org.omg.CORBA.INTERNAL", "org.omg.CORBA.INTF_REPOS",
      "org.omg.CORBA.INVALID_TRANSACTION", "org.omg.CORBA.INV_FLAG",
      "org.omg.CORBA.INV_IDENT", "org.omg.CORBA.INV_OBJREF",
      "org.omg.CORBA.INV_POLICY", "org.omg.CORBA.MARSHAL",
      "org.omg.CORBA.NO_IMPLEMENT", "org.omg.CORBA.NO_MEMORY",
      "org.omg.CORBA.NO_PERMISSION", "org.omg.CORBA.NO_RESOURCES",
      "org.omg.CORBA.NO_RESPONSE", "org.omg.CORBA.OBJECT_NOT_EXIST",
      "org.omg.CORBA.OBJ_ADAPTER", "org.omg.CORBA.PERSIST_STORE",
      "org.omg.CORBA.REBIND", "org.omg.CORBA.TIMEOUT",
      "org.omg.CORBA.TRANSACTION_MODE", "org.omg.CORBA.TRANSACTION_REQUIRED",
      "org.omg.CORBA.TRANSACTION_ROLLEDBACK",
      "org.omg.CORBA.TRANSACTION_UNAVAILABLE", "org.omg.CORBA.TRANSIENT",
      "org.omg.CORBA.UNKNOWN"
    };
  private static String[] sysExIds_ =
    {
      "IDL:omg.org/CORBA/BAD_CONTEXT:1.0", "IDL:omg.org/CORBA/BAD_INV_ORDER:1.0",
      "IDL:omg.org/CORBA/BAD_OPERATION:1.0", "IDL:omg.org/CORBA/BAD_PARAM:1.0",
      "IDL:omg.org/CORBA/BAD_QOS:1.0", "IDL:omg.org/CORBA/BAD_TYPECODE:1.0",
      "IDL:omg.org/CORBA/CODESET_INCOMPATIBLE:1.0",
      "IDL:omg.org/CORBA/COMM_FAILURE:1.0",
      "IDL:omg.org/CORBA/DATA_CONVERSION:1.0", "IDL:omg.org/CORBA/FREE_MEM:1.0",
      "IDL:omg.org/CORBA/IMP_LIMIT:1.0", "IDL:omg.org/CORBA/INITIALIZE:1.0",
      "IDL:omg.org/CORBA/INTERNAL:1.0", "IDL:omg.org/CORBA/INTF_REPOS:1.0",
      "IDL:omg.org/CORBA/INVALID_TRANSACTION:1.0",
      "IDL:omg.org/CORBA/INV_FLAG:1.0", "IDL:omg.org/CORBA/INV_IDENT:1.0",
      "IDL:omg.org/CORBA/INV_OBJREF:1.0", "IDL:omg.org/CORBA/INV_POLICY:1.0",
      "IDL:omg.org/CORBA/MARSHAL:1.0", "IDL:omg.org/CORBA/NO_IMPLEMENT:1.0",
      "IDL:omg.org/CORBA/NO_MEMORY:1.0", "IDL:omg.org/CORBA/NO_PERMISSION:1.0",
      "IDL:omg.org/CORBA/NO_RESOURCES:1.0", "IDL:omg.org/CORBA/NO_RESPONSE:1.0",
      "IDL:omg.org/CORBA/OBJECT_NOT_EXIST:1.0",
      "IDL:omg.org/CORBA/OBJ_ADAPTER:1.0", "IDL:omg.org/CORBA/PERSIST_STORE:1.0",
      "IDL:omg.org/CORBA/REBIND:1.0", "IDL:omg.org/CORBA/TIMEOUT:1.0",
      "IDL:omg.org/CORBA/TRANSACTION_MODE:1.0",
      "IDL:omg.org/CORBA/TRANSACTION_REQUIRED:1.0",
      "IDL:omg.org/CORBA/TRANSACTION_ROLLEDBACK:1.0",
      "IDL:omg.org/CORBA/TRANSACTION_UNAVAILABLE:1.0",
      "IDL:omg.org/CORBA/TRANSIENT:1.0", "IDL:omg.org/CORBA/UNKNOWN:1.0"
    };

  private static int binarySearch(String[] arr, String value)
  {
    int left = 0;
    int right = arr.length;
    int index = -1;

    while (left < right)
      {
        int m = (left + right) / 2;
        int res = arr [ m ].compareTo(value);
        if (res == 0)
          {
            index = m;
            break;
          }
        else if (res > 0)
          right = m;
        else
          left = m + 1;
      }

    return index;
  }

  //
  // Determine the repository ID of an exception
  //
  public static String getExceptionId(Exception ex)
  {
    if (ex instanceof org.omg.CORBA.SystemException)
      {
        String className = ex.getClass().getName();
        int index = binarySearch(sysExClassNames_, className);

        if (index == -1)
          return "IDL:omg.org/CORBA/UNKNOWN:1.0";
        else
          return sysExIds_ [ index ];
      }
    else if (ex instanceof org.omg.CORBA.UserException)
      {
        Class exClass = ex.getClass();
        String className = exClass.getName();
        String id = null;
        try
          {
            Class c = Class.forName(className + "Helper");
            java.lang.reflect.Method m = c.getMethod("id", null);
            id = (String) m.invoke(null, null);
          }
        catch (ClassNotFoundException e)
          {
          }
        catch (NoSuchMethodException e)
          {
            throw new RuntimeException(e);
          }
        catch (IllegalAccessException e)
          {
            throw new RuntimeException(e);
          }
        catch (IllegalArgumentException e)
          {
            throw new RuntimeException(e);
          }
        catch (java.lang.reflect.InvocationTargetException e)
          {
            throw new RuntimeException(e);
          }
        catch (SecurityException e)
          {
          }

        //
        // TODO: Is this correct?
        //
        if (id == null)
          return "IDL:omg.org/CORBA/UserException:1.0";
        else
          return id;
      }
    else
      {
        throw new RuntimeException();
      }
  }
}