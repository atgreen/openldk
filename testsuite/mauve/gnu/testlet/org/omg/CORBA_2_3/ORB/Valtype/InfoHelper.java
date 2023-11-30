// Tags: not-a-test
// Uses: InfoImpl InfoValueFactory

/*
* This file is part of the CORBA 2_3 tests, the test executable
* class being gnu.testlet.org.omg.CORBA_2_3.ORB.ValueTypeTest.
* Due large number of the required classes, they are moved into
* a separate package, Valuetype.
*
* @author Audrius Meskauskas (AudriusA@bluewin.ch)
*/


package gnu.testlet.org.omg.CORBA_2_3.ORB.Valtype;

import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

public abstract class InfoHelper
{
  public static int testMode = 4;
  private static String _id =
    "IDL:gnu/testlet/org/omg/CORBA_2_3/ORB/Valtype/Info:1.0";

  public static void insert(org.omg.CORBA.Any a, Info that)
  {
    OutputStream out = a.create_output_stream();
    a.type(type());
    write(out, that);
    a.read_value(out.create_input_stream(), type());
  }

  public static Info extract(org.omg.CORBA.Any a)
  {
    return read(a.create_input_stream());
  }

  private static org.omg.CORBA.TypeCode typeCode = null;

  public static org.omg.CORBA.TypeCode type()
  {
    if (typeCode == null)
      {
        org.omg.CORBA.ValueMember[] members =
          new org.omg.CORBA.ValueMember[ 2 ];

        org.omg.CORBA.TypeCode member;

        // ValueMember instance for _name
        member = org.omg.CORBA.ORB.init().create_string_tc(0);
        members [ 0 ] =
          new org.omg.CORBA.ValueMember("_name", "", _id, "", member, null,
                                        org.omg.CORBA.PRIVATE_MEMBER.value
                                       );

        // ValueMember instance for _message
        member = org.omg.CORBA.ORB.init().create_string_tc(0);
        members [ 1 ] =
          new org.omg.CORBA.ValueMember("_message", "", _id, "", member, null,
                                        org.omg.CORBA.PRIVATE_MEMBER.value
                                       );
        typeCode =
          org.omg.CORBA.ORB.init().create_value_tc(_id, "Info",
                                                   org.omg.CORBA.VM_NONE.value,
                                                   null, members
                                                  );
      }
    return typeCode;
  }

  public static String id()
  {
    return _id;
  }

  public static Info read(InputStream istream)
  {
    // This method is a real disaster, but the most of the IDL compilers
    // generate like that:
    if (testMode == 0)
      return (Info) ((org.omg.CORBA_2_3.portable.InputStream) istream).read_value(id());
    else if (testMode == 1)
      return (Info) ((org.omg.CORBA_2_3.portable.InputStream) istream).read_value(new InfoImpl());
    else 
      return (Info) ((org.omg.CORBA_2_3.portable.InputStream) istream).read_value(InfoImpl.class);
  }

  public static void write(OutputStream ostream, Info value)
  {
    ((org.omg.CORBA_2_3.portable.OutputStream) ostream).write_value(value, id());
  }

  public static Info create(org.omg.CORBA.ORB orb, String name, String message)
  {
    try
      {
        InfoValueFactory factory =
          (InfoValueFactory) ((org.omg.CORBA_2_3.ORB) orb).lookup_value_factory(id());
        return factory.create(name, message);
      }
    catch (ClassCastException ex)
      {
        throw new org.omg.CORBA.BAD_PARAM();
      }
  }
}
