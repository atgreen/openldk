// Tags: not-a-test
/*
* This file is part of the CORBA 2_3 tests, the test executable
* class being gnu.testlet.org.omg.CORBA_2_3.ORB.ValueTypeTest.
* Due large number of the required classes, they are moved into
* a separate package, Valuetype.
*
* @author Audrius Meskauskas (AudriusA@bluewin.ch)
*/


package gnu.testlet.org.omg.CORBA_2_3.ORB.Valtype;

import org.omg.CORBA.Any;
import org.omg.CORBA.ORB;
import org.omg.CORBA.PRIVATE_MEMBER;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.VM_NONE;
import org.omg.CORBA.ValueMember;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

public abstract class cmInfoHelper
{
  private static String _id =
    "IDL:gnu/testlet/org/omg/CORBA_2_3/ORB/Valtype/cmInfo:1.0";

  public static void insert(Any a, cmInfo that)
  {
    OutputStream out = a.create_output_stream();
    a.type(type());
    write(out, that);
    a.read_value(out.create_input_stream(), type());
  }

  public static cmInfo extract(Any a)
  {
    return read(a.create_input_stream());
  }

  private static TypeCode typeCode = null;

  public static TypeCode type()
  {
    if (typeCode == null)
      {
        ValueMember[] members = new ValueMember[ 2 ];

        TypeCode member;

        // ValueMember instance for _name
        member = ORB.init().create_string_tc(0);
        members [ 0 ] =
          new ValueMember("_name", "", _id, "", member, null,
                          PRIVATE_MEMBER.value
                         );

        // ValueMember instance for _message
        member = ORB.init().create_string_tc(0);
        members [ 1 ] =
          new ValueMember("_message", "", _id, "", member, null,
                          PRIVATE_MEMBER.value
                         );
        typeCode =
          ORB.init().create_value_tc(_id, "cmInfo", VM_NONE.value, null, members);
      }
    return typeCode;
  }

  public static String id()
  {
    return _id;
  }

  public static cmInfo read(InputStream istream)
  {
    return (cmInfo) ((org.omg.CORBA_2_3.portable.InputStream) istream).read_value(id());
  }

  public static void write(OutputStream ostream, cmInfo value)
  {
    ((org.omg.CORBA_2_3.portable.OutputStream) ostream).write_value(value, id());
  }
}
