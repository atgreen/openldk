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

import java.util.StringTokenizer;

import org.omg.CORBA.DataInputStream;
import org.omg.CORBA.DataOutputStream;
import org.omg.CORBA.portable.CustomValue;

/**
 * This value type object has the user defined methods for reading
 * and writing. To make something different, we write two components
 * as a single string, separating them by '#'.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public abstract class cmInfo
  implements CustomValue
{
  public String name;
  public String message;
  private static String[] _truncatable_ids = { cmInfoHelper.id() };

  public String[] _truncatable_ids()
  {
    return _truncatable_ids;
  }

  public abstract String _toString();

  public void unmarshal(DataInputStream istream)
  {
    String s = istream.read_string();
    StringTokenizer st = new StringTokenizer(s, "#");
    name = st.nextToken();
    message = st.nextToken();
  }

  public void marshal(DataOutputStream ostream)
  {
    ostream.write_string(name+"#"+message);
  }

  public String toString()
  {
    return _toString();
  }

}
