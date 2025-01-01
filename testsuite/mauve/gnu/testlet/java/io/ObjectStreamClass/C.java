// Tags: not-a-test

package gnu.testlet.java.io.ObjectStreamClass;

import java.io.Externalizable;
import java.io.ObjectInput;
import java.io.ObjectOutput;

class C extends B implements Cloneable, Externalizable
{
  public void absfoo () {}
  public void readExternal (ObjectInput i) {}
  public void writeExternal (ObjectOutput o) {}
}
