/* MyBeanContextSupport.java -- support class
   Copyright (C) 2006 David Gilbert <david.gilbert@object-refinery.com>
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

package gnu.testlet.java.beans.beancontext.BeanContextSupport;

import java.beans.PropertyChangeListener;
import java.beans.VetoableChangeListener;
import java.beans.Visibility;
import java.beans.beancontext.BeanContextChild;
import java.beans.beancontext.BeanContextMembershipListener;
import java.beans.beancontext.BeanContextSupport;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.Collection;

public class MyBeanContextSupport extends BeanContextSupport
{
  public MyBeanContextSupport() 
  {
    super();
  }
  
  public static BeanContextChild getChildBeanContextChildX(Object child)
  {
    return BeanContextSupport.getChildBeanContextChild(child);
  }
  
  public static BeanContextMembershipListener getChildBeanContextMembershipListenerX(
          Object child)
  {
    return BeanContextSupport.getChildBeanContextMembershipListener(child);
  }

  public static PropertyChangeListener getChildPropertyChangeListenerX(
          Object child)
  {
    return BeanContextSupport.getChildPropertyChangeListener(child);
  }

  public static Serializable getChildSerializableX(Object child)
  {
    return BeanContextSupport.getChildSerializable(child);
  }

  public static VetoableChangeListener getChildVetoableChangeListenerX(
          Object child)
  {
    return BeanContextSupport.getChildVetoableChangeListener(child);
  }

  public static Visibility getChildVisibilityX(Object child)
  {
    return BeanContextSupport.getChildVisibility(child);
  }
  
  protected void deserializeX(ObjectInputStream ois, Collection coll)
      throws IOException, ClassNotFoundException 
  {
    super.deserialize(ois, coll);    
  }
  
  protected void serializeX(ObjectOutputStream oos, Collection coll)
      throws IOException
  {
    super.serialize(oos, coll);
  }
  
}
