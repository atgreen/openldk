/*
  Copyright (C) 2003 C. Brian Jones
  Copyright (C) 2004 Mark J. Wielaard
  
  This file is part of Mauve.
  
  Mauve is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2, or (at your option)
  any later version.
  
  Mauve is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with Mauve; see the file COPYING.  If not, write to
  the Free Software Foundation, 59 Temple Place - Suite 330,
  Boston, MA 02111-1307, USA.
*/

package gnu.testlet.java.net.Socket;

import java.net.*;
import java.lang.reflect.*;

public class TestSocketImplFactory
  implements SocketImplFactory
{
  private Constructor implConstructor = null;
  
  public TestSocketImplFactory()
  {
    // We better make sure we can actually return something in case
    // this factory is actually used later.  The trick we use is to
    // create a Socket with the empty constructor (which is defined as
    // returning an unconnected Socket with the default SocketImpl).
    // Then we use reflection to scoop out this object.
    SocketImpl impl = null;
    try
      {
	Class sic = Class.forName("java.net.SocketImpl");
	Socket s = new Socket();
	Field[] fields = s.getClass().getDeclaredFields();
	int i = 0;
	while (impl == null && i < fields.length)
	  {
	    Field f = fields[i];
	    Class fc = f.getType();
	    if (sic.isAssignableFrom(fc))
	      {
		f.setAccessible(true);
		impl = (SocketImpl) f.get(s);
	      }
	    i++;
	  }
      }
    catch (IllegalAccessException iae)
      {
	Error e = new InternalError("Unable to get default SocketImpl " + iae);
	e.initCause(iae);
	throw e;
      }
    catch (ClassNotFoundException cnf)
      {
	Error e = new InternalError("Unable to get default SocketImpl " + cnf);
	e.initCause(cnf);
	throw e;
      }

    if (impl == null)
      throw new InternalError("Couldn't determine default SocketImpl");

    // Now hope that there is a non-argument constructor.
    int i = 0;
    Constructor[] cons = impl.getClass().getDeclaredConstructors();
    while (implConstructor == null && i < cons.length)
      {
	Constructor c = cons[i];
	if (c.getParameterTypes().length == 0)
	  implConstructor = c;
	i++;
      }
    
    if (implConstructor == null)
      throw new InternalError("Couldn't get SocketImpl Constructor");
    else
      implConstructor.setAccessible(true);
  }

  public SocketImpl createSocketImpl ()
  {
    try
      {
	return (SocketImpl) implConstructor.newInstance(new Object[0]);
      }
    catch (InstantiationException ie)
      {
	ie.printStackTrace();
	// Null is better then nothing?
	return null;
      }
    catch (IllegalAccessException iae)
      {
	iae.printStackTrace();
	// Null is better then nothing?
	return null;
      }
    catch(InvocationTargetException ite)
      {
	ite.printStackTrace();
	// Null is better then nothing?
	return null;
      }
  }
}
