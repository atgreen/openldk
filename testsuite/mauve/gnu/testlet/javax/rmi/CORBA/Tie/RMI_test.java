// Tags: not-a-test
// Not a test, required by RMI_IIOP.java.

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

package gnu.testlet.javax.rmi.CORBA.Tie;

import gnu.testlet.org.omg.CORBA_2_3.ORB.Valtype.Info;
import gnu.testlet.org.omg.CORBA_2_3.ORB.Valtype.cmInfo;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.Collection;

/**
 * This file is part of the CORBA RMI over IIOP the test executable
 * class being gnu.testlet.javax.rmi.CORBA.Tie.RMI_IIOP.
 *
 * @author Audrius Meskauskas (AudriusA@bluewin.ch)
 */
public interface RMI_test
  extends Remote
{
  String sayHello(RMI_test h)
    throws RemoteException;

  String joinStrings(String a, String b)
    throws RemoteException;

  long multiply(byte a, long b)
    throws RemoteException;

  int passArray(int[] array)
    throws RemoteException;

  String passStringArray(String[] array)
    throws RemoteException;

  String passPrimitives(byte b, double d, int i, String s, float f, char c,
    short sh)
    throws RemoteException;

  String passStructure(myStructure s)
    throws RemoteException;

  String passStructureArray(myStructure[] structures)
    throws RemoteException;

  String passCorbaCMValueType(cmInfo info)
    throws RemoteException;

  String passCorbaValueType(Info info)
    throws RemoteException;

  String passCorbaValueTypeArray(Info[] infos)
    throws RemoteException;

  String passCorbaObject(org.omg.CORBA.Object object)
    throws RemoteException;

  NodeObject exchangeNodeObject(NodeObject nx)
    throws RemoteException;

  String passArrayOfRemotes(RMI_test[] tests)
    throws RemoteException;

  RMI_test passReturnRemote(RMI_test test)
    throws RemoteException;

  String getEgo()
    throws RemoteException;

  String passCollection(Collection cx)
    throws RemoteException;

}
