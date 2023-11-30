// Tags: not-a-test
// Uses: ../../Asserter

// Copyright (c) 2000, 2001 NEC Corporation.

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

// Copyright (c) 2000, 2001 NEC Corporation. All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
// 1. The functionality to test the interoperability specified by the
//    Object Management Group's CORBA/IIOP specification version two (or
//    later versions) must be preserved.
//
// 2. Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer as the
//    first lines of this file unmodified.
//
// 3. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the
//    distribution.
//
// THIS SOFTWARE IS PROVIDED BY NEC CORPORATION ``AS IS'' AND ANY EXPRESS
// OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL NEC CORPORATION BE LIABLE FOR ANY
// DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
// GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
// IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
// ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
// CVS
// $Id: rf11Caller.java,v 1.2 2008/06/25 08:01:02 twisti Exp $
//

package gnu.testlet.org.omg.CORBA.ORB.RF11;

import org.omg.CORBA.ORB;
import gnu.testlet.org.omg.CORBA.Asserter;
import gnu.testlet.TestHarness;

public class rf11Caller extends Asserter
{
  ORB orb;
  NEC_RF11 target;

  public void init(ORB _orb, NEC_RF11 _target)
  {
    orb = _orb;
    target = _target;
  }

  //runtime routines
  boolean comp_0000(org.omg.CORBA.Any _v1)
  {
    int _v2;
    _v2 = _v1.extract_long();
    return (_v2 == -200000);
  }

  C_struct cons_0000()
  {
    C_struct _v1;
    _v1 = new C_struct();
    _v1.e_short = -100;
    _v1.e_ushort = 100;
    _v1.e_long = -100000;
    _v1.e_ulong = 100000;
    _v1.e_float = 0.123f;
    _v1.e_double = 0.12e3;
    _v1.e_char = 'a';
    _v1.e_boolean = false;
    _v1.e_octet = 10;
    _v1.e_any = orb.create_any();
    _v1.e_any.insert_string("abc");
    _v1.e_string = "abc";
    _v1.e_Object = target;
    return (_v1);
  }

  boolean comp_0002(org.omg.CORBA.Any _v1)
  {
    int _v2;
    _v2 = _v1.extract_long();
    return (_v2 == -200000);
  }

  boolean comp_0001(C_struct _v1)
  {
    return (true && (_v1.e_short == -200) && (_v1.e_ushort == 200) &&
           (_v1.e_long == -200000) && (_v1.e_ulong == 200000) &&
           (_v1.e_float == 1.234f) && (_v1.e_double == 1.23e4) &&
           (_v1.e_char == 'b') && (_v1.e_boolean == true) &&
           (_v1.e_octet == 20) && comp_0002(_v1.e_any) &&
           (_v1.e_string.equals("def")) &&
           (_v1.e_Object._is_equivalent(target)));
  }

  C_union cons_0001()
  {
    C_union _v1;
    _v1 = new C_union();

    short _v2;
    _v2 = -100;
    _v1.e_short(_v2);
    return (_v1);
  }

  boolean comp_0003(C_union _v1)
  {
    if (_v1.discriminator() != 2)
      return false;
    return (_v1.e_ushort() == 200);
  }

  boolean comp_0004(org.omg.CORBA.Any _v1)
  {
    int _v2;
    _v2 = _v1.extract_long();
    return (_v2 == -200000);
  }

  boolean comp_0005(org.omg.CORBA.Any _v1)
  {
    int _v2;
    _v2 = _v1.extract_long();
    return (_v2 == -200000);
  }

  D_d_short cons_0002()
  {
    D_d_short _v1;
    _v1 = new D_d_short();

    int _v2;
    _v2 = -100000;
    _v1.l1(_v2);
    return (_v1);
  }

  boolean comp_0006(D_d_short _v1)
  {
    if (_v1.discriminator() != 2)
      return false;
    return (_v1.l2() == -200000);
  }

  D_d_ushort cons_0003()
  {
    D_d_ushort _v1;
    _v1 = new D_d_ushort();

    int _v2;
    _v2 = -100000;
    _v1.l1(_v2);
    return (_v1);
  }

  boolean comp_0007(D_d_ushort _v1)
  {
    if (_v1.discriminator() != 2)
      return false;
    return (_v1.l2() == -200000);
  }

  D_d_long cons_0004()
  {
    D_d_long _v1;
    _v1 = new D_d_long();

    int _v2;
    _v2 = -100000;
    _v1.l1(_v2);
    return (_v1);
  }

  boolean comp_0008(D_d_long _v1)
  {
    if (_v1.discriminator() != 2)
      return false;
    return (_v1.l2() == -200000);
  }

  D_d_ulong cons_0005()
  {
    D_d_ulong _v1;
    _v1 = new D_d_ulong();

    int _v2;
    _v2 = -100000;
    _v1.l1(_v2);
    return (_v1);
  }

  boolean comp_0009(D_d_ulong _v1)
  {
    if (_v1.discriminator() != 2)
      return false;
    return (_v1.l2() == -200000);
  }

  D_d_char cons_0006()
  {
    D_d_char _v1;
    _v1 = new D_d_char();

    int _v2;
    _v2 = -100000;
    _v1.l1(_v2);
    return (_v1);
  }

  boolean comp_0010(D_d_char _v1)
  {
    if (_v1.discriminator() != 'b')
      return false;
    return (_v1.l2() == -200000);
  }

  D_d_boolean cons_0007()
  {
    D_d_boolean _v1;
    _v1 = new D_d_boolean();

    int _v2;
    _v2 = -100000;
    _v1.l2(_v2);
    return (_v1);
  }

  boolean comp_0011(D_d_boolean _v1)
  {
    if (_v1.discriminator() != true)
      return false;
    return (_v1.l1() == -200000);
  }

  D_d_B cons_0008()
  {
    D_d_B _v1;
    _v1 = new D_d_B();

    int _v2;
    _v2 = -100000;
    _v1.l1(_v2);
    return (_v1);
  }

  boolean comp_0012(D_d_B _v1)
  {
    if (_v1.discriminator() != B.b2)
      return false;
    return (_v1.l2() == -200000);
  }

  E_struct cons_0009()
  {
    E_struct _v1;
    _v1 = new E_struct();
    _v1.e_b1 = B.b1;
    _v1.e_b2 = B.b1;
    return (_v1);
  }

  boolean comp_0013(E_struct _v1)
  {
    return (true && (_v1.e_b1 == B.b2) && (_v1.e_b2 == B.b2));
  }

  E_union cons_0010()
  {
    E_union _v1;
    _v1 = new E_union();

    B _v2;
    _v2 = B.b1;
    _v1.e_b1(_v2);
    return (_v1);
  }

  boolean comp_0014(E_union _v1)
  {
    if (_v1.discriminator() != 2)
      return false;
    return (_v1.e_b2() == B.b2);
  }

  C_struct cons_0012()
  {
    C_struct _v1;
    _v1 = new C_struct();
    _v1.e_short = -100;
    _v1.e_ushort = 100;
    _v1.e_long = -100000;
    _v1.e_ulong = 100000;
    _v1.e_float = 0.123f;
    _v1.e_double = 0.12e3;
    _v1.e_char = 'a';
    _v1.e_boolean = false;
    _v1.e_octet = 10;
    _v1.e_any = orb.create_any();
    _v1.e_any.insert_string("abc");
    _v1.e_string = "abc";
    _v1.e_Object = target;
    return (_v1);
  }

  C_union cons_0013()
  {
    C_union _v1;
    _v1 = new C_union();

    short _v2;
    _v2 = -100;
    _v1.e_short(_v2);
    return (_v1);
  }

  F_struct cons_0011()
  {
    F_struct _v1;
    _v1 = new F_struct();
    _v1.e_c_struct = cons_0012();
    _v1.e_c_union = cons_0013();
    _v1.e_c_sequence_e_short = new short[ 2 ];
    _v1.e_c_sequence_e_short [ 0 ] = -100;
    _v1.e_c_sequence_e_short [ 1 ] = -100;
    _v1.e_c_sequence_e_ushort = new short[ 2 ];
    _v1.e_c_sequence_e_ushort [ 0 ] = 100;
    _v1.e_c_sequence_e_ushort [ 1 ] = 100;
    _v1.e_c_sequence_e_long = new int[ 2 ];
    _v1.e_c_sequence_e_long [ 0 ] = -100000;
    _v1.e_c_sequence_e_long [ 1 ] = -100000;
    _v1.e_c_sequence_e_ulong = new int[ 2 ];
    _v1.e_c_sequence_e_ulong [ 0 ] = 100000;
    _v1.e_c_sequence_e_ulong [ 1 ] = 100000;
    _v1.e_c_sequence_e_float = new float[ 2 ];
    _v1.e_c_sequence_e_float [ 0 ] = 0.123f;
    _v1.e_c_sequence_e_float [ 1 ] = 0.123f;
    _v1.e_c_sequence_e_double = new double[ 2 ];
    _v1.e_c_sequence_e_double [ 0 ] = 0.12e3;
    _v1.e_c_sequence_e_double [ 1 ] = 0.12e3;
    _v1.e_c_sequence_e_char = new char[ 2 ];
    _v1.e_c_sequence_e_char [ 0 ] = 'a';
    _v1.e_c_sequence_e_char [ 1 ] = 'a';
    _v1.e_c_sequence_e_boolean = new boolean[ 2 ];
    _v1.e_c_sequence_e_boolean [ 0 ] = false;
    _v1.e_c_sequence_e_boolean [ 1 ] = false;
    _v1.e_c_sequence_e_octet = new byte[ 2 ];
    _v1.e_c_sequence_e_octet [ 0 ] = 10;
    _v1.e_c_sequence_e_octet [ 1 ] = 10;
    _v1.e_c_sequence_e_any = new org.omg.CORBA.Any[ 2 ];
    _v1.e_c_sequence_e_any [ 0 ] = orb.create_any();
    _v1.e_c_sequence_e_any [ 0 ].insert_string("abc");
    _v1.e_c_sequence_e_any [ 1 ] = orb.create_any();
    _v1.e_c_sequence_e_any [ 1 ].insert_string("abc");
    _v1.e_c_sequence_e_string = new String[ 2 ];
    _v1.e_c_sequence_e_string [ 0 ] = "abc";
    _v1.e_c_sequence_e_string [ 1 ] = "abc";
    _v1.e_c_sequence_e_Object = new org.omg.CORBA.Object[ 2 ];
    _v1.e_c_sequence_e_Object [ 0 ] = target;
    _v1.e_c_sequence_e_Object [ 1 ] = target;
    _v1.e_c_array_e_short = new short[ 2 ];
    _v1.e_c_array_e_short [ 0 ] = -100;
    _v1.e_c_array_e_short [ 1 ] = -100;
    _v1.e_c_array_e_ushort = new short[ 2 ];
    _v1.e_c_array_e_ushort [ 0 ] = 100;
    _v1.e_c_array_e_ushort [ 1 ] = 100;
    _v1.e_c_array_e_long = new int[ 2 ];
    _v1.e_c_array_e_long [ 0 ] = -100000;
    _v1.e_c_array_e_long [ 1 ] = -100000;
    _v1.e_c_array_e_ulong = new int[ 2 ];
    _v1.e_c_array_e_ulong [ 0 ] = 100000;
    _v1.e_c_array_e_ulong [ 1 ] = 100000;
    _v1.e_c_array_e_float = new float[ 2 ];
    _v1.e_c_array_e_float [ 0 ] = 0.123f;
    _v1.e_c_array_e_float [ 1 ] = 0.123f;
    _v1.e_c_array_e_double = new double[ 2 ];
    _v1.e_c_array_e_double [ 0 ] = 0.12e3;
    _v1.e_c_array_e_double [ 1 ] = 0.12e3;
    _v1.e_c_array_e_char = new char[ 2 ];
    _v1.e_c_array_e_char [ 0 ] = 'a';
    _v1.e_c_array_e_char [ 1 ] = 'a';
    _v1.e_c_array_e_boolean = new boolean[ 2 ];
    _v1.e_c_array_e_boolean [ 0 ] = false;
    _v1.e_c_array_e_boolean [ 1 ] = false;
    _v1.e_c_array_e_octet = new byte[ 2 ];
    _v1.e_c_array_e_octet [ 0 ] = 10;
    _v1.e_c_array_e_octet [ 1 ] = 10;
    _v1.e_c_array_e_any = new org.omg.CORBA.Any[ 2 ];
    _v1.e_c_array_e_any [ 0 ] = orb.create_any();
    _v1.e_c_array_e_any [ 0 ].insert_string("abc");
    _v1.e_c_array_e_any [ 1 ] = orb.create_any();
    _v1.e_c_array_e_any [ 1 ].insert_string("abc");
    _v1.e_c_array_e_string = new String[ 2 ];
    _v1.e_c_array_e_string [ 0 ] = "abc";
    _v1.e_c_array_e_string [ 1 ] = "abc";
    _v1.e_c_array_e_Object = new org.omg.CORBA.Object[ 2 ];
    _v1.e_c_array_e_Object [ 0 ] = target;
    _v1.e_c_array_e_Object [ 1 ] = target;
    return (_v1);
  }

  boolean comp_0017(org.omg.CORBA.Any _v1)
  {
    int _v2;
    _v2 = _v1.extract_long();
    return (_v2 == -200000);
  }

  boolean comp_0016(C_struct _v1)
  {
    return (true && (_v1.e_short == -200) && (_v1.e_ushort == 200) &&
           (_v1.e_long == -200000) && (_v1.e_ulong == 200000) &&
           (_v1.e_float == 1.234f) && (_v1.e_double == 1.23e4) &&
           (_v1.e_char == 'b') && (_v1.e_boolean == true) &&
           (_v1.e_octet == 20) && comp_0017(_v1.e_any) &&
           (_v1.e_string.equals("def")) &&
           (_v1.e_Object._is_equivalent(target)));
  }

  boolean comp_0018(C_union _v1)
  {
    if (_v1.discriminator() != 2)
      return false;
    return (_v1.e_ushort() == 200);
  }

  boolean comp_0015(F_struct _v1)
  {
    return (true && comp_0016(_v1.e_c_struct) && comp_0018(_v1.e_c_union) &&
           (
             true && (_v1.e_c_sequence_e_short [ 0 ] == -200) &&
             (_v1.e_c_sequence_e_short [ 1 ] == -200)
           ) &&
           (
             true && (_v1.e_c_sequence_e_ushort [ 0 ] == 200) &&
             (_v1.e_c_sequence_e_ushort [ 1 ] == 200)
           ) &&
           (
             true && (_v1.e_c_sequence_e_long [ 0 ] == -200000) &&
             (_v1.e_c_sequence_e_long [ 1 ] == -200000)
           ) &&
           (
             true && (_v1.e_c_sequence_e_ulong [ 0 ] == 200000) &&
             (_v1.e_c_sequence_e_ulong [ 1 ] == 200000)
           ) &&
           (
             true && (_v1.e_c_sequence_e_float [ 0 ] == 1.234f) &&
             (_v1.e_c_sequence_e_float [ 1 ] == 1.234f)
           ) &&
           (
             true && (_v1.e_c_sequence_e_double [ 0 ] == 1.23e4) &&
             (_v1.e_c_sequence_e_double [ 1 ] == 1.23e4)
           ) &&
           (
             true && (_v1.e_c_sequence_e_char [ 0 ] == 'b') &&
             (_v1.e_c_sequence_e_char [ 1 ] == 'b')
           ) &&
           (
             true && (_v1.e_c_sequence_e_boolean [ 0 ] == true) &&
             (_v1.e_c_sequence_e_boolean [ 1 ] == true)
           ) &&
           (
             true && (_v1.e_c_sequence_e_octet [ 0 ] == 20) &&
             (_v1.e_c_sequence_e_octet [ 1 ] == 20)
           ) &&
           (
             true && comp_0017(_v1.e_c_sequence_e_any [ 0 ]) &&
             comp_0017(_v1.e_c_sequence_e_any [ 1 ])
           ) &&
           (
             true && (_v1.e_c_sequence_e_string [ 0 ].equals("def")) &&
             (_v1.e_c_sequence_e_string [ 1 ].equals("def"))
           ) &&
           (
             true && (_v1.e_c_sequence_e_Object [ 0 ]._is_equivalent(target)) &&
             (_v1.e_c_sequence_e_Object [ 1 ]._is_equivalent(target))
           ) &&
           (
             true && (_v1.e_c_array_e_short [ 0 ] == -200) &&
             (_v1.e_c_array_e_short [ 1 ] == -200)
           ) &&
           (
             true && (_v1.e_c_array_e_ushort [ 0 ] == 200) &&
             (_v1.e_c_array_e_ushort [ 1 ] == 200)
           ) &&
           (
             true && (_v1.e_c_array_e_long [ 0 ] == -200000) &&
             (_v1.e_c_array_e_long [ 1 ] == -200000)
           ) &&
           (
             true && (_v1.e_c_array_e_ulong [ 0 ] == 200000) &&
             (_v1.e_c_array_e_ulong [ 1 ] == 200000)
           ) &&
           (
             true && (_v1.e_c_array_e_float [ 0 ] == 1.234f) &&
             (_v1.e_c_array_e_float [ 1 ] == 1.234f)
           ) &&
           (
             true && (_v1.e_c_array_e_double [ 0 ] == 1.23e4) &&
             (_v1.e_c_array_e_double [ 1 ] == 1.23e4)
           ) &&
           (
             true && (_v1.e_c_array_e_char [ 0 ] == 'b') &&
             (_v1.e_c_array_e_char [ 1 ] == 'b')
           ) &&
           (
             true && (_v1.e_c_array_e_boolean [ 0 ] == true) &&
             (_v1.e_c_array_e_boolean [ 1 ] == true)
           ) &&
           (
             true && (_v1.e_c_array_e_octet [ 0 ] == 20) &&
             (_v1.e_c_array_e_octet [ 1 ] == 20)
           ) &&
           (
             true && comp_0017(_v1.e_c_array_e_any [ 0 ]) &&
             comp_0017(_v1.e_c_array_e_any [ 1 ])
           ) &&
           (
             true && (_v1.e_c_array_e_string [ 0 ].equals("def")) &&
             (_v1.e_c_array_e_string [ 1 ].equals("def"))
           ) &&
           (
             true && (_v1.e_c_array_e_Object [ 0 ]._is_equivalent(target)) &&
             (_v1.e_c_array_e_Object [ 1 ]._is_equivalent(target))
           ));
  }

  C_struct cons_0015()
  {
    C_struct _v1;
    _v1 = new C_struct();
    _v1.e_short = -100;
    _v1.e_ushort = 100;
    _v1.e_long = -100000;
    _v1.e_ulong = 100000;
    _v1.e_float = 0.123f;
    _v1.e_double = 0.12e3;
    _v1.e_char = 'a';
    _v1.e_boolean = false;
    _v1.e_octet = 10;
    _v1.e_any = orb.create_any();
    _v1.e_any.insert_string("abc");
    _v1.e_string = "abc";
    _v1.e_Object = target;
    return (_v1);
  }

  F_union cons_0014()
  {
    F_union _v1;
    _v1 = new F_union();

    C_struct _v2;
    _v2 = cons_0015();
    _v1.e_c_struct(_v2);
    return (_v1);
  }

  boolean comp_0020(C_union _v1)
  {
    if (_v1.discriminator() != 2)
      return false;
    return (_v1.e_ushort() == 200);
  }

  boolean comp_0019(F_union _v1)
  {
    if (_v1.discriminator() != 2)
      return false;
    return comp_0020(_v1.e_c_union());
  }

  C_struct cons_0016()
  {
    C_struct _v1;
    _v1 = new C_struct();
    _v1.e_short = -100;
    _v1.e_ushort = 100;
    _v1.e_long = -100000;
    _v1.e_ulong = 100000;
    _v1.e_float = 0.123f;
    _v1.e_double = 0.12e3;
    _v1.e_char = 'a';
    _v1.e_boolean = false;
    _v1.e_octet = 10;
    _v1.e_any = orb.create_any();
    _v1.e_any.insert_string("abc");
    _v1.e_string = "abc";
    _v1.e_Object = target;
    return (_v1);
  }

  boolean comp_0022(org.omg.CORBA.Any _v1)
  {
    int _v2;
    _v2 = _v1.extract_long();
    return (_v2 == -200000);
  }

  boolean comp_0021(C_struct _v1)
  {
    return (true && (_v1.e_short == -200) && (_v1.e_ushort == 200) &&
           (_v1.e_long == -200000) && (_v1.e_ulong == 200000) &&
           (_v1.e_float == 1.234f) && (_v1.e_double == 1.23e4) &&
           (_v1.e_char == 'b') && (_v1.e_boolean == true) &&
           (_v1.e_octet == 20) && comp_0022(_v1.e_any) &&
           (_v1.e_string.equals("def")) &&
           (_v1.e_Object._is_equivalent(target)));
  }

  C_union cons_0017()
  {
    C_union _v1;
    _v1 = new C_union();

    short _v2;
    _v2 = -100;
    _v1.e_short(_v2);
    return (_v1);
  }

  boolean comp_0023(C_union _v1)
  {
    if (_v1.discriminator() != 2)
      return false;
    return (_v1.e_ushort() == 200);
  }

  C_struct cons_0018()
  {
    C_struct _v1;
    _v1 = new C_struct();
    _v1.e_short = -100;
    _v1.e_ushort = 100;
    _v1.e_long = -100000;
    _v1.e_ulong = 100000;
    _v1.e_float = 0.123f;
    _v1.e_double = 0.12e3;
    _v1.e_char = 'a';
    _v1.e_boolean = false;
    _v1.e_octet = 10;
    _v1.e_any = orb.create_any();
    _v1.e_any.insert_string("abc");
    _v1.e_string = "abc";
    _v1.e_Object = target;
    return (_v1);
  }

  boolean comp_0025(org.omg.CORBA.Any _v1)
  {
    int _v2;
    _v2 = _v1.extract_long();
    return (_v2 == -200000);
  }

  boolean comp_0024(C_struct _v1)
  {
    return (true && (_v1.e_short == -200) && (_v1.e_ushort == 200) &&
           (_v1.e_long == -200000) && (_v1.e_ulong == 200000) &&
           (_v1.e_float == 1.234f) && (_v1.e_double == 1.23e4) &&
           (_v1.e_char == 'b') && (_v1.e_boolean == true) &&
           (_v1.e_octet == 20) && comp_0025(_v1.e_any) &&
           (_v1.e_string.equals("def")) &&
           (_v1.e_Object._is_equivalent(target)));
  }

  C_union cons_0019()
  {
    C_union _v1;
    _v1 = new C_union();

    short _v2;
    _v2 = -100;
    _v1.e_short(_v2);
    return (_v1);
  }

  boolean comp_0026(C_union _v1)
  {
    if (_v1.discriminator() != 2)
      return false;
    return (_v1.e_ushort() == 200);
  }

  E_struct cons_0021()
  {
    E_struct _v1;
    _v1 = new E_struct();
    _v1.e_b1 = B.b1;
    _v1.e_b2 = B.b1;
    return (_v1);
  }

  E_union cons_0022()
  {
    E_union _v1;
    _v1 = new E_union();

    B _v2;
    _v2 = B.b1;
    _v1.e_b1(_v2);
    return (_v1);
  }

  G_struct cons_0020()
  {
    G_struct _v1;
    _v1 = new G_struct();
    _v1.e_e_struct = cons_0021();
    _v1.e_e_union = cons_0022();
    _v1.e_e_sequence = new B[ 2 ];
    _v1.e_e_sequence [ 0 ] = B.b1;
    _v1.e_e_sequence [ 1 ] = B.b1;
    _v1.e_e_array = new B[ 2 ];
    _v1.e_e_array [ 0 ] = B.b1;
    _v1.e_e_array [ 1 ] = B.b1;
    return (_v1);
  }

  boolean comp_0028(E_struct _v1)
  {
    return (true && (_v1.e_b1 == B.b2) && (_v1.e_b2 == B.b2));
  }

  boolean comp_0029(E_union _v1)
  {
    if (_v1.discriminator() != 2)
      return false;
    return (_v1.e_b2() == B.b2);
  }

  boolean comp_0027(G_struct _v1)
  {
    return (true && comp_0028(_v1.e_e_struct) && comp_0029(_v1.e_e_union) &&
           (
             true && (_v1.e_e_sequence [ 0 ] == B.b2) &&
             (_v1.e_e_sequence [ 1 ] == B.b2)
           ) &&
           (
             true && (_v1.e_e_array [ 0 ] == B.b2) &&
             (_v1.e_e_array [ 1 ] == B.b2)
           ));
  }

  E_struct cons_0024()
  {
    E_struct _v1;
    _v1 = new E_struct();
    _v1.e_b1 = B.b1;
    _v1.e_b2 = B.b1;
    return (_v1);
  }

  G_union cons_0023()
  {
    G_union _v1;
    _v1 = new G_union();

    E_struct _v2;
    _v2 = cons_0024();
    _v1.e_e_struct(_v2);
    return (_v1);
  }

  boolean comp_0031(E_union _v1)
  {
    if (_v1.discriminator() != 2)
      return false;
    return (_v1.e_b2() == B.b2);
  }

  boolean comp_0030(G_union _v1)
  {
    if (_v1.discriminator() != 2)
      return false;
    return comp_0031(_v1.e_e_union());
  }

  E_struct cons_0025()
  {
    E_struct _v1;
    _v1 = new E_struct();
    _v1.e_b1 = B.b1;
    _v1.e_b2 = B.b1;
    return (_v1);
  }

  boolean comp_0032(E_struct _v1)
  {
    return (true && (_v1.e_b1 == B.b2) && (_v1.e_b2 == B.b2));
  }

  E_union cons_0026()
  {
    E_union _v1;
    _v1 = new E_union();

    B _v2;
    _v2 = B.b1;
    _v1.e_b1(_v2);
    return (_v1);
  }

  boolean comp_0033(E_union _v1)
  {
    if (_v1.discriminator() != 2)
      return false;
    return (_v1.e_b2() == B.b2);
  }

  E_struct cons_0027()
  {
    E_struct _v1;
    _v1 = new E_struct();
    _v1.e_b1 = B.b1;
    _v1.e_b2 = B.b1;
    return (_v1);
  }

  boolean comp_0034(E_struct _v1)
  {
    return (true && (_v1.e_b1 == B.b2) && (_v1.e_b2 == B.b2));
  }

  E_union cons_0028()
  {
    E_union _v1;
    _v1 = new E_union();

    B _v2;
    _v2 = B.b1;
    _v1.e_b1(_v2);
    return (_v1);
  }

  boolean comp_0035(E_union _v1)
  {
    if (_v1.discriminator() != 2)
      return false;
    return (_v1.e_b2() == B.b2);
  }

  C_struct cons_0030()
  {
    C_struct _v1;
    _v1 = new C_struct();
    _v1.e_short = -100;
    _v1.e_ushort = 100;
    _v1.e_long = -100000;
    _v1.e_ulong = 100000;
    _v1.e_float = 0.123f;
    _v1.e_double = 0.12e3;
    _v1.e_char = 'a';
    _v1.e_boolean = false;
    _v1.e_octet = 10;
    _v1.e_any = orb.create_any();
    _v1.e_any.insert_string("abc");
    _v1.e_string = "abc";
    _v1.e_Object = target;
    return (_v1);
  }

  F_union cons_0029()
  {
    F_union _v1;
    _v1 = new F_union();

    C_struct _v2;
    _v2 = cons_0030();
    _v1.e_c_struct(_v2);
    return (_v1);
  }

  boolean comp_0037(C_union _v1)
  {
    if (_v1.discriminator() != 2)
      return false;
    return (_v1.e_ushort() == 200);
  }

  boolean comp_0036(F_union _v1)
  {
    if (_v1.discriminator() != 2)
      return false;
    return comp_0037(_v1.e_c_union());
  }

  C_struct cons_0032()
  {
    C_struct _v1;
    _v1 = new C_struct();
    _v1.e_short = -100;
    _v1.e_ushort = 100;
    _v1.e_long = -100000;
    _v1.e_ulong = 100000;
    _v1.e_float = 0.123f;
    _v1.e_double = 0.12e3;
    _v1.e_char = 'a';
    _v1.e_boolean = false;
    _v1.e_octet = 10;
    _v1.e_any = orb.create_any();
    _v1.e_any.insert_string("abc");
    _v1.e_string = "abc";
    _v1.e_Object = target;
    return (_v1);
  }

  F_union cons_0031()
  {
    F_union _v1;
    _v1 = new F_union();

    C_struct _v2;
    _v2 = cons_0032();
    _v1.e_c_struct(_v2);
    return (_v1);
  }

  boolean comp_0039(C_union _v1)
  {
    if (_v1.discriminator() != 2)
      return false;
    return (_v1.e_ushort() == 200);
  }

  boolean comp_0038(F_union _v1)
  {
    if (_v1.discriminator() != 2)
      return false;
    return comp_0039(_v1.e_c_union());
  }

  C_struct cons_0034()
  {
    C_struct _v1;
    _v1 = new C_struct();
    _v1.e_short = -100;
    _v1.e_ushort = 100;
    _v1.e_long = -100000;
    _v1.e_ulong = 100000;
    _v1.e_float = 0.123f;
    _v1.e_double = 0.12e3;
    _v1.e_char = 'a';
    _v1.e_boolean = false;
    _v1.e_octet = 10;
    _v1.e_any = orb.create_any();
    _v1.e_any.insert_string("abc");
    _v1.e_string = "abc";
    _v1.e_Object = target;
    return (_v1);
  }

  F_union cons_0033()
  {
    F_union _v1;
    _v1 = new F_union();

    C_struct _v2;
    _v2 = cons_0034();
    _v1.e_c_struct(_v2);
    return (_v1);
  }

  boolean comp_0041(C_union _v1)
  {
    if (_v1.discriminator() != 2)
      return false;
    return (_v1.e_ushort() == 200);
  }

  boolean comp_0040(F_union _v1)
  {
    if (_v1.discriminator() != 2)
      return false;
    return comp_0041(_v1.e_c_union());
  }

  boolean comp_0042(A_except1 _v1)
  {
    return (true && (_v1.v1 == -200) && (_v1.v2 == 200) && (_v1.v3 == -200000) &&
           (_v1.v4 == 200000) && (_v1.v5 == 1.234f) && (_v1.v6 == 1.23e4) &&
           (_v1.v7 == 'b') && (_v1.v8 == true) && (_v1.v9 == 20));
  }

  boolean comp_0044(org.omg.CORBA.Any _v1)
  {
    int _v2;
    _v2 = _v1.extract_long();
    return (_v2 == -200000);
  }

  boolean comp_0043(A_except2 _v1)
  {
    return (true && comp_0044(_v1.v10) && (_v1.v11.equals("def")) &&
           (_v1.v12._is_equivalent(target)));
  }

  boolean comp_0045(B_except _v1)
  {
    return (true && (_v1.v == B.b2));
  }

  boolean comp_0048(org.omg.CORBA.Any _v1)
  {
    int _v2;
    _v2 = _v1.extract_long();
    return (_v2 == -200000);
  }

  boolean comp_0047(C_struct _v1)
  {
    return (true && (_v1.e_short == -200) && (_v1.e_ushort == 200) &&
           (_v1.e_long == -200000) && (_v1.e_ulong == 200000) &&
           (_v1.e_float == 1.234f) && (_v1.e_double == 1.23e4) &&
           (_v1.e_char == 'b') && (_v1.e_boolean == true) &&
           (_v1.e_octet == 20) && comp_0048(_v1.e_any) &&
           (_v1.e_string.equals("def")) &&
           (_v1.e_Object._is_equivalent(target)));
  }

  boolean comp_0049(C_union _v1)
  {
    if (_v1.discriminator() != 2)
      return false;
    return (_v1.e_ushort() == 200);
  }

  boolean comp_0046(C_except _v1)
  {
    return (true && comp_0047(_v1.v1) && comp_0049(_v1.v2) &&
           comp_0049(_v1.v3) && comp_0049(_v1.v4) && comp_0049(_v1.v5) &&
           comp_0049(_v1.v6) && comp_0049(_v1.v7) && comp_0049(_v1.v8) &&
           comp_0049(_v1.v9) && comp_0049(_v1.v10) && comp_0049(_v1.v11) &&
           comp_0049(_v1.v12) && comp_0049(_v1.v13) &&
           (true && (_v1.v16 [ 0 ] == -200) && (_v1.v16 [ 1 ] == -200)) &&
           (true && (_v1.v17 [ 0 ] == 200) && (_v1.v17 [ 1 ] == 200)) &&
           (true && (_v1.v18 [ 0 ] == -200000) && (_v1.v18 [ 1 ] == -200000)) &&
           (true && (_v1.v19 [ 0 ] == 200000) && (_v1.v19 [ 1 ] == 200000)) &&
           (true && (_v1.v20 [ 0 ] == 1.234f) && (_v1.v20 [ 1 ] == 1.234f)) &&
           (true && (_v1.v21 [ 0 ] == 1.23e4) && (_v1.v21 [ 1 ] == 1.23e4)) &&
           (true && (_v1.v22 [ 0 ] == 'b') && (_v1.v22 [ 1 ] == 'b')) &&
           (true && (_v1.v23 [ 0 ] == true) && (_v1.v23 [ 1 ] == true)) &&
           (true && (_v1.v24 [ 0 ] == 20) && (_v1.v24 [ 1 ] == 20)) &&
           (true && comp_0048(_v1.v25 [ 0 ]) && comp_0048(_v1.v25 [ 1 ])) &&
           (
             true && (_v1.v26 [ 0 ].equals("def")) &&
             (_v1.v26 [ 1 ].equals("def"))
           ) &&
           (
             true && (_v1.v27 [ 0 ]._is_equivalent(target)) &&
             (_v1.v27 [ 1 ]._is_equivalent(target))
           ) && (true && (_v1.v30 [ 0 ] == -200) && (_v1.v30 [ 1 ] == -200)) &&
           (true && (_v1.v31 [ 0 ] == 200) && (_v1.v31 [ 1 ] == 200)) &&
           (true && (_v1.v32 [ 0 ] == -200000) && (_v1.v32 [ 1 ] == -200000)) &&
           (true && (_v1.v33 [ 0 ] == 200000) && (_v1.v33 [ 1 ] == 200000)) &&
           (true && (_v1.v34 [ 0 ] == 1.234f) && (_v1.v34 [ 1 ] == 1.234f)) &&
           (true && (_v1.v35 [ 0 ] == 1.23e4) && (_v1.v35 [ 1 ] == 1.23e4)) &&
           (true && (_v1.v36 [ 0 ] == 'b') && (_v1.v36 [ 1 ] == 'b')) &&
           (true && (_v1.v37 [ 0 ] == true) && (_v1.v37 [ 1 ] == true)) &&
           (true && (_v1.v38 [ 0 ] == 20) && (_v1.v38 [ 1 ] == 20)) &&
           (true && comp_0048(_v1.v39 [ 0 ]) && comp_0048(_v1.v39 [ 1 ])) &&
           (
             true && (_v1.v40 [ 0 ].equals("def")) &&
             (_v1.v40 [ 1 ].equals("def"))
           ) &&
           (
             true && (_v1.v41 [ 0 ]._is_equivalent(target)) &&
             (_v1.v41 [ 1 ]._is_equivalent(target))
           ));
  }

  boolean comp_0051(D_d_short _v1)
  {
    if (_v1.discriminator() != 2)
      return false;
    return (_v1.l2() == -200000);
  }

  boolean comp_0052(D_d_ushort _v1)
  {
    if (_v1.discriminator() != 2)
      return false;
    return (_v1.l2() == -200000);
  }

  boolean comp_0053(D_d_long _v1)
  {
    if (_v1.discriminator() != 2)
      return false;
    return (_v1.l2() == -200000);
  }

  boolean comp_0054(D_d_ulong _v1)
  {
    if (_v1.discriminator() != 2)
      return false;
    return (_v1.l2() == -200000);
  }

  boolean comp_0055(D_d_char _v1)
  {
    if (_v1.discriminator() != 'b')
      return false;
    return (_v1.l2() == -200000);
  }

  boolean comp_0056(D_d_boolean _v1)
  {
    if (_v1.discriminator() != true)
      return false;
    return (_v1.l1() == -200000);
  }

  boolean comp_0057(D_d_B _v1)
  {
    if (_v1.discriminator() != B.b2)
      return false;
    return (_v1.l2() == -200000);
  }

  boolean comp_0050(D_except _v1)
  {
    return (true && comp_0051(_v1.v1) && comp_0052(_v1.v2) &&
           comp_0053(_v1.v3) && comp_0054(_v1.v4) && comp_0055(_v1.v5) &&
           comp_0056(_v1.v6) && comp_0057(_v1.v7));
  }

  boolean comp_0059(E_struct _v1)
  {
    return (true && (_v1.e_b1 == B.b2) && (_v1.e_b2 == B.b2));
  }

  boolean comp_0060(E_union _v1)
  {
    if (_v1.discriminator() != 2)
      return false;
    return (_v1.e_b2() == B.b2);
  }

  boolean comp_0058(E_except _v1)
  {
    return (true && comp_0059(_v1.v1) && comp_0060(_v1.v2) &&
           (true && (_v1.v3 [ 0 ] == B.b2) && (_v1.v3 [ 1 ] == B.b2)) &&
           (true && (_v1.v4 [ 0 ] == B.b2) && (_v1.v4 [ 1 ] == B.b2)));
  }

  boolean comp_0064(org.omg.CORBA.Any _v1)
  {
    int _v2;
    _v2 = _v1.extract_long();
    return (_v2 == -200000);
  }

  boolean comp_0063(C_struct _v1)
  {
    return (true && (_v1.e_short == -200) && (_v1.e_ushort == 200) &&
           (_v1.e_long == -200000) && (_v1.e_ulong == 200000) &&
           (_v1.e_float == 1.234f) && (_v1.e_double == 1.23e4) &&
           (_v1.e_char == 'b') && (_v1.e_boolean == true) &&
           (_v1.e_octet == 20) && comp_0064(_v1.e_any) &&
           (_v1.e_string.equals("def")) &&
           (_v1.e_Object._is_equivalent(target)));
  }

  boolean comp_0065(C_union _v1)
  {
    if (_v1.discriminator() != 2)
      return false;
    return (_v1.e_ushort() == 200);
  }

  boolean comp_0062(F_struct _v1)
  {
    return (true && comp_0063(_v1.e_c_struct) && comp_0065(_v1.e_c_union) &&
           (
             true && (_v1.e_c_sequence_e_short [ 0 ] == -200) &&
             (_v1.e_c_sequence_e_short [ 1 ] == -200)
           ) &&
           (
             true && (_v1.e_c_sequence_e_ushort [ 0 ] == 200) &&
             (_v1.e_c_sequence_e_ushort [ 1 ] == 200)
           ) &&
           (
             true && (_v1.e_c_sequence_e_long [ 0 ] == -200000) &&
             (_v1.e_c_sequence_e_long [ 1 ] == -200000)
           ) &&
           (
             true && (_v1.e_c_sequence_e_ulong [ 0 ] == 200000) &&
             (_v1.e_c_sequence_e_ulong [ 1 ] == 200000)
           ) &&
           (
             true && (_v1.e_c_sequence_e_float [ 0 ] == 1.234f) &&
             (_v1.e_c_sequence_e_float [ 1 ] == 1.234f)
           ) &&
           (
             true && (_v1.e_c_sequence_e_double [ 0 ] == 1.23e4) &&
             (_v1.e_c_sequence_e_double [ 1 ] == 1.23e4)
           ) &&
           (
             true && (_v1.e_c_sequence_e_char [ 0 ] == 'b') &&
             (_v1.e_c_sequence_e_char [ 1 ] == 'b')
           ) &&
           (
             true && (_v1.e_c_sequence_e_boolean [ 0 ] == true) &&
             (_v1.e_c_sequence_e_boolean [ 1 ] == true)
           ) &&
           (
             true && (_v1.e_c_sequence_e_octet [ 0 ] == 20) &&
             (_v1.e_c_sequence_e_octet [ 1 ] == 20)
           ) &&
           (
             true && comp_0064(_v1.e_c_sequence_e_any [ 0 ]) &&
             comp_0064(_v1.e_c_sequence_e_any [ 1 ])
           ) &&
           (
             true && (_v1.e_c_sequence_e_string [ 0 ].equals("def")) &&
             (_v1.e_c_sequence_e_string [ 1 ].equals("def"))
           ) &&
           (
             true && (_v1.e_c_sequence_e_Object [ 0 ]._is_equivalent(target)) &&
             (_v1.e_c_sequence_e_Object [ 1 ]._is_equivalent(target))
           ) &&
           (
             true && (_v1.e_c_array_e_short [ 0 ] == -200) &&
             (_v1.e_c_array_e_short [ 1 ] == -200)
           ) &&
           (
             true && (_v1.e_c_array_e_ushort [ 0 ] == 200) &&
             (_v1.e_c_array_e_ushort [ 1 ] == 200)
           ) &&
           (
             true && (_v1.e_c_array_e_long [ 0 ] == -200000) &&
             (_v1.e_c_array_e_long [ 1 ] == -200000)
           ) &&
           (
             true && (_v1.e_c_array_e_ulong [ 0 ] == 200000) &&
             (_v1.e_c_array_e_ulong [ 1 ] == 200000)
           ) &&
           (
             true && (_v1.e_c_array_e_float [ 0 ] == 1.234f) &&
             (_v1.e_c_array_e_float [ 1 ] == 1.234f)
           ) &&
           (
             true && (_v1.e_c_array_e_double [ 0 ] == 1.23e4) &&
             (_v1.e_c_array_e_double [ 1 ] == 1.23e4)
           ) &&
           (
             true && (_v1.e_c_array_e_char [ 0 ] == 'b') &&
             (_v1.e_c_array_e_char [ 1 ] == 'b')
           ) &&
           (
             true && (_v1.e_c_array_e_boolean [ 0 ] == true) &&
             (_v1.e_c_array_e_boolean [ 1 ] == true)
           ) &&
           (
             true && (_v1.e_c_array_e_octet [ 0 ] == 20) &&
             (_v1.e_c_array_e_octet [ 1 ] == 20)
           ) &&
           (
             true && comp_0064(_v1.e_c_array_e_any [ 0 ]) &&
             comp_0064(_v1.e_c_array_e_any [ 1 ])
           ) &&
           (
             true && (_v1.e_c_array_e_string [ 0 ].equals("def")) &&
             (_v1.e_c_array_e_string [ 1 ].equals("def"))
           ) &&
           (
             true && (_v1.e_c_array_e_Object [ 0 ]._is_equivalent(target)) &&
             (_v1.e_c_array_e_Object [ 1 ]._is_equivalent(target))
           ));
  }

  boolean comp_0066(F_union _v1)
  {
    if (_v1.discriminator() != 2)
      return false;
    return comp_0065(_v1.e_c_union());
  }

  boolean comp_0061(F_except1 _v1)
  {
    return (true && comp_0062(_v1.v1) && comp_0066(_v1.v2) &&
           comp_0066(_v1.v3) && comp_0066(_v1.v4) && comp_0066(_v1.v5) &&
           comp_0066(_v1.v6) && comp_0066(_v1.v7) && comp_0066(_v1.v8) &&
           comp_0066(_v1.v9) && comp_0066(_v1.v10) && comp_0066(_v1.v11) &&
           comp_0066(_v1.v12) && comp_0066(_v1.v13) && comp_0066(_v1.v14) &&
           comp_0066(_v1.v15) && comp_0066(_v1.v18) && comp_0066(_v1.v19) &&
           comp_0066(_v1.v20) && comp_0066(_v1.v21) && comp_0066(_v1.v22) &&
           comp_0066(_v1.v23) && comp_0066(_v1.v24) && comp_0066(_v1.v25) &&
           comp_0066(_v1.v26) && comp_0066(_v1.v27) && comp_0066(_v1.v28) &&
           comp_0066(_v1.v29));
  }

  boolean comp_0069(org.omg.CORBA.Any _v1)
  {
    int _v2;
    _v2 = _v1.extract_long();
    return (_v2 == -200000);
  }

  boolean comp_0068(C_struct _v1)
  {
    return (true && (_v1.e_short == -200) && (_v1.e_ushort == 200) &&
           (_v1.e_long == -200000) && (_v1.e_ulong == 200000) &&
           (_v1.e_float == 1.234f) && (_v1.e_double == 1.23e4) &&
           (_v1.e_char == 'b') && (_v1.e_boolean == true) &&
           (_v1.e_octet == 20) && comp_0069(_v1.e_any) &&
           (_v1.e_string.equals("def")) &&
           (_v1.e_Object._is_equivalent(target)));
  }

  boolean comp_0070(C_union _v1)
  {
    if (_v1.discriminator() != 2)
      return false;
    return (_v1.e_ushort() == 200);
  }

  boolean comp_0067(F_except2 _v1)
  {
    return (true &&
           (true && comp_0068(_v1.v32 [ 0 ]) && comp_0068(_v1.v32 [ 1 ])) &&
           (true && comp_0070(_v1.v33 [ 0 ]) && comp_0070(_v1.v33 [ 1 ])));
  }

  boolean comp_0073(org.omg.CORBA.Any _v1)
  {
    int _v2;
    _v2 = _v1.extract_long();
    return (_v2 == -200000);
  }

  boolean comp_0072(C_struct _v1)
  {
    return (true && (_v1.e_short == -200) && (_v1.e_ushort == 200) &&
           (_v1.e_long == -200000) && (_v1.e_ulong == 200000) &&
           (_v1.e_float == 1.234f) && (_v1.e_double == 1.23e4) &&
           (_v1.e_char == 'b') && (_v1.e_boolean == true) &&
           (_v1.e_octet == 20) && comp_0073(_v1.e_any) &&
           (_v1.e_string.equals("def")) &&
           (_v1.e_Object._is_equivalent(target)));
  }

  boolean comp_0074(C_union _v1)
  {
    if (_v1.discriminator() != 2)
      return false;
    return (_v1.e_ushort() == 200);
  }

  boolean comp_0071(F_except3 _v1)
  {
    return (true &&
           (true && comp_0072(_v1.v62 [ 0 ]) && comp_0072(_v1.v62 [ 1 ])) &&
           (true && comp_0074(_v1.v63 [ 0 ]) && comp_0074(_v1.v63 [ 1 ])));
  }

  boolean comp_0077(E_struct _v1)
  {
    return (true && (_v1.e_b1 == B.b2) && (_v1.e_b2 == B.b2));
  }

  boolean comp_0078(E_union _v1)
  {
    if (_v1.discriminator() != 2)
      return false;
    return (_v1.e_b2() == B.b2);
  }

  boolean comp_0076(G_struct _v1)
  {
    return (true && comp_0077(_v1.e_e_struct) && comp_0078(_v1.e_e_union) &&
           (
             true && (_v1.e_e_sequence [ 0 ] == B.b2) &&
             (_v1.e_e_sequence [ 1 ] == B.b2)
           ) &&
           (
             true && (_v1.e_e_array [ 0 ] == B.b2) &&
             (_v1.e_e_array [ 1 ] == B.b2)
           ));
  }

  boolean comp_0079(G_union _v1)
  {
    if (_v1.discriminator() != 2)
      return false;
    return comp_0078(_v1.e_e_union());
  }

  boolean comp_0075(G_except _v1)
  {
    return (true && comp_0076(_v1.v1) && comp_0079(_v1.v2) &&
           comp_0079(_v1.v3) && comp_0079(_v1.v4) && comp_0079(_v1.v5) &&
           (true && comp_0077(_v1.v6 [ 0 ]) && comp_0077(_v1.v6 [ 1 ])) &&
           (true && comp_0078(_v1.v7 [ 0 ]) && comp_0078(_v1.v7 [ 1 ])) &&
           (true && comp_0077(_v1.v10 [ 0 ]) && comp_0077(_v1.v10 [ 1 ])) &&
           (true && comp_0078(_v1.v11 [ 0 ]) && comp_0078(_v1.v11 [ 1 ])));
  }

  //operator definitions
  void call_op1()
  {
    short argin;
    org.omg.CORBA.ShortHolder argout;
    argout = new org.omg.CORBA.ShortHolder();

    org.omg.CORBA.ShortHolder arginout;
    arginout = new org.omg.CORBA.ShortHolder();

    short _ret;
    argin = -100;
    arginout.value = -100;
    try
      {
        _ret = target.op1(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op1");
        return;
      }
    if (!(_ret == -200))
      {
        fail("_ret value error in op1");
      }
    if (!(argout.value == -200))
      {
        fail("argout value error in op1");
      }
    if (!(arginout.value == -200))
      {
        fail("arginout value error in op1");
      }
  }

  void call_op2()
  {
    short argin;
    org.omg.CORBA.ShortHolder argout;
    argout = new org.omg.CORBA.ShortHolder();

    org.omg.CORBA.ShortHolder arginout;
    arginout = new org.omg.CORBA.ShortHolder();

    short _ret;
    argin = 100;
    arginout.value = 100;
    try
      {
        _ret = target.op2(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op2");
        return;
      }
    if (!(_ret == 200))
      {
        fail("_ret value error in op2");
      }
    if (!(argout.value == 200))
      {
        fail("argout value error in op2");
      }
    if (!(arginout.value == 200))
      {
        fail("arginout value error in op2");
      }
  }

  void call_op3()
  {
    int argin;
    org.omg.CORBA.IntHolder argout;
    argout = new org.omg.CORBA.IntHolder();

    org.omg.CORBA.IntHolder arginout;
    arginout = new org.omg.CORBA.IntHolder();

    int _ret;
    argin = -100000;
    arginout.value = -100000;
    try
      {
        _ret = target.op3(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op3");
        return;
      }
    if (!(_ret == -200000))
      {
        fail("_ret value error in op3");
      }
    if (!(argout.value == -200000))
      {
        fail("argout value error in op3");
      }
    if (!(arginout.value == -200000))
      {
        fail("arginout value error in op3");
      }
  }

  void call_op4()
  {
    int argin;
    org.omg.CORBA.IntHolder argout;
    argout = new org.omg.CORBA.IntHolder();

    org.omg.CORBA.IntHolder arginout;
    arginout = new org.omg.CORBA.IntHolder();

    int _ret;
    argin = 100000;
    arginout.value = 100000;
    try
      {
        _ret = target.op4(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op4");
        return;
      }
    if (!(_ret == 200000))
      {
        fail("_ret value error in op4");
      }
    if (!(argout.value == 200000))
      {
        fail("argout value error in op4");
      }
    if (!(arginout.value == 200000))
      {
        fail("arginout value error in op4");
      }
  }

  void call_op5()
  {
    float argin;
    org.omg.CORBA.FloatHolder argout;
    argout = new org.omg.CORBA.FloatHolder();

    org.omg.CORBA.FloatHolder arginout;
    arginout = new org.omg.CORBA.FloatHolder();

    float _ret;
    argin = 0.123f;
    arginout.value = 0.123f;
    try
      {
        _ret = target.op5(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op5");
        return;
      }
    if (!(_ret == 1.234f))
      {
        fail("_ret value error in op5");
      }
    if (!(argout.value == 1.234f))
      {
        fail("argout value error in op5");
      }
    if (!(arginout.value == 1.234f))
      {
        fail("arginout value error in op5");
      }
  }

  void call_op6()
  {
    double argin;
    org.omg.CORBA.DoubleHolder argout;
    argout = new org.omg.CORBA.DoubleHolder();

    org.omg.CORBA.DoubleHolder arginout;
    arginout = new org.omg.CORBA.DoubleHolder();

    double _ret;
    argin = 0.12e3;
    arginout.value = 0.12e3;
    try
      {
        _ret = target.op6(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op6");
        return;
      }
    if (!(_ret == 1.23e4))
      {
        fail("_ret value error in op6");
      }
    if (!(argout.value == 1.23e4))
      {
        fail("argout value error in op6");
      }
    if (!(arginout.value == 1.23e4))
      {
        fail("arginout value error in op6");
      }
  }

  void call_op7()
  {
    char argin;
    org.omg.CORBA.CharHolder argout;
    argout = new org.omg.CORBA.CharHolder();

    org.omg.CORBA.CharHolder arginout;
    arginout = new org.omg.CORBA.CharHolder();

    char _ret;
    argin = 'a';
    arginout.value = 'a';
    try
      {
        _ret = target.op7(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op7");
        return;
      }
    if (!(_ret == 'b'))
      {
        fail("_ret value error in op7");
      }
    if (!(argout.value == 'b'))
      {
        fail("argout value error in op7");
      }
    if (!(arginout.value == 'b'))
      {
        fail("arginout value error in op7");
      }
  }

  void call_op8()
  {
    boolean argin;
    org.omg.CORBA.BooleanHolder argout;
    argout = new org.omg.CORBA.BooleanHolder();

    org.omg.CORBA.BooleanHolder arginout;
    arginout = new org.omg.CORBA.BooleanHolder();

    boolean _ret;
    argin = false;
    arginout.value = false;
    try
      {
        _ret = target.op8(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op8");
        return;
      }
    if (!(_ret == true))
      {
        fail("_ret value error in op8");
      }
    if (!(argout.value == true))
      {
        fail("argout value error in op8");
      }
    if (!(arginout.value == true))
      {
        fail("arginout value error in op8");
      }
  }

  void call_op9()
  {
    byte argin;
    org.omg.CORBA.ByteHolder argout;
    argout = new org.omg.CORBA.ByteHolder();

    org.omg.CORBA.ByteHolder arginout;
    arginout = new org.omg.CORBA.ByteHolder();

    byte _ret;
    argin = 10;
    arginout.value = 10;
    try
      {
        _ret = target.op9(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op9");
        return;
      }
    if (!(_ret == 20))
      {
        fail("_ret value error in op9");
      }
    if (!(argout.value == 20))
      {
        fail("argout value error in op9");
      }
    if (!(arginout.value == 20))
      {
        fail("arginout value error in op9");
      }
  }

  void call_op15()
  {
    B argin;
    BHolder argout;
    argout = new BHolder();

    BHolder arginout;
    arginout = new BHolder();

    B _ret;
    argin = B.b1;
    arginout.value = B.b1;
    try
      {
        _ret = target.op15(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op15");
        return;
      }
    if (!(_ret == B.b2))
      {
        fail("_ret value error in op15");
      }
    if (!(argout.value == B.b2))
      {
        fail("argout value error in op15");
      }
    if (!(arginout.value == B.b2))
      {
        fail("arginout value error in op15");
      }
  }

  void call_op17()
  {
    C_union argin;
    C_unionHolder argout;
    argout = new C_unionHolder();

    C_unionHolder arginout;
    arginout = new C_unionHolder();

    C_union _ret;
    argin = cons_0001();
    arginout.value = cons_0001();
    try
      {
        _ret = target.op17(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op17");
        return;
      }
    if (!comp_0003(_ret))
      {
        fail("_ret value error in op17");
      }
    if (!comp_0003(argout.value))
      {
        fail("argout value error in op17");
      }
    if (!comp_0003(arginout.value))
      {
        fail("arginout value error in op17");
      }
  }

  void call_op18()
  {
    short[] argin;
    C_sequence_e_shortHolder argout;
    argout = new C_sequence_e_shortHolder();

    C_sequence_e_shortHolder arginout;
    arginout = new C_sequence_e_shortHolder();

    short[] _ret;
    argin = new short[ 2 ];
    argin [ 0 ] = -100;
    argin [ 1 ] = -100;
    arginout.value = new short[ 2 ];
    arginout.value [ 0 ] = -100;
    arginout.value [ 1 ] = -100;
    try
      {
        _ret = target.op18(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op18");
        return;
      }
    if (!(true && (_ret [ 0 ] == -200) && (_ret [ 1 ] == -200)))
      {
        fail("_ret value error in op18");
      }
    if (!(
          true && (argout.value [ 0 ] == -200) && (argout.value [ 1 ] == -200)
        ))
      {
        fail("argout value error in op18");
      }
    if (!(
          true && (arginout.value [ 0 ] == -200) &&
          (arginout.value [ 1 ] == -200)
        )
       )
      {
        fail("arginout value error in op18");
      }
  }

  void call_op19()
  {
    short[] argin;
    C_sequence_e_ushortHolder argout;
    argout = new C_sequence_e_ushortHolder();

    C_sequence_e_ushortHolder arginout;
    arginout = new C_sequence_e_ushortHolder();

    short[] _ret;
    argin = new short[ 2 ];
    argin [ 0 ] = 100;
    argin [ 1 ] = 100;
    arginout.value = new short[ 2 ];
    arginout.value [ 0 ] = 100;
    arginout.value [ 1 ] = 100;
    try
      {
        _ret = target.op19(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op19");
        return;
      }
    if (!(true && (_ret [ 0 ] == 200) && (_ret [ 1 ] == 200)))
      {
        fail("_ret value error in op19");
      }
    if (!(true && (argout.value [ 0 ] == 200) && (argout.value [ 1 ] == 200)))
      {
        fail("argout value error in op19");
      }
    if (!(
          true && (arginout.value [ 0 ] == 200) &&
          (arginout.value [ 1 ] == 200)
        )
       )
      {
        fail("arginout value error in op19");
      }
  }

  void call_op20()
  {
    int[] argin;
    C_sequence_e_longHolder argout;
    argout = new C_sequence_e_longHolder();

    C_sequence_e_longHolder arginout;
    arginout = new C_sequence_e_longHolder();

    int[] _ret;
    argin = new int[ 2 ];
    argin [ 0 ] = -100000;
    argin [ 1 ] = -100000;
    arginout.value = new int[ 2 ];
    arginout.value [ 0 ] = -100000;
    arginout.value [ 1 ] = -100000;
    try
      {
        _ret = target.op20(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op20");
        return;
      }
    if (!(true && (_ret [ 0 ] == -200000) && (_ret [ 1 ] == -200000)))
      {
        fail("_ret value error in op20");
      }
    if (!(
          true && (argout.value [ 0 ] == -200000) &&
          (argout.value [ 1 ] == -200000)
        )
       )
      {
        fail("argout value error in op20");
      }
    if (!(
          true && (arginout.value [ 0 ] == -200000) &&
          (arginout.value [ 1 ] == -200000)
        )
       )
      {
        fail("arginout value error in op20");
      }
  }

  void call_op21()
  {
    int[] argin;
    C_sequence_e_ulongHolder argout;
    argout = new C_sequence_e_ulongHolder();

    C_sequence_e_ulongHolder arginout;
    arginout = new C_sequence_e_ulongHolder();

    int[] _ret;
    argin = new int[ 2 ];
    argin [ 0 ] = 100000;
    argin [ 1 ] = 100000;
    arginout.value = new int[ 2 ];
    arginout.value [ 0 ] = 100000;
    arginout.value [ 1 ] = 100000;
    try
      {
        _ret = target.op21(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op21");
        return;
      }
    if (!(true && (_ret [ 0 ] == 200000) && (_ret [ 1 ] == 200000)))
      {
        fail("_ret value error in op21");
      }
    if (!(
          true && (argout.value [ 0 ] == 200000) &&
          (argout.value [ 1 ] == 200000)
        )
       )
      {
        fail("argout value error in op21");
      }
    if (!(
          true && (arginout.value [ 0 ] == 200000) &&
          (arginout.value [ 1 ] == 200000)
        )
       )
      {
        fail("arginout value error in op21");
      }
  }

  void call_op22()
  {
    float[] argin;
    C_sequence_e_floatHolder argout;
    argout = new C_sequence_e_floatHolder();

    C_sequence_e_floatHolder arginout;
    arginout = new C_sequence_e_floatHolder();

    float[] _ret;
    argin = new float[ 2 ];
    argin [ 0 ] = 0.123f;
    argin [ 1 ] = 0.123f;
    arginout.value = new float[ 2 ];
    arginout.value [ 0 ] = 0.123f;
    arginout.value [ 1 ] = 0.123f;
    try
      {
        _ret = target.op22(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op22");
        return;
      }
    if (!(true && (_ret [ 0 ] == 1.234f) && (_ret [ 1 ] == 1.234f)))
      {
        fail("_ret value error in op22");
      }
    if (!(
          true && (argout.value [ 0 ] == 1.234f) &&
          (argout.value [ 1 ] == 1.234f)
        )
       )
      {
        fail("argout value error in op22");
      }
    if (!(
          true && (arginout.value [ 0 ] == 1.234f) &&
          (arginout.value [ 1 ] == 1.234f)
        )
       )
      {
        fail("arginout value error in op22");
      }
  }

  void call_op23()
  {
    double[] argin;
    C_sequence_e_doubleHolder argout;
    argout = new C_sequence_e_doubleHolder();

    C_sequence_e_doubleHolder arginout;
    arginout = new C_sequence_e_doubleHolder();

    double[] _ret;
    argin = new double[ 2 ];
    argin [ 0 ] = 0.12e3;
    argin [ 1 ] = 0.12e3;
    arginout.value = new double[ 2 ];
    arginout.value [ 0 ] = 0.12e3;
    arginout.value [ 1 ] = 0.12e3;
    try
      {
        _ret = target.op23(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op23");
        return;
      }
    if (!(true && (_ret [ 0 ] == 1.23e4) && (_ret [ 1 ] == 1.23e4)))
      {
        fail("_ret value error in op23");
      }
    if (!(
          true && (argout.value [ 0 ] == 1.23e4) &&
          (argout.value [ 1 ] == 1.23e4)
        )
       )
      {
        fail("argout value error in op23");
      }
    if (!(
          true && (arginout.value [ 0 ] == 1.23e4) &&
          (arginout.value [ 1 ] == 1.23e4)
        )
       )
      {
        fail("arginout value error in op23");
      }
  }

  void call_op24()
  {
    char[] argin;
    C_sequence_e_charHolder argout;
    argout = new C_sequence_e_charHolder();

    C_sequence_e_charHolder arginout;
    arginout = new C_sequence_e_charHolder();

    char[] _ret;
    argin = new char[ 2 ];
    argin [ 0 ] = 'a';
    argin [ 1 ] = 'a';
    arginout.value = new char[ 2 ];
    arginout.value [ 0 ] = 'a';
    arginout.value [ 1 ] = 'a';
    try
      {
        _ret = target.op24(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op24");
        return;
      }
    if (!(true && (_ret [ 0 ] == 'b') && (_ret [ 1 ] == 'b')))
      {
        fail("_ret value error in op24");
      }
    if (!(true && (argout.value [ 0 ] == 'b') && (argout.value [ 1 ] == 'b')))
      {
        fail("argout value error in op24");
      }
    if (!(
          true && (arginout.value [ 0 ] == 'b') &&
          (arginout.value [ 1 ] == 'b')
        )
       )
      {
        fail("arginout value error in op24");
      }
  }

  void call_op25()
  {
    boolean[] argin;
    C_sequence_e_booleanHolder argout;
    argout = new C_sequence_e_booleanHolder();

    C_sequence_e_booleanHolder arginout;
    arginout = new C_sequence_e_booleanHolder();

    boolean[] _ret;
    argin = new boolean[ 2 ];
    argin [ 0 ] = false;
    argin [ 1 ] = false;
    arginout.value = new boolean[ 2 ];
    arginout.value [ 0 ] = false;
    arginout.value [ 1 ] = false;
    try
      {
        _ret = target.op25(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op25");
        return;
      }
    if (!(true && (_ret [ 0 ] == true) && (_ret [ 1 ] == true)))
      {
        fail("_ret value error in op25");
      }
    if (!(
          true && (argout.value [ 0 ] == true) && (argout.value [ 1 ] == true)
        ))
      {
        fail("argout value error in op25");
      }
    if (!(
          true && (arginout.value [ 0 ] == true) &&
          (arginout.value [ 1 ] == true)
        )
       )
      {
        fail("arginout value error in op25");
      }
  }

  void call_op26()
  {
    byte[] argin;
    C_sequence_e_octetHolder argout;
    argout = new C_sequence_e_octetHolder();

    C_sequence_e_octetHolder arginout;
    arginout = new C_sequence_e_octetHolder();

    byte[] _ret;
    argin = new byte[ 2 ];
    argin [ 0 ] = 10;
    argin [ 1 ] = 10;
    arginout.value = new byte[ 2 ];
    arginout.value [ 0 ] = 10;
    arginout.value [ 1 ] = 10;
    try
      {
        _ret = target.op26(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op26");
        return;
      }
    if (!(true && (_ret [ 0 ] == 20) && (_ret [ 1 ] == 20)))
      {
        fail("_ret value error in op26");
      }
    if (!(true && (argout.value [ 0 ] == 20) && (argout.value [ 1 ] == 20)))
      {
        fail("argout value error in op26");
      }
    if (!(
          true && (arginout.value [ 0 ] == 20) && (arginout.value [ 1 ] == 20)
        ))
      {
        fail("arginout value error in op26");
      }
  }

  void call_op28()
  {
    String[] argin;
    C_sequence_e_stringHolder argout;
    argout = new C_sequence_e_stringHolder();

    C_sequence_e_stringHolder arginout;
    arginout = new C_sequence_e_stringHolder();

    String[] _ret;
    argin = new String[ 2 ];
    argin [ 0 ] = "abc";
    argin [ 1 ] = "abc";
    arginout.value = new String[ 2 ];
    arginout.value [ 0 ] = "abc";
    arginout.value [ 1 ] = "abc";
    try
      {
        _ret = target.op28(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op28");
        return;
      }
    if (!(true && (_ret [ 0 ].equals("def")) && (_ret [ 1 ].equals("def"))))
      {
        fail("_ret value error in op28");
      }
    if (!(
          true && (argout.value [ 0 ].equals("def")) &&
          (argout.value [ 1 ].equals("def"))
        )
       )
      {
        fail("argout value error in op28");
      }
    if (!(
          true && (arginout.value [ 0 ].equals("def")) &&
          (arginout.value [ 1 ].equals("def"))
        )
       )
      {
        fail("arginout value error in op28");
      }
  }

  void call_op32()
  {
    short[] argin;
    C_array_e_shortHolder argout;
    argout = new C_array_e_shortHolder();

    C_array_e_shortHolder arginout;
    arginout = new C_array_e_shortHolder();

    short[] _ret;
    argin = new short[ 2 ];
    argin [ 0 ] = -100;
    argin [ 1 ] = -100;
    arginout.value = new short[ 2 ];
    arginout.value [ 0 ] = -100;
    arginout.value [ 1 ] = -100;
    try
      {
        _ret = target.op32(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op32");
        return;
      }
    if (!(true && (_ret [ 0 ] == -200) && (_ret [ 1 ] == -200)))
      {
        fail("_ret value error in op32");
      }
    if (!(
          true && (argout.value [ 0 ] == -200) && (argout.value [ 1 ] == -200)
        ))
      {
        fail("argout value error in op32");
      }
    if (!(
          true && (arginout.value [ 0 ] == -200) &&
          (arginout.value [ 1 ] == -200)
        )
       )
      {
        fail("arginout value error in op32");
      }
  }

  void call_op33()
  {
    short[] argin;
    C_array_e_ushortHolder argout;
    argout = new C_array_e_ushortHolder();

    C_array_e_ushortHolder arginout;
    arginout = new C_array_e_ushortHolder();

    short[] _ret;
    argin = new short[ 2 ];
    argin [ 0 ] = 100;
    argin [ 1 ] = 100;
    arginout.value = new short[ 2 ];
    arginout.value [ 0 ] = 100;
    arginout.value [ 1 ] = 100;
    try
      {
        _ret = target.op33(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op33");
        return;
      }
    if (!(true && (_ret [ 0 ] == 200) && (_ret [ 1 ] == 200)))
      {
        fail("_ret value error in op33");
      }
    if (!(true && (argout.value [ 0 ] == 200) && (argout.value [ 1 ] == 200)))
      {
        fail("argout value error in op33");
      }
    if (!(
          true && (arginout.value [ 0 ] == 200) &&
          (arginout.value [ 1 ] == 200)
        )
       )
      {
        fail("arginout value error in op33");
      }
  }

  void call_op34()
  {
    int[] argin;
    C_array_e_longHolder argout;
    argout = new C_array_e_longHolder();

    C_array_e_longHolder arginout;
    arginout = new C_array_e_longHolder();

    int[] _ret;
    argin = new int[ 2 ];
    argin [ 0 ] = -100000;
    argin [ 1 ] = -100000;
    arginout.value = new int[ 2 ];
    arginout.value [ 0 ] = -100000;
    arginout.value [ 1 ] = -100000;
    try
      {
        _ret = target.op34(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op34");
        return;
      }
    if (!(true && (_ret [ 0 ] == -200000) && (_ret [ 1 ] == -200000)))
      {
        fail("_ret value error in op34");
      }
    if (!(
          true && (argout.value [ 0 ] == -200000) &&
          (argout.value [ 1 ] == -200000)
        )
       )
      {
        fail("argout value error in op34");
      }
    if (!(
          true && (arginout.value [ 0 ] == -200000) &&
          (arginout.value [ 1 ] == -200000)
        )
       )
      {
        fail("arginout value error in op34");
      }
  }

  void call_op35()
  {
    int[] argin;
    C_array_e_ulongHolder argout;
    argout = new C_array_e_ulongHolder();

    C_array_e_ulongHolder arginout;
    arginout = new C_array_e_ulongHolder();

    int[] _ret;
    argin = new int[ 2 ];
    argin [ 0 ] = 100000;
    argin [ 1 ] = 100000;
    arginout.value = new int[ 2 ];
    arginout.value [ 0 ] = 100000;
    arginout.value [ 1 ] = 100000;
    try
      {
        _ret = target.op35(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op35");
        return;
      }
    if (!(true && (_ret [ 0 ] == 200000) && (_ret [ 1 ] == 200000)))
      {
        fail("_ret value error in op35");
      }
    if (!(
          true && (argout.value [ 0 ] == 200000) &&
          (argout.value [ 1 ] == 200000)
        )
       )
      {
        fail("argout value error in op35");
      }
    if (!(
          true && (arginout.value [ 0 ] == 200000) &&
          (arginout.value [ 1 ] == 200000)
        )
       )
      {
        fail("arginout value error in op35");
      }
  }

  void call_op36()
  {
    float[] argin;
    C_array_e_floatHolder argout;
    argout = new C_array_e_floatHolder();

    C_array_e_floatHolder arginout;
    arginout = new C_array_e_floatHolder();

    float[] _ret;
    argin = new float[ 2 ];
    argin [ 0 ] = 0.123f;
    argin [ 1 ] = 0.123f;
    arginout.value = new float[ 2 ];
    arginout.value [ 0 ] = 0.123f;
    arginout.value [ 1 ] = 0.123f;
    try
      {
        _ret = target.op36(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op36");
        return;
      }
    if (!(true && (_ret [ 0 ] == 1.234f) && (_ret [ 1 ] == 1.234f)))
      {
        fail("_ret value error in op36");
      }
    if (!(
          true && (argout.value [ 0 ] == 1.234f) &&
          (argout.value [ 1 ] == 1.234f)
        )
       )
      {
        fail("argout value error in op36");
      }
    if (!(
          true && (arginout.value [ 0 ] == 1.234f) &&
          (arginout.value [ 1 ] == 1.234f)
        )
       )
      {
        fail("arginout value error in op36");
      }
  }

  void call_op37()
  {
    double[] argin;
    C_array_e_doubleHolder argout;
    argout = new C_array_e_doubleHolder();

    C_array_e_doubleHolder arginout;
    arginout = new C_array_e_doubleHolder();

    double[] _ret;
    argin = new double[ 2 ];
    argin [ 0 ] = 0.12e3;
    argin [ 1 ] = 0.12e3;
    arginout.value = new double[ 2 ];
    arginout.value [ 0 ] = 0.12e3;
    arginout.value [ 1 ] = 0.12e3;
    try
      {
        _ret = target.op37(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op37");
        return;
      }
    if (!(true && (_ret [ 0 ] == 1.23e4) && (_ret [ 1 ] == 1.23e4)))
      {
        fail("_ret value error in op37");
      }
    if (!(
          true && (argout.value [ 0 ] == 1.23e4) &&
          (argout.value [ 1 ] == 1.23e4)
        )
       )
      {
        fail("argout value error in op37");
      }
    if (!(
          true && (arginout.value [ 0 ] == 1.23e4) &&
          (arginout.value [ 1 ] == 1.23e4)
        )
       )
      {
        fail("arginout value error in op37");
      }
  }

  void call_op38()
  {
    char[] argin;
    C_array_e_charHolder argout;
    argout = new C_array_e_charHolder();

    C_array_e_charHolder arginout;
    arginout = new C_array_e_charHolder();

    char[] _ret;
    argin = new char[ 2 ];
    argin [ 0 ] = 'a';
    argin [ 1 ] = 'a';
    arginout.value = new char[ 2 ];
    arginout.value [ 0 ] = 'a';
    arginout.value [ 1 ] = 'a';
    try
      {
        _ret = target.op38(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op38");
        return;
      }
    if (!(true && (_ret [ 0 ] == 'b') && (_ret [ 1 ] == 'b')))
      {
        fail("_ret value error in op38");
      }
    if (!(true && (argout.value [ 0 ] == 'b') && (argout.value [ 1 ] == 'b')))
      {
        fail("argout value error in op38");
      }
    if (!(
          true && (arginout.value [ 0 ] == 'b') &&
          (arginout.value [ 1 ] == 'b')
        )
       )
      {
        fail("arginout value error in op38");
      }
  }

  void call_op39()
  {
    boolean[] argin;
    C_array_e_booleanHolder argout;
    argout = new C_array_e_booleanHolder();

    C_array_e_booleanHolder arginout;
    arginout = new C_array_e_booleanHolder();

    boolean[] _ret;
    argin = new boolean[ 2 ];
    argin [ 0 ] = false;
    argin [ 1 ] = false;
    arginout.value = new boolean[ 2 ];
    arginout.value [ 0 ] = false;
    arginout.value [ 1 ] = false;
    try
      {
        _ret = target.op39(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op39");
        return;
      }
    if (!(true && (_ret [ 0 ] == true) && (_ret [ 1 ] == true)))
      {
        fail("_ret value error in op39");
      }
    if (!(
          true && (argout.value [ 0 ] == true) && (argout.value [ 1 ] == true)
        ))
      {
        fail("argout value error in op39");
      }
    if (!(
          true && (arginout.value [ 0 ] == true) &&
          (arginout.value [ 1 ] == true)
        )
       )
      {
        fail("arginout value error in op39");
      }
  }

  void call_op40()
  {
    byte[] argin;
    C_array_e_octetHolder argout;
    argout = new C_array_e_octetHolder();

    C_array_e_octetHolder arginout;
    arginout = new C_array_e_octetHolder();

    byte[] _ret;
    argin = new byte[ 2 ];
    argin [ 0 ] = 10;
    argin [ 1 ] = 10;
    arginout.value = new byte[ 2 ];
    arginout.value [ 0 ] = 10;
    arginout.value [ 1 ] = 10;
    try
      {
        _ret = target.op40(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op40");
        return;
      }
    if (!(true && (_ret [ 0 ] == 20) && (_ret [ 1 ] == 20)))
      {
        fail("_ret value error in op40");
      }
    if (!(true && (argout.value [ 0 ] == 20) && (argout.value [ 1 ] == 20)))
      {
        fail("argout value error in op40");
      }
    if (!(
          true && (arginout.value [ 0 ] == 20) && (arginout.value [ 1 ] == 20)
        ))
      {
        fail("arginout value error in op40");
      }
  }

  void call_op42()
  {
    String[] argin;
    C_array_e_stringHolder argout;
    argout = new C_array_e_stringHolder();

    C_array_e_stringHolder arginout;
    arginout = new C_array_e_stringHolder();

    String[] _ret;
    argin = new String[ 2 ];
    argin [ 0 ] = "abc";
    argin [ 1 ] = "abc";
    arginout.value = new String[ 2 ];
    arginout.value [ 0 ] = "abc";
    arginout.value [ 1 ] = "abc";
    try
      {
        _ret = target.op42(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op42");
        return;
      }
    if (!(true && (_ret [ 0 ].equals("def")) && (_ret [ 1 ].equals("def"))))
      {
        fail("_ret value error in op42");
      }
    if (!(
          true && (argout.value [ 0 ].equals("def")) &&
          (argout.value [ 1 ].equals("def"))
        )
       )
      {
        fail("argout value error in op42");
      }
    if (!(
          true && (arginout.value [ 0 ].equals("def")) &&
          (arginout.value [ 1 ].equals("def"))
        )
       )
      {
        fail("arginout value error in op42");
      }
  }

  void call_op46()
  {
    D_d_short argin;
    D_d_shortHolder argout;
    argout = new D_d_shortHolder();

    D_d_shortHolder arginout;
    arginout = new D_d_shortHolder();

    D_d_short _ret;
    argin = cons_0002();
    arginout.value = cons_0002();
    try
      {
        _ret = target.op46(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op46");
        return;
      }
    if (!comp_0006(_ret))
      {
        fail("_ret value error in op46");
      }
    if (!comp_0006(argout.value))
      {
        fail("argout value error in op46");
      }
    if (!comp_0006(arginout.value))
      {
        fail("arginout value error in op46");
      }
  }

  void call_op47()
  {
    D_d_ushort argin;
    D_d_ushortHolder argout;
    argout = new D_d_ushortHolder();

    D_d_ushortHolder arginout;
    arginout = new D_d_ushortHolder();

    D_d_ushort _ret;
    argin = cons_0003();
    arginout.value = cons_0003();
    try
      {
        _ret = target.op47(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op47");
        return;
      }
    if (!comp_0007(_ret))
      {
        fail("_ret value error in op47");
      }
    if (!comp_0007(argout.value))
      {
        fail("argout value error in op47");
      }
    if (!comp_0007(arginout.value))
      {
        fail("arginout value error in op47");
      }
  }

  void call_op48()
  {
    D_d_long argin;
    D_d_longHolder argout;
    argout = new D_d_longHolder();

    D_d_longHolder arginout;
    arginout = new D_d_longHolder();

    D_d_long _ret;
    argin = cons_0004();
    arginout.value = cons_0004();
    try
      {
        _ret = target.op48(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op48");
        return;
      }
    if (!comp_0008(_ret))
      {
        fail("_ret value error in op48");
      }
    if (!comp_0008(argout.value))
      {
        fail("argout value error in op48");
      }
    if (!comp_0008(arginout.value))
      {
        fail("arginout value error in op48");
      }
  }

  void call_op49()
  {
    D_d_ulong argin;
    D_d_ulongHolder argout;
    argout = new D_d_ulongHolder();

    D_d_ulongHolder arginout;
    arginout = new D_d_ulongHolder();

    D_d_ulong _ret;
    argin = cons_0005();
    arginout.value = cons_0005();
    try
      {
        _ret = target.op49(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op49");
        return;
      }
    if (!comp_0009(_ret))
      {
        fail("_ret value error in op49");
      }
    if (!comp_0009(argout.value))
      {
        fail("argout value error in op49");
      }
    if (!comp_0009(arginout.value))
      {
        fail("arginout value error in op49");
      }
  }

  void call_op50()
  {
    D_d_char argin;
    D_d_charHolder argout;
    argout = new D_d_charHolder();

    D_d_charHolder arginout;
    arginout = new D_d_charHolder();

    D_d_char _ret;
    argin = cons_0006();
    arginout.value = cons_0006();
    try
      {
        _ret = target.op50(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op50");
        return;
      }
    if (!comp_0010(_ret))
      {
        fail("_ret value error in op50");
      }
    if (!comp_0010(argout.value))
      {
        fail("argout value error in op50");
      }
    if (!comp_0010(arginout.value))
      {
        fail("arginout value error in op50");
      }
  }

  void call_op51()
  {
    D_d_boolean argin;
    D_d_booleanHolder argout;
    argout = new D_d_booleanHolder();

    D_d_booleanHolder arginout;
    arginout = new D_d_booleanHolder();

    D_d_boolean _ret;
    argin = cons_0007();
    arginout.value = cons_0007();
    try
      {
        _ret = target.op51(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op51");
        return;
      }
    if (!comp_0011(_ret))
      {
        fail("_ret value error in op51");
      }
    if (!comp_0011(argout.value))
      {
        fail("argout value error in op51");
      }
    if (!comp_0011(arginout.value))
      {
        fail("arginout value error in op51");
      }
  }

  void call_op52()
  {
    D_d_B argin;
    D_d_BHolder argout;
    argout = new D_d_BHolder();

    D_d_BHolder arginout;
    arginout = new D_d_BHolder();

    D_d_B _ret;
    argin = cons_0008();
    arginout.value = cons_0008();
    try
      {
        _ret = target.op52(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op52");
        return;
      }
    if (!comp_0012(_ret))
      {
        fail("_ret value error in op52");
      }
    if (!comp_0012(argout.value))
      {
        fail("argout value error in op52");
      }
    if (!comp_0012(arginout.value))
      {
        fail("arginout value error in op52");
      }
  }

  void call_op53()
  {
    E_struct argin;
    E_structHolder argout;
    argout = new E_structHolder();

    E_structHolder arginout;
    arginout = new E_structHolder();

    E_struct _ret;
    argin = cons_0009();
    arginout.value = cons_0009();
    try
      {
        _ret = target.op53(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op53");
        return;
      }
    if (!comp_0013(_ret))
      {
        fail("_ret value error in op53");
      }
    if (!comp_0013(argout.value))
      {
        fail("argout value error in op53");
      }
    if (!comp_0013(arginout.value))
      {
        fail("arginout value error in op53");
      }
  }

  void call_op54()
  {
    E_union argin;
    E_unionHolder argout;
    argout = new E_unionHolder();

    E_unionHolder arginout;
    arginout = new E_unionHolder();

    E_union _ret;
    argin = cons_0010();
    arginout.value = cons_0010();
    try
      {
        _ret = target.op54(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op54");
        return;
      }
    if (!comp_0014(_ret))
      {
        fail("_ret value error in op54");
      }
    if (!comp_0014(argout.value))
      {
        fail("argout value error in op54");
      }
    if (!comp_0014(arginout.value))
      {
        fail("arginout value error in op54");
      }
  }

  void call_op55()
  {
    B[] argin;
    E_sequenceHolder argout;
    argout = new E_sequenceHolder();

    E_sequenceHolder arginout;
    arginout = new E_sequenceHolder();

    B[] _ret;
    argin = new B[ 2 ];
    argin [ 0 ] = B.b1;
    argin [ 1 ] = B.b1;
    arginout.value = new B[ 2 ];
    arginout.value [ 0 ] = B.b1;
    arginout.value [ 1 ] = B.b1;
    try
      {
        _ret = target.op55(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op55");
        return;
      }
    if (!(true && (_ret [ 0 ] == B.b2) && (_ret [ 1 ] == B.b2)))
      {
        fail("_ret value error in op55");
      }
    if (!(
          true && (argout.value [ 0 ] == B.b2) && (argout.value [ 1 ] == B.b2)
        ))
      {
        fail("argout value error in op55");
      }
    if (!(
          true && (arginout.value [ 0 ] == B.b2) &&
          (arginout.value [ 1 ] == B.b2)
        )
       )
      {
        fail("arginout value error in op55");
      }
  }

  void call_op56()
  {
    B[] argin;
    E_arrayHolder argout;
    argout = new E_arrayHolder();

    E_arrayHolder arginout;
    arginout = new E_arrayHolder();

    B[] _ret;
    argin = new B[ 2 ];
    argin [ 0 ] = B.b1;
    argin [ 1 ] = B.b1;
    arginout.value = new B[ 2 ];
    arginout.value [ 0 ] = B.b1;
    arginout.value [ 1 ] = B.b1;
    try
      {
        _ret = target.op56(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op56");
        return;
      }
    if (!(true && (_ret [ 0 ] == B.b2) && (_ret [ 1 ] == B.b2)))
      {
        fail("_ret value error in op56");
      }
    if (!(
          true && (argout.value [ 0 ] == B.b2) && (argout.value [ 1 ] == B.b2)
        ))
      {
        fail("argout value error in op56");
      }
    if (!(
          true && (arginout.value [ 0 ] == B.b2) &&
          (arginout.value [ 1 ] == B.b2)
        )
       )
      {
        fail("arginout value error in op56");
      }
  }

  void call_op60()
  {
    C_union[] argin;
    F_sequence_e_c_unionHolder argout;
    argout = new F_sequence_e_c_unionHolder();

    F_sequence_e_c_unionHolder arginout;
    arginout = new F_sequence_e_c_unionHolder();

    C_union[] _ret;
    argin = new C_union[ 2 ];
    argin [ 0 ] = cons_0017();
    argin [ 1 ] = cons_0017();
    arginout.value = new C_union[ 2 ];
    arginout.value [ 0 ] = cons_0017();
    arginout.value [ 1 ] = cons_0017();
    try
      {
        _ret = target.op60(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op60");
        return;
      }
    if (!(true && comp_0023(_ret [ 0 ]) && comp_0023(_ret [ 1 ])))
      {
        fail("_ret value error in op60");
      }
    if (!(
          true && comp_0023(argout.value [ 0 ]) &&
          comp_0023(argout.value [ 1 ])
        )
       )
      {
        fail("argout value error in op60");
      }
    if (!(
          true && comp_0023(arginout.value [ 0 ]) &&
          comp_0023(arginout.value [ 1 ])
        )
       )
      {
        fail("arginout value error in op60");
      }
  }

  void call_op90()
  {
    C_union[] argin;
    F_array_e_c_unionHolder argout;
    argout = new F_array_e_c_unionHolder();

    F_array_e_c_unionHolder arginout;
    arginout = new F_array_e_c_unionHolder();

    C_union[] _ret;
    argin = new C_union[ 2 ];
    argin [ 0 ] = cons_0019();
    argin [ 1 ] = cons_0019();
    arginout.value = new C_union[ 2 ];
    arginout.value [ 0 ] = cons_0019();
    arginout.value [ 1 ] = cons_0019();
    try
      {
        _ret = target.op90(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op90");
        return;
      }
    if (!(true && comp_0026(_ret [ 0 ]) && comp_0026(_ret [ 1 ])))
      {
        fail("_ret value error in op90");
      }
    if (!(
          true && comp_0026(argout.value [ 0 ]) &&
          comp_0026(argout.value [ 1 ])
        )
       )
      {
        fail("argout value error in op90");
      }
    if (!(
          true && comp_0026(arginout.value [ 0 ]) &&
          comp_0026(arginout.value [ 1 ])
        )
       )
      {
        fail("arginout value error in op90");
      }
  }

  void call_op119()
  {
    G_struct argin;
    G_structHolder argout;
    argout = new G_structHolder();

    G_structHolder arginout;
    arginout = new G_structHolder();

    G_struct _ret;
    argin = cons_0020();
    arginout.value = cons_0020();
    try
      {
        _ret = target.op119(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op119");
        return;
      }
    if (!comp_0027(_ret))
      {
        fail("_ret value error in op119");
      }
    if (!comp_0027(argout.value))
      {
        fail("argout value error in op119");
      }
    if (!comp_0027(arginout.value))
      {
        fail("arginout value error in op119");
      }
  }

  void call_op120()
  {
    G_union argin;
    G_unionHolder argout;
    argout = new G_unionHolder();

    G_unionHolder arginout;
    arginout = new G_unionHolder();

    G_union _ret;
    argin = cons_0023();
    arginout.value = cons_0023();
    try
      {
        _ret = target.op120(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op120");
        return;
      }
    if (!comp_0030(_ret))
      {
        fail("_ret value error in op120");
      }
    if (!comp_0030(argout.value))
      {
        fail("argout value error in op120");
      }
    if (!comp_0030(arginout.value))
      {
        fail("arginout value error in op120");
      }
  }

  void call_op121()
  {
    E_struct[] argin;
    G_sequence_e_e_structHolder argout;
    argout = new G_sequence_e_e_structHolder();

    G_sequence_e_e_structHolder arginout;
    arginout = new G_sequence_e_e_structHolder();

    E_struct[] _ret;
    argin = new E_struct[ 2 ];
    argin [ 0 ] = cons_0025();
    argin [ 1 ] = cons_0025();
    arginout.value = new E_struct[ 2 ];
    arginout.value [ 0 ] = cons_0025();
    arginout.value [ 1 ] = cons_0025();
    try
      {
        _ret = target.op121(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op121");
        return;
      }
    if (!(true && comp_0032(_ret [ 0 ]) && comp_0032(_ret [ 1 ])))
      {
        fail("_ret value error in op121");
      }
    if (!(
          true && comp_0032(argout.value [ 0 ]) &&
          comp_0032(argout.value [ 1 ])
        )
       )
      {
        fail("argout value error in op121");
      }
    if (!(
          true && comp_0032(arginout.value [ 0 ]) &&
          comp_0032(arginout.value [ 1 ])
        )
       )
      {
        fail("arginout value error in op121");
      }
  }

  void call_op122()
  {
    E_union[] argin;
    G_sequence_e_e_unionHolder argout;
    argout = new G_sequence_e_e_unionHolder();

    G_sequence_e_e_unionHolder arginout;
    arginout = new G_sequence_e_e_unionHolder();

    E_union[] _ret;
    argin = new E_union[ 2 ];
    argin [ 0 ] = cons_0026();
    argin [ 1 ] = cons_0026();
    arginout.value = new E_union[ 2 ];
    arginout.value [ 0 ] = cons_0026();
    arginout.value [ 1 ] = cons_0026();
    try
      {
        _ret = target.op122(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op122");
        return;
      }
    if (!(true && comp_0033(_ret [ 0 ]) && comp_0033(_ret [ 1 ])))
      {
        fail("_ret value error in op122");
      }
    if (!(
          true && comp_0033(argout.value [ 0 ]) &&
          comp_0033(argout.value [ 1 ])
        )
       )
      {
        fail("argout value error in op122");
      }
    if (!(
          true && comp_0033(arginout.value [ 0 ]) &&
          comp_0033(arginout.value [ 1 ])
        )
       )
      {
        fail("arginout value error in op122");
      }
  }

  void call_op125()
  {
    E_struct[] argin;
    G_array_e_e_structHolder argout;
    argout = new G_array_e_e_structHolder();

    G_array_e_e_structHolder arginout;
    arginout = new G_array_e_e_structHolder();

    E_struct[] _ret;
    argin = new E_struct[ 2 ];
    argin [ 0 ] = cons_0027();
    argin [ 1 ] = cons_0027();
    arginout.value = new E_struct[ 2 ];
    arginout.value [ 0 ] = cons_0027();
    arginout.value [ 1 ] = cons_0027();
    try
      {
        _ret = target.op125(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op125");
        return;
      }
    if (!(true && comp_0034(_ret [ 0 ]) && comp_0034(_ret [ 1 ])))
      {
        fail("_ret value error in op125");
      }
    if (!(
          true && comp_0034(argout.value [ 0 ]) &&
          comp_0034(argout.value [ 1 ])
        )
       )
      {
        fail("argout value error in op125");
      }
    if (!(
          true && comp_0034(arginout.value [ 0 ]) &&
          comp_0034(arginout.value [ 1 ])
        )
       )
      {
        fail("arginout value error in op125");
      }
  }

  void call_op126()
  {
    E_union[] argin;
    G_array_e_e_unionHolder argout;
    argout = new G_array_e_e_unionHolder();

    G_array_e_e_unionHolder arginout;
    arginout = new G_array_e_e_unionHolder();

    E_union[] _ret;
    argin = new E_union[ 2 ];
    argin [ 0 ] = cons_0028();
    argin [ 1 ] = cons_0028();
    arginout.value = new E_union[ 2 ];
    arginout.value [ 0 ] = cons_0028();
    arginout.value [ 1 ] = cons_0028();
    try
      {
        _ret = target.op126(argin, argout, arginout);
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in op126");
        return;
      }
    if (!(true && comp_0035(_ret [ 0 ]) && comp_0035(_ret [ 1 ])))
      {
        fail("_ret value error in op126");
      }
    if (!(
          true && comp_0035(argout.value [ 0 ]) &&
          comp_0035(argout.value [ 1 ])
        )
       )
      {
        fail("argout value error in op126");
      }
    if (!(
          true && comp_0035(arginout.value [ 0 ]) &&
          comp_0035(arginout.value [ 1 ])
        )
       )
      {
        fail("arginout value error in op126");
      }
  }

  void call_excop1()
  {
    try
      {
        target.excop1();
      }
    catch (A_except1 _exc)
      {
        if (!comp_0042(_exc))
          {
            fail("_exc value error in excop1");
          }
        return;
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in excop1");
        return;
      }
    fail("no exception raised in excop1");
  }

  void call_excop3()
  {
    try
      {
        target.excop3();
      }
    catch (B_except _exc)
      {
        if (!comp_0045(_exc))
          {
            fail("_exc value error in excop3");
          }
        return;
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in excop3");
        return;
      }
    fail("no exception raised in excop3");
  }

  void call_excop5()
  {
    try
      {
        target.excop5();
      }
    catch (D_except _exc)
      {
        if (!comp_0050(_exc))
          {
            fail("_exc value error in excop5");
          }
        return;
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in excop5");
        return;
      }
    fail("no exception raised in excop5");
  }

  void call_excop6()
  {
    try
      {
        target.excop6();
      }
    catch (E_except _exc)
      {
        if (!comp_0058(_exc))
          {
            fail("_exc value error in excop6");
          }
        return;
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in excop6");
        return;
      }
    fail("no exception raised in excop6");
  }

  void call_excop10()
  {
    try
      {
        target.excop10();
      }
    catch (G_except _exc)
      {
        if (!comp_0075(_exc))
          {
            fail("_exc value error in excop10");
          }
        return;
      }
    catch (Exception _exc)
      {
        _exc.printStackTrace(System.out);
        fail("unexpected exception in excop10");
        return;
      }
    fail("no exception raised in excop10");
  }

  public void run_all(TestHarness harness)
  {
    h = harness;

    call_op1();
    call_op2();
    call_op3();
    call_op4();
    call_op5();
    call_op6();
    call_op7();
    call_op8();
    call_op9();
    call_op15();
    call_op17();
    call_op18();
    call_op19();
    call_op20();
    call_op21();
    call_op22();
    call_op23();
    call_op24();
    call_op25();
    call_op26();
    call_op28();
    call_op32();
    call_op33();
    call_op34();
    call_op35();
    call_op36();
    call_op37();
    call_op38();
    call_op39();
    call_op40();
    call_op42();
    call_op46();
    call_op47();
    call_op48();
    call_op49();
    call_op50();
    call_op51();
    call_op52();
    call_op53();
    call_op54();
    call_op55();
    call_op56();
    call_op60();
    call_op90();
    call_op119();
    call_op120();
    call_op121();
    call_op122();
    call_op125();
    call_op126();
    call_excop1();
    call_excop3();
    call_excop5();
    call_excop6();
    call_excop10();
  }
}
