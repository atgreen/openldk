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
// $Id: rf11Servant.java,v 1.1 2005/06/04 12:00:34 audriusa Exp $
//
// common headers
// ****************************

/*
This code originally came from the OMG's CORBA Open Source Testing project,
which lived at cost.omg.org. That site no longer exists.

All the contributing companies agreed to release their tests under the
terms of the GNU Lesser General Public License, available in the file
COPYING.LIB.

The code has been modified integrating into Mauve test environment and
removing tests that are not yet supported by Suns jre 1.4. Hence the license
is now GPL.
*/

package gnu.testlet.org.omg.CORBA.ORB.RF11;

import org.omg.CORBA.ORB;
import org.omg.CORBA.TCKind;

import java.util.ArrayList;

public class rf11Servant
  extends _rf11ImplBase
{
  /**
   * The server side error messages are logged into this array.
   * If both client and server runs in the same virtual machine,
   * this static field can be later accessed for checking for the
   * possible server side errors.
   */
  public static ArrayList error_messages = new ArrayList();
  ORB orb;
  rf11Servant targetobj;
  NEC_RF11 target;

  public void fail(String why)
  {
    error_messages.add(why);
  }

  //runtime routines
  boolean comp_0080(org.omg.CORBA.Any _v1)
  {
    String _v2;
    _v2 = _v1.extract_string();
    return (_v2.equals("abc"));
  }

  boolean comp_0082(org.omg.CORBA.Any _v1)
  {
    String _v2;
    _v2 = _v1.extract_string();
    return (_v2.equals("abc"));
  }

  boolean comp_0081(C_struct _v1)
  {
    return (true && (_v1.e_short == -100) && (_v1.e_ushort == 100) &&
           (_v1.e_long == -100000) && (_v1.e_ulong == 100000) &&
           (_v1.e_float == 0.123f) && (_v1.e_double == 0.12e3) &&
           (_v1.e_char == 'a') && (_v1.e_boolean == false) &&
           (_v1.e_octet == 10) && comp_0082(_v1.e_any) &&
           (_v1.e_string.equals("abc")) &&
           (_v1.e_Object._is_equivalent(target)));
  }

  C_struct cons_0035()
  {
    C_struct _v1;
    _v1 = new C_struct();
    _v1.e_short = -200;
    _v1.e_ushort = 200;
    _v1.e_long = -200000;
    _v1.e_ulong = 200000;
    _v1.e_float = 1.234f;
    _v1.e_double = 1.23e4;
    _v1.e_char = 'b';
    _v1.e_boolean = true;
    _v1.e_octet = 20;
    _v1.e_any = orb.create_any();
    _v1.e_any.insert_long(-200000);
    _v1.e_string = "def";
    _v1.e_Object = target;
    return (_v1);
  }

  boolean comp_0083(C_union _v1)
  {
    if (_v1.discriminator() != 1)
      return false;
    return (_v1.e_short() == -100);
  }

  C_union cons_0036()
  {
    C_union _v1;
    _v1 = new C_union();

    short _v2;
    _v2 = 200;
    _v1.e_ushort(_v2);
    return (_v1);
  }

  boolean comp_0084(org.omg.CORBA.Any _v1)
  {
    String _v2;
    _v2 = _v1.extract_string();
    return (_v2.equals("abc"));
  }

  boolean comp_0085(org.omg.CORBA.Any _v1)
  {
    String _v2;
    _v2 = _v1.extract_string();
    return (_v2.equals("abc"));
  }

  boolean comp_0086(D_d_short _v1)
  {
    if (_v1.discriminator() != 1)
      return false;
    return (_v1.l1() == -100000);
  }

  D_d_short cons_0037()
  {
    D_d_short _v1;
    _v1 = new D_d_short();

    int _v2;
    _v2 = -200000;
    _v1.l2(_v2);
    return (_v1);
  }

  boolean comp_0087(D_d_ushort _v1)
  {
    if (_v1.discriminator() != 1)
      return false;
    return (_v1.l1() == -100000);
  }

  D_d_ushort cons_0038()
  {
    D_d_ushort _v1;
    _v1 = new D_d_ushort();

    int _v2;
    _v2 = -200000;
    _v1.l2(_v2);
    return (_v1);
  }

  boolean comp_0088(D_d_long _v1)
  {
    if (_v1.discriminator() != 1)
      return false;
    return (_v1.l1() == -100000);
  }

  D_d_long cons_0039()
  {
    D_d_long _v1;
    _v1 = new D_d_long();

    int _v2;
    _v2 = -200000;
    _v1.l2(_v2);
    return (_v1);
  }

  boolean comp_0089(D_d_ulong _v1)
  {
    if (_v1.discriminator() != 1)
      return false;
    return (_v1.l1() == -100000);
  }

  D_d_ulong cons_0040()
  {
    D_d_ulong _v1;
    _v1 = new D_d_ulong();

    int _v2;
    _v2 = -200000;
    _v1.l2(_v2);
    return (_v1);
  }

  boolean comp_0090(D_d_char _v1)
  {
    if (_v1.discriminator() != 'a')
      return false;
    return (_v1.l1() == -100000);
  }

  D_d_char cons_0041()
  {
    D_d_char _v1;
    _v1 = new D_d_char();

    int _v2;
    _v2 = -200000;
    _v1.l2(_v2);
    return (_v1);
  }

  boolean comp_0091(D_d_boolean _v1)
  {
    if (_v1.discriminator() != false)
      return false;
    return (_v1.l2() == -100000);
  }

  D_d_boolean cons_0042()
  {
    D_d_boolean _v1;
    _v1 = new D_d_boolean();

    int _v2;
    _v2 = -200000;
    _v1.l1(_v2);
    return (_v1);
  }

  boolean comp_0092(D_d_B _v1)
  {
    if (_v1.discriminator() != B.b1)
      return false;
    return (_v1.l1() == -100000);
  }

  D_d_B cons_0043()
  {
    D_d_B _v1;
    _v1 = new D_d_B();

    int _v2;
    _v2 = -200000;
    _v1.l2(_v2);
    return (_v1);
  }

  boolean comp_0093(E_struct _v1)
  {
    return (true && (_v1.e_b1 == B.b1) && (_v1.e_b2 == B.b1));
  }

  E_struct cons_0044()
  {
    E_struct _v1;
    _v1 = new E_struct();
    _v1.e_b1 = B.b2;
    _v1.e_b2 = B.b2;
    return (_v1);
  }

  boolean comp_0094(E_union _v1)
  {
    if (_v1.discriminator() != 1)
      return false;
    return (_v1.e_b1() == B.b1);
  }

  E_union cons_0045()
  {
    E_union _v1;
    _v1 = new E_union();

    B _v2;
    _v2 = B.b2;
    _v1.e_b2(_v2);
    return (_v1);
  }

  boolean comp_0097(org.omg.CORBA.Any _v1)
  {
    String _v2;
    _v2 = _v1.extract_string();
    return (_v2.equals("abc"));
  }

  boolean comp_0096(C_struct _v1)
  {
    return (true && (_v1.e_short == -100) && (_v1.e_ushort == 100) &&
           (_v1.e_long == -100000) && (_v1.e_ulong == 100000) &&
           (_v1.e_float == 0.123f) && (_v1.e_double == 0.12e3) &&
           (_v1.e_char == 'a') && (_v1.e_boolean == false) &&
           (_v1.e_octet == 10) && comp_0097(_v1.e_any) &&
           (_v1.e_string.equals("abc")) &&
           (_v1.e_Object._is_equivalent(target)));
  }

  boolean comp_0098(C_union _v1)
  {
    if (_v1.discriminator() != 1)
      return false;
    return (_v1.e_short() == -100);
  }

  boolean comp_0095(F_struct _v1)
  {
    return (true && comp_0096(_v1.e_c_struct) && comp_0098(_v1.e_c_union) &&
           (
             true && (_v1.e_c_sequence_e_short [ 0 ] == -100) &&
             (_v1.e_c_sequence_e_short [ 1 ] == -100)
           ) &&
           (
             true && (_v1.e_c_sequence_e_ushort [ 0 ] == 100) &&
             (_v1.e_c_sequence_e_ushort [ 1 ] == 100)
           ) &&
           (
             true && (_v1.e_c_sequence_e_long [ 0 ] == -100000) &&
             (_v1.e_c_sequence_e_long [ 1 ] == -100000)
           ) &&
           (
             true && (_v1.e_c_sequence_e_ulong [ 0 ] == 100000) &&
             (_v1.e_c_sequence_e_ulong [ 1 ] == 100000)
           ) &&
           (
             true && (_v1.e_c_sequence_e_float [ 0 ] == 0.123f) &&
             (_v1.e_c_sequence_e_float [ 1 ] == 0.123f)
           ) &&
           (
             true && (_v1.e_c_sequence_e_double [ 0 ] == 0.12e3) &&
             (_v1.e_c_sequence_e_double [ 1 ] == 0.12e3)
           ) &&
           (
             true && (_v1.e_c_sequence_e_char [ 0 ] == 'a') &&
             (_v1.e_c_sequence_e_char [ 1 ] == 'a')
           ) &&
           (
             true && (_v1.e_c_sequence_e_boolean [ 0 ] == false) &&
             (_v1.e_c_sequence_e_boolean [ 1 ] == false)
           ) &&
           (
             true && (_v1.e_c_sequence_e_octet [ 0 ] == 10) &&
             (_v1.e_c_sequence_e_octet [ 1 ] == 10)
           ) &&
           (
             true && comp_0097(_v1.e_c_sequence_e_any [ 0 ]) &&
             comp_0097(_v1.e_c_sequence_e_any [ 1 ])
           ) &&
           (
             true && (_v1.e_c_sequence_e_string [ 0 ].equals("abc")) &&
             (_v1.e_c_sequence_e_string [ 1 ].equals("abc"))
           ) &&
           (
             true && (_v1.e_c_sequence_e_Object [ 0 ]._is_equivalent(target)) &&
             (_v1.e_c_sequence_e_Object [ 1 ]._is_equivalent(target))
           ) &&
           (
             true && (_v1.e_c_array_e_short [ 0 ] == -100) &&
             (_v1.e_c_array_e_short [ 1 ] == -100)
           ) &&
           (
             true && (_v1.e_c_array_e_ushort [ 0 ] == 100) &&
             (_v1.e_c_array_e_ushort [ 1 ] == 100)
           ) &&
           (
             true && (_v1.e_c_array_e_long [ 0 ] == -100000) &&
             (_v1.e_c_array_e_long [ 1 ] == -100000)
           ) &&
           (
             true && (_v1.e_c_array_e_ulong [ 0 ] == 100000) &&
             (_v1.e_c_array_e_ulong [ 1 ] == 100000)
           ) &&
           (
             true && (_v1.e_c_array_e_float [ 0 ] == 0.123f) &&
             (_v1.e_c_array_e_float [ 1 ] == 0.123f)
           ) &&
           (
             true && (_v1.e_c_array_e_double [ 0 ] == 0.12e3) &&
             (_v1.e_c_array_e_double [ 1 ] == 0.12e3)
           ) &&
           (
             true && (_v1.e_c_array_e_char [ 0 ] == 'a') &&
             (_v1.e_c_array_e_char [ 1 ] == 'a')
           ) &&
           (
             true && (_v1.e_c_array_e_boolean [ 0 ] == false) &&
             (_v1.e_c_array_e_boolean [ 1 ] == false)
           ) &&
           (
             true && (_v1.e_c_array_e_octet [ 0 ] == 10) &&
             (_v1.e_c_array_e_octet [ 1 ] == 10)
           ) &&
           (
             true && comp_0097(_v1.e_c_array_e_any [ 0 ]) &&
             comp_0097(_v1.e_c_array_e_any [ 1 ])
           ) &&
           (
             true && (_v1.e_c_array_e_string [ 0 ].equals("abc")) &&
             (_v1.e_c_array_e_string [ 1 ].equals("abc"))
           ) &&
           (
             true && (_v1.e_c_array_e_Object [ 0 ]._is_equivalent(target)) &&
             (_v1.e_c_array_e_Object [ 1 ]._is_equivalent(target))
           ));
  }

  C_struct cons_0047()
  {
    C_struct _v1;
    _v1 = new C_struct();
    _v1.e_short = -200;
    _v1.e_ushort = 200;
    _v1.e_long = -200000;
    _v1.e_ulong = 200000;
    _v1.e_float = 1.234f;
    _v1.e_double = 1.23e4;
    _v1.e_char = 'b';
    _v1.e_boolean = true;
    _v1.e_octet = 20;
    _v1.e_any = orb.create_any();
    _v1.e_any.insert_long(-200000);
    _v1.e_string = "def";
    _v1.e_Object = target;
    return (_v1);
  }

  C_union cons_0048()
  {
    C_union _v1;
    _v1 = new C_union();

    short _v2;
    _v2 = 200;
    _v1.e_ushort(_v2);
    return (_v1);
  }

  F_struct cons_0046()
  {
    F_struct _v1;
    _v1 = new F_struct();
    _v1.e_c_struct = cons_0047();
    _v1.e_c_union = cons_0048();
    _v1.e_c_sequence_e_short = new short[ 2 ];
    _v1.e_c_sequence_e_short [ 0 ] = -200;
    _v1.e_c_sequence_e_short [ 1 ] = -200;
    _v1.e_c_sequence_e_ushort = new short[ 2 ];
    _v1.e_c_sequence_e_ushort [ 0 ] = 200;
    _v1.e_c_sequence_e_ushort [ 1 ] = 200;
    _v1.e_c_sequence_e_long = new int[ 2 ];
    _v1.e_c_sequence_e_long [ 0 ] = -200000;
    _v1.e_c_sequence_e_long [ 1 ] = -200000;
    _v1.e_c_sequence_e_ulong = new int[ 2 ];
    _v1.e_c_sequence_e_ulong [ 0 ] = 200000;
    _v1.e_c_sequence_e_ulong [ 1 ] = 200000;
    _v1.e_c_sequence_e_float = new float[ 2 ];
    _v1.e_c_sequence_e_float [ 0 ] = 1.234f;
    _v1.e_c_sequence_e_float [ 1 ] = 1.234f;
    _v1.e_c_sequence_e_double = new double[ 2 ];
    _v1.e_c_sequence_e_double [ 0 ] = 1.23e4;
    _v1.e_c_sequence_e_double [ 1 ] = 1.23e4;
    _v1.e_c_sequence_e_char = new char[ 2 ];
    _v1.e_c_sequence_e_char [ 0 ] = 'b';
    _v1.e_c_sequence_e_char [ 1 ] = 'b';
    _v1.e_c_sequence_e_boolean = new boolean[ 2 ];
    _v1.e_c_sequence_e_boolean [ 0 ] = true;
    _v1.e_c_sequence_e_boolean [ 1 ] = true;
    _v1.e_c_sequence_e_octet = new byte[ 2 ];
    _v1.e_c_sequence_e_octet [ 0 ] = 20;
    _v1.e_c_sequence_e_octet [ 1 ] = 20;
    _v1.e_c_sequence_e_any = new org.omg.CORBA.Any[ 2 ];
    _v1.e_c_sequence_e_any [ 0 ] = orb.create_any();
    _v1.e_c_sequence_e_any [ 0 ].insert_long(-200000);
    _v1.e_c_sequence_e_any [ 1 ] = orb.create_any();
    _v1.e_c_sequence_e_any [ 1 ].insert_long(-200000);
    _v1.e_c_sequence_e_string = new String[ 2 ];
    _v1.e_c_sequence_e_string [ 0 ] = "def";
    _v1.e_c_sequence_e_string [ 1 ] = "def";
    _v1.e_c_sequence_e_Object = new org.omg.CORBA.Object[ 2 ];
    _v1.e_c_sequence_e_Object [ 0 ] = target;
    _v1.e_c_sequence_e_Object [ 1 ] = target;
    _v1.e_c_array_e_short = new short[ 2 ];
    _v1.e_c_array_e_short [ 0 ] = -200;
    _v1.e_c_array_e_short [ 1 ] = -200;
    _v1.e_c_array_e_ushort = new short[ 2 ];
    _v1.e_c_array_e_ushort [ 0 ] = 200;
    _v1.e_c_array_e_ushort [ 1 ] = 200;
    _v1.e_c_array_e_long = new int[ 2 ];
    _v1.e_c_array_e_long [ 0 ] = -200000;
    _v1.e_c_array_e_long [ 1 ] = -200000;
    _v1.e_c_array_e_ulong = new int[ 2 ];
    _v1.e_c_array_e_ulong [ 0 ] = 200000;
    _v1.e_c_array_e_ulong [ 1 ] = 200000;
    _v1.e_c_array_e_float = new float[ 2 ];
    _v1.e_c_array_e_float [ 0 ] = 1.234f;
    _v1.e_c_array_e_float [ 1 ] = 1.234f;
    _v1.e_c_array_e_double = new double[ 2 ];
    _v1.e_c_array_e_double [ 0 ] = 1.23e4;
    _v1.e_c_array_e_double [ 1 ] = 1.23e4;
    _v1.e_c_array_e_char = new char[ 2 ];
    _v1.e_c_array_e_char [ 0 ] = 'b';
    _v1.e_c_array_e_char [ 1 ] = 'b';
    _v1.e_c_array_e_boolean = new boolean[ 2 ];
    _v1.e_c_array_e_boolean [ 0 ] = true;
    _v1.e_c_array_e_boolean [ 1 ] = true;
    _v1.e_c_array_e_octet = new byte[ 2 ];
    _v1.e_c_array_e_octet [ 0 ] = 20;
    _v1.e_c_array_e_octet [ 1 ] = 20;
    _v1.e_c_array_e_any = new org.omg.CORBA.Any[ 2 ];
    _v1.e_c_array_e_any [ 0 ] = orb.create_any();
    _v1.e_c_array_e_any [ 0 ].insert_long(-200000);
    _v1.e_c_array_e_any [ 1 ] = orb.create_any();
    _v1.e_c_array_e_any [ 1 ].insert_long(-200000);
    _v1.e_c_array_e_string = new String[ 2 ];
    _v1.e_c_array_e_string [ 0 ] = "def";
    _v1.e_c_array_e_string [ 1 ] = "def";
    _v1.e_c_array_e_Object = new org.omg.CORBA.Object[ 2 ];
    _v1.e_c_array_e_Object [ 0 ] = target;
    _v1.e_c_array_e_Object [ 1 ] = target;
    return (_v1);
  }

  boolean comp_0101(org.omg.CORBA.Any _v1)
  {
    String _v2;
    _v2 = _v1.extract_string();
    return (_v2.equals("abc"));
  }

  boolean comp_0100(C_struct _v1)
  {
    return (true && (_v1.e_short == -100) && (_v1.e_ushort == 100) &&
           (_v1.e_long == -100000) && (_v1.e_ulong == 100000) &&
           (_v1.e_float == 0.123f) && (_v1.e_double == 0.12e3) &&
           (_v1.e_char == 'a') && (_v1.e_boolean == false) &&
           (_v1.e_octet == 10) && comp_0101(_v1.e_any) &&
           (_v1.e_string.equals("abc")) &&
           (_v1.e_Object._is_equivalent(target)));
  }

  boolean comp_0099(F_union _v1)
  {
    if (_v1.discriminator() != 1)
      return false;
    return comp_0100(_v1.e_c_struct());
  }

  C_union cons_0050()
  {
    C_union _v1;
    _v1 = new C_union();

    short _v2;
    _v2 = 200;
    _v1.e_ushort(_v2);
    return (_v1);
  }

  F_union cons_0049()
  {
    F_union _v1;
    _v1 = new F_union();

    C_union _v2;
    _v2 = cons_0050();
    _v1.e_c_union(_v2);
    return (_v1);
  }

  boolean comp_0103(org.omg.CORBA.Any _v1)
  {
    String _v2;
    _v2 = _v1.extract_string();
    return (_v2.equals("abc"));
  }

  boolean comp_0102(C_struct _v1)
  {
    return (true && (_v1.e_short == -100) && (_v1.e_ushort == 100) &&
           (_v1.e_long == -100000) && (_v1.e_ulong == 100000) &&
           (_v1.e_float == 0.123f) && (_v1.e_double == 0.12e3) &&
           (_v1.e_char == 'a') && (_v1.e_boolean == false) &&
           (_v1.e_octet == 10) && comp_0103(_v1.e_any) &&
           (_v1.e_string.equals("abc")) &&
           (_v1.e_Object._is_equivalent(target)));
  }

  C_struct cons_0051()
  {
    C_struct _v1;
    _v1 = new C_struct();
    _v1.e_short = -200;
    _v1.e_ushort = 200;
    _v1.e_long = -200000;
    _v1.e_ulong = 200000;
    _v1.e_float = 1.234f;
    _v1.e_double = 1.23e4;
    _v1.e_char = 'b';
    _v1.e_boolean = true;
    _v1.e_octet = 20;
    _v1.e_any = orb.create_any();
    _v1.e_any.insert_long(-200000);
    _v1.e_string = "def";
    _v1.e_Object = target;
    return (_v1);
  }

  boolean comp_0104(C_union _v1)
  {
    if (_v1.discriminator() != 1)
      return false;
    return (_v1.e_short() == -100);
  }

  C_union cons_0052()
  {
    C_union _v1;
    _v1 = new C_union();

    short _v2;
    _v2 = 200;
    _v1.e_ushort(_v2);
    return (_v1);
  }

  boolean comp_0106(org.omg.CORBA.Any _v1)
  {
    String _v2;
    _v2 = _v1.extract_string();
    return (_v2.equals("abc"));
  }

  boolean comp_0105(C_struct _v1)
  {
    return (true && (_v1.e_short == -100) && (_v1.e_ushort == 100) &&
           (_v1.e_long == -100000) && (_v1.e_ulong == 100000) &&
           (_v1.e_float == 0.123f) && (_v1.e_double == 0.12e3) &&
           (_v1.e_char == 'a') && (_v1.e_boolean == false) &&
           (_v1.e_octet == 10) && comp_0106(_v1.e_any) &&
           (_v1.e_string.equals("abc")) &&
           (_v1.e_Object._is_equivalent(target)));
  }

  C_struct cons_0053()
  {
    C_struct _v1;
    _v1 = new C_struct();
    _v1.e_short = -200;
    _v1.e_ushort = 200;
    _v1.e_long = -200000;
    _v1.e_ulong = 200000;
    _v1.e_float = 1.234f;
    _v1.e_double = 1.23e4;
    _v1.e_char = 'b';
    _v1.e_boolean = true;
    _v1.e_octet = 20;
    _v1.e_any = orb.create_any();
    _v1.e_any.insert_long(-200000);
    _v1.e_string = "def";
    _v1.e_Object = target;
    return (_v1);
  }

  boolean comp_0107(C_union _v1)
  {
    if (_v1.discriminator() != 1)
      return false;
    return (_v1.e_short() == -100);
  }

  C_union cons_0054()
  {
    C_union _v1;
    _v1 = new C_union();

    short _v2;
    _v2 = 200;
    _v1.e_ushort(_v2);
    return (_v1);
  }

  boolean comp_0109(E_struct _v1)
  {
    return (true && (_v1.e_b1 == B.b1) && (_v1.e_b2 == B.b1));
  }

  boolean comp_0110(E_union _v1)
  {
    if (_v1.discriminator() != 1)
      return false;
    return (_v1.e_b1() == B.b1);
  }

  boolean comp_0108(G_struct _v1)
  {
    return (true && comp_0109(_v1.e_e_struct) && comp_0110(_v1.e_e_union) &&
           (
             true && (_v1.e_e_sequence [ 0 ] == B.b1) &&
             (_v1.e_e_sequence [ 1 ] == B.b1)
           ) &&
           (
             true && (_v1.e_e_array [ 0 ] == B.b1) &&
             (_v1.e_e_array [ 1 ] == B.b1)
           ));
  }

  E_struct cons_0056()
  {
    E_struct _v1;
    _v1 = new E_struct();
    _v1.e_b1 = B.b2;
    _v1.e_b2 = B.b2;
    return (_v1);
  }

  E_union cons_0057()
  {
    E_union _v1;
    _v1 = new E_union();

    B _v2;
    _v2 = B.b2;
    _v1.e_b2(_v2);
    return (_v1);
  }

  G_struct cons_0055()
  {
    G_struct _v1;
    _v1 = new G_struct();
    _v1.e_e_struct = cons_0056();
    _v1.e_e_union = cons_0057();
    _v1.e_e_sequence = new B[ 2 ];
    _v1.e_e_sequence [ 0 ] = B.b2;
    _v1.e_e_sequence [ 1 ] = B.b2;
    _v1.e_e_array = new B[ 2 ];
    _v1.e_e_array [ 0 ] = B.b2;
    _v1.e_e_array [ 1 ] = B.b2;
    return (_v1);
  }

  boolean comp_0112(E_struct _v1)
  {
    return (true && (_v1.e_b1 == B.b1) && (_v1.e_b2 == B.b1));
  }

  boolean comp_0111(G_union _v1)
  {
    if (_v1.discriminator() != 1)
      return false;
    return comp_0112(_v1.e_e_struct());
  }

  E_union cons_0059()
  {
    E_union _v1;
    _v1 = new E_union();

    B _v2;
    _v2 = B.b2;
    _v1.e_b2(_v2);
    return (_v1);
  }

  G_union cons_0058()
  {
    G_union _v1;
    _v1 = new G_union();

    E_union _v2;
    _v2 = cons_0059();
    _v1.e_e_union(_v2);
    return (_v1);
  }

  boolean comp_0113(E_struct _v1)
  {
    return (true && (_v1.e_b1 == B.b1) && (_v1.e_b2 == B.b1));
  }

  E_struct cons_0060()
  {
    E_struct _v1;
    _v1 = new E_struct();
    _v1.e_b1 = B.b2;
    _v1.e_b2 = B.b2;
    return (_v1);
  }

  boolean comp_0114(E_union _v1)
  {
    if (_v1.discriminator() != 1)
      return false;
    return (_v1.e_b1() == B.b1);
  }

  E_union cons_0061()
  {
    E_union _v1;
    _v1 = new E_union();

    B _v2;
    _v2 = B.b2;
    _v1.e_b2(_v2);
    return (_v1);
  }

  boolean comp_0115(E_struct _v1)
  {
    return (true && (_v1.e_b1 == B.b1) && (_v1.e_b2 == B.b1));
  }

  E_struct cons_0062()
  {
    E_struct _v1;
    _v1 = new E_struct();
    _v1.e_b1 = B.b2;
    _v1.e_b2 = B.b2;
    return (_v1);
  }

  boolean comp_0116(E_union _v1)
  {
    if (_v1.discriminator() != 1)
      return false;
    return (_v1.e_b1() == B.b1);
  }

  E_union cons_0063()
  {
    E_union _v1;
    _v1 = new E_union();

    B _v2;
    _v2 = B.b2;
    _v1.e_b2(_v2);
    return (_v1);
  }

  boolean comp_0119(org.omg.CORBA.Any _v1)
  {
    String _v2;
    _v2 = _v1.extract_string();
    return (_v2.equals("abc"));
  }

  boolean comp_0118(C_struct _v1)
  {
    return (true && (_v1.e_short == -100) && (_v1.e_ushort == 100) &&
           (_v1.e_long == -100000) && (_v1.e_ulong == 100000) &&
           (_v1.e_float == 0.123f) && (_v1.e_double == 0.12e3) &&
           (_v1.e_char == 'a') && (_v1.e_boolean == false) &&
           (_v1.e_octet == 10) && comp_0119(_v1.e_any) &&
           (_v1.e_string.equals("abc")) &&
           (_v1.e_Object._is_equivalent(target)));
  }

  boolean comp_0117(F_union _v1)
  {
    if (_v1.discriminator() != 1)
      return false;
    return comp_0118(_v1.e_c_struct());
  }

  C_union cons_0065()
  {
    C_union _v1;
    _v1 = new C_union();

    short _v2;
    _v2 = 200;
    _v1.e_ushort(_v2);
    return (_v1);
  }

  F_union cons_0064()
  {
    F_union _v1;
    _v1 = new F_union();

    C_union _v2;
    _v2 = cons_0065();
    _v1.e_c_union(_v2);
    return (_v1);
  }

  boolean comp_0122(org.omg.CORBA.Any _v1)
  {
    String _v2;
    _v2 = _v1.extract_string();
    return (_v2.equals("abc"));
  }

  boolean comp_0121(C_struct _v1)
  {
    return (true && (_v1.e_short == -100) && (_v1.e_ushort == 100) &&
           (_v1.e_long == -100000) && (_v1.e_ulong == 100000) &&
           (_v1.e_float == 0.123f) && (_v1.e_double == 0.12e3) &&
           (_v1.e_char == 'a') && (_v1.e_boolean == false) &&
           (_v1.e_octet == 10) && comp_0122(_v1.e_any) &&
           (_v1.e_string.equals("abc")) &&
           (_v1.e_Object._is_equivalent(target)));
  }

  boolean comp_0120(F_union _v1)
  {
    if (_v1.discriminator() != 1)
      return false;
    return comp_0121(_v1.e_c_struct());
  }

  C_union cons_0067()
  {
    C_union _v1;
    _v1 = new C_union();

    short _v2;
    _v2 = 200;
    _v1.e_ushort(_v2);
    return (_v1);
  }

  F_union cons_0066()
  {
    F_union _v1;
    _v1 = new F_union();

    C_union _v2;
    _v2 = cons_0067();
    _v1.e_c_union(_v2);
    return (_v1);
  }

  boolean comp_0125(org.omg.CORBA.Any _v1)
  {
    String _v2;
    _v2 = _v1.extract_string();
    return (_v2.equals("abc"));
  }

  boolean comp_0124(C_struct _v1)
  {
    return (true && (_v1.e_short == -100) && (_v1.e_ushort == 100) &&
           (_v1.e_long == -100000) && (_v1.e_ulong == 100000) &&
           (_v1.e_float == 0.123f) && (_v1.e_double == 0.12e3) &&
           (_v1.e_char == 'a') && (_v1.e_boolean == false) &&
           (_v1.e_octet == 10) && comp_0125(_v1.e_any) &&
           (_v1.e_string.equals("abc")) &&
           (_v1.e_Object._is_equivalent(target)));
  }

  boolean comp_0123(F_union _v1)
  {
    if (_v1.discriminator() != 1)
      return false;
    return comp_0124(_v1.e_c_struct());
  }

  C_union cons_0069()
  {
    C_union _v1;
    _v1 = new C_union();

    short _v2;
    _v2 = 200;
    _v1.e_ushort(_v2);
    return (_v1);
  }

  F_union cons_0068()
  {
    F_union _v1;
    _v1 = new F_union();

    C_union _v2;
    _v2 = cons_0069();
    _v1.e_c_union(_v2);
    return (_v1);
  }

  A_except1 cons_0070()
  {
    A_except1 _v1;
    _v1 = new A_except1();
    _v1.v1 = -200;
    _v1.v2 = 200;
    _v1.v3 = -200000;
    _v1.v4 = 200000;
    _v1.v5 = 1.234f;
    _v1.v6 = 1.23e4;
    _v1.v7 = 'b';
    _v1.v8 = true;
    _v1.v9 = 20;
    return (_v1);
  }

  A_except2 cons_0071()
  {
    A_except2 _v1;
    _v1 = new A_except2();
    _v1.v10 = orb.create_any();
    _v1.v10.insert_long(-200000);
    _v1.v11 = "def";
    _v1.v12 = target;
    return (_v1);
  }

  B_except cons_0072()
  {
    B_except _v1;
    _v1 = new B_except();
    _v1.v = B.b2;
    return (_v1);
  }

  C_struct cons_0074()
  {
    C_struct _v1;
    _v1 = new C_struct();
    _v1.e_short = -200;
    _v1.e_ushort = 200;
    _v1.e_long = -200000;
    _v1.e_ulong = 200000;
    _v1.e_float = 1.234f;
    _v1.e_double = 1.23e4;
    _v1.e_char = 'b';
    _v1.e_boolean = true;
    _v1.e_octet = 20;
    _v1.e_any = orb.create_any();
    _v1.e_any.insert_long(-200000);
    _v1.e_string = "def";
    _v1.e_Object = target;
    return (_v1);
  }

  C_union cons_0075()
  {
    C_union _v1;
    _v1 = new C_union();

    short _v2;
    _v2 = 200;
    _v1.e_ushort(_v2);
    return (_v1);
  }

  C_except cons_0073()
  {
    C_except _v1;
    _v1 = new C_except();
    _v1.v1 = cons_0074();
    _v1.v2 = cons_0075();
    _v1.v3 = cons_0075();
    _v1.v4 = cons_0075();
    _v1.v5 = cons_0075();
    _v1.v6 = cons_0075();
    _v1.v7 = cons_0075();
    _v1.v8 = cons_0075();
    _v1.v9 = cons_0075();
    _v1.v10 = cons_0075();
    _v1.v11 = cons_0075();
    _v1.v12 = cons_0075();
    _v1.v13 = cons_0075();
    _v1.v16 = new short[ 2 ];
    _v1.v16 [ 0 ] = -200;
    _v1.v16 [ 1 ] = -200;
    _v1.v17 = new short[ 2 ];
    _v1.v17 [ 0 ] = 200;
    _v1.v17 [ 1 ] = 200;
    _v1.v18 = new int[ 2 ];
    _v1.v18 [ 0 ] = -200000;
    _v1.v18 [ 1 ] = -200000;
    _v1.v19 = new int[ 2 ];
    _v1.v19 [ 0 ] = 200000;
    _v1.v19 [ 1 ] = 200000;
    _v1.v20 = new float[ 2 ];
    _v1.v20 [ 0 ] = 1.234f;
    _v1.v20 [ 1 ] = 1.234f;
    _v1.v21 = new double[ 2 ];
    _v1.v21 [ 0 ] = 1.23e4;
    _v1.v21 [ 1 ] = 1.23e4;
    _v1.v22 = new char[ 2 ];
    _v1.v22 [ 0 ] = 'b';
    _v1.v22 [ 1 ] = 'b';
    _v1.v23 = new boolean[ 2 ];
    _v1.v23 [ 0 ] = true;
    _v1.v23 [ 1 ] = true;
    _v1.v24 = new byte[ 2 ];
    _v1.v24 [ 0 ] = 20;
    _v1.v24 [ 1 ] = 20;
    _v1.v25 = new org.omg.CORBA.Any[ 2 ];
    _v1.v25 [ 0 ] = orb.create_any();
    _v1.v25 [ 0 ].insert_long(-200000);
    _v1.v25 [ 1 ] = orb.create_any();
    _v1.v25 [ 1 ].insert_long(-200000);
    _v1.v26 = new String[ 2 ];
    _v1.v26 [ 0 ] = "def";
    _v1.v26 [ 1 ] = "def";
    _v1.v27 = new org.omg.CORBA.Object[ 2 ];
    _v1.v27 [ 0 ] = target;
    _v1.v27 [ 1 ] = target;
    _v1.v30 = new short[ 2 ];
    _v1.v30 [ 0 ] = -200;
    _v1.v30 [ 1 ] = -200;
    _v1.v31 = new short[ 2 ];
    _v1.v31 [ 0 ] = 200;
    _v1.v31 [ 1 ] = 200;
    _v1.v32 = new int[ 2 ];
    _v1.v32 [ 0 ] = -200000;
    _v1.v32 [ 1 ] = -200000;
    _v1.v33 = new int[ 2 ];
    _v1.v33 [ 0 ] = 200000;
    _v1.v33 [ 1 ] = 200000;
    _v1.v34 = new float[ 2 ];
    _v1.v34 [ 0 ] = 1.234f;
    _v1.v34 [ 1 ] = 1.234f;
    _v1.v35 = new double[ 2 ];
    _v1.v35 [ 0 ] = 1.23e4;
    _v1.v35 [ 1 ] = 1.23e4;
    _v1.v36 = new char[ 2 ];
    _v1.v36 [ 0 ] = 'b';
    _v1.v36 [ 1 ] = 'b';
    _v1.v37 = new boolean[ 2 ];
    _v1.v37 [ 0 ] = true;
    _v1.v37 [ 1 ] = true;
    _v1.v38 = new byte[ 2 ];
    _v1.v38 [ 0 ] = 20;
    _v1.v38 [ 1 ] = 20;
    _v1.v39 = new org.omg.CORBA.Any[ 2 ];
    _v1.v39 [ 0 ] = orb.create_any();
    _v1.v39 [ 0 ].insert_long(-200000);
    _v1.v39 [ 1 ] = orb.create_any();
    _v1.v39 [ 1 ].insert_long(-200000);
    _v1.v40 = new String[ 2 ];
    _v1.v40 [ 0 ] = "def";
    _v1.v40 [ 1 ] = "def";
    _v1.v41 = new org.omg.CORBA.Object[ 2 ];
    _v1.v41 [ 0 ] = target;
    _v1.v41 [ 1 ] = target;
    return (_v1);
  }

  D_d_short cons_0077()
  {
    D_d_short _v1;
    _v1 = new D_d_short();

    int _v2;
    _v2 = -200000;
    _v1.l2(_v2);
    return (_v1);
  }

  D_d_ushort cons_0078()
  {
    D_d_ushort _v1;
    _v1 = new D_d_ushort();

    int _v2;
    _v2 = -200000;
    _v1.l2(_v2);
    return (_v1);
  }

  D_d_long cons_0079()
  {
    D_d_long _v1;
    _v1 = new D_d_long();

    int _v2;
    _v2 = -200000;
    _v1.l2(_v2);
    return (_v1);
  }

  D_d_ulong cons_0080()
  {
    D_d_ulong _v1;
    _v1 = new D_d_ulong();

    int _v2;
    _v2 = -200000;
    _v1.l2(_v2);
    return (_v1);
  }

  D_d_char cons_0081()
  {
    D_d_char _v1;
    _v1 = new D_d_char();

    int _v2;
    _v2 = -200000;
    _v1.l2(_v2);
    return (_v1);
  }

  D_d_boolean cons_0082()
  {
    D_d_boolean _v1;
    _v1 = new D_d_boolean();

    int _v2;
    _v2 = -200000;
    _v1.l1(_v2);
    return (_v1);
  }

  D_d_B cons_0083()
  {
    D_d_B _v1;
    _v1 = new D_d_B();

    int _v2;
    _v2 = -200000;
    _v1.l2(_v2);
    return (_v1);
  }

  D_except cons_0076()
  {
    D_except _v1;
    _v1 = new D_except();
    _v1.v1 = cons_0077();
    _v1.v2 = cons_0078();
    _v1.v3 = cons_0079();
    _v1.v4 = cons_0080();
    _v1.v5 = cons_0081();
    _v1.v6 = cons_0082();
    _v1.v7 = cons_0083();
    return (_v1);
  }

  E_struct cons_0085()
  {
    E_struct _v1;
    _v1 = new E_struct();
    _v1.e_b1 = B.b2;
    _v1.e_b2 = B.b2;
    return (_v1);
  }

  E_union cons_0086()
  {
    E_union _v1;
    _v1 = new E_union();

    B _v2;
    _v2 = B.b2;
    _v1.e_b2(_v2);
    return (_v1);
  }

  E_except cons_0084()
  {
    E_except _v1;
    _v1 = new E_except();
    _v1.v1 = cons_0085();
    _v1.v2 = cons_0086();
    _v1.v3 = new B[ 2 ];
    _v1.v3 [ 0 ] = B.b2;
    _v1.v3 [ 1 ] = B.b2;
    _v1.v4 = new B[ 2 ];
    _v1.v4 [ 0 ] = B.b2;
    _v1.v4 [ 1 ] = B.b2;
    return (_v1);
  }

  C_struct cons_0089()
  {
    C_struct _v1;
    _v1 = new C_struct();
    _v1.e_short = -200;
    _v1.e_ushort = 200;
    _v1.e_long = -200000;
    _v1.e_ulong = 200000;
    _v1.e_float = 1.234f;
    _v1.e_double = 1.23e4;
    _v1.e_char = 'b';
    _v1.e_boolean = true;
    _v1.e_octet = 20;
    _v1.e_any = orb.create_any();
    _v1.e_any.insert_long(-200000);
    _v1.e_string = "def";
    _v1.e_Object = target;
    return (_v1);
  }

  C_union cons_0090()
  {
    C_union _v1;
    _v1 = new C_union();

    short _v2;
    _v2 = 200;
    _v1.e_ushort(_v2);
    return (_v1);
  }

  F_struct cons_0088()
  {
    F_struct _v1;
    _v1 = new F_struct();
    _v1.e_c_struct = cons_0089();
    _v1.e_c_union = cons_0090();
    _v1.e_c_sequence_e_short = new short[ 2 ];
    _v1.e_c_sequence_e_short [ 0 ] = -200;
    _v1.e_c_sequence_e_short [ 1 ] = -200;
    _v1.e_c_sequence_e_ushort = new short[ 2 ];
    _v1.e_c_sequence_e_ushort [ 0 ] = 200;
    _v1.e_c_sequence_e_ushort [ 1 ] = 200;
    _v1.e_c_sequence_e_long = new int[ 2 ];
    _v1.e_c_sequence_e_long [ 0 ] = -200000;
    _v1.e_c_sequence_e_long [ 1 ] = -200000;
    _v1.e_c_sequence_e_ulong = new int[ 2 ];
    _v1.e_c_sequence_e_ulong [ 0 ] = 200000;
    _v1.e_c_sequence_e_ulong [ 1 ] = 200000;
    _v1.e_c_sequence_e_float = new float[ 2 ];
    _v1.e_c_sequence_e_float [ 0 ] = 1.234f;
    _v1.e_c_sequence_e_float [ 1 ] = 1.234f;
    _v1.e_c_sequence_e_double = new double[ 2 ];
    _v1.e_c_sequence_e_double [ 0 ] = 1.23e4;
    _v1.e_c_sequence_e_double [ 1 ] = 1.23e4;
    _v1.e_c_sequence_e_char = new char[ 2 ];
    _v1.e_c_sequence_e_char [ 0 ] = 'b';
    _v1.e_c_sequence_e_char [ 1 ] = 'b';
    _v1.e_c_sequence_e_boolean = new boolean[ 2 ];
    _v1.e_c_sequence_e_boolean [ 0 ] = true;
    _v1.e_c_sequence_e_boolean [ 1 ] = true;
    _v1.e_c_sequence_e_octet = new byte[ 2 ];
    _v1.e_c_sequence_e_octet [ 0 ] = 20;
    _v1.e_c_sequence_e_octet [ 1 ] = 20;
    _v1.e_c_sequence_e_any = new org.omg.CORBA.Any[ 2 ];
    _v1.e_c_sequence_e_any [ 0 ] = orb.create_any();
    _v1.e_c_sequence_e_any [ 0 ].insert_long(-200000);
    _v1.e_c_sequence_e_any [ 1 ] = orb.create_any();
    _v1.e_c_sequence_e_any [ 1 ].insert_long(-200000);
    _v1.e_c_sequence_e_string = new String[ 2 ];
    _v1.e_c_sequence_e_string [ 0 ] = "def";
    _v1.e_c_sequence_e_string [ 1 ] = "def";
    _v1.e_c_sequence_e_Object = new org.omg.CORBA.Object[ 2 ];
    _v1.e_c_sequence_e_Object [ 0 ] = target;
    _v1.e_c_sequence_e_Object [ 1 ] = target;
    _v1.e_c_array_e_short = new short[ 2 ];
    _v1.e_c_array_e_short [ 0 ] = -200;
    _v1.e_c_array_e_short [ 1 ] = -200;
    _v1.e_c_array_e_ushort = new short[ 2 ];
    _v1.e_c_array_e_ushort [ 0 ] = 200;
    _v1.e_c_array_e_ushort [ 1 ] = 200;
    _v1.e_c_array_e_long = new int[ 2 ];
    _v1.e_c_array_e_long [ 0 ] = -200000;
    _v1.e_c_array_e_long [ 1 ] = -200000;
    _v1.e_c_array_e_ulong = new int[ 2 ];
    _v1.e_c_array_e_ulong [ 0 ] = 200000;
    _v1.e_c_array_e_ulong [ 1 ] = 200000;
    _v1.e_c_array_e_float = new float[ 2 ];
    _v1.e_c_array_e_float [ 0 ] = 1.234f;
    _v1.e_c_array_e_float [ 1 ] = 1.234f;
    _v1.e_c_array_e_double = new double[ 2 ];
    _v1.e_c_array_e_double [ 0 ] = 1.23e4;
    _v1.e_c_array_e_double [ 1 ] = 1.23e4;
    _v1.e_c_array_e_char = new char[ 2 ];
    _v1.e_c_array_e_char [ 0 ] = 'b';
    _v1.e_c_array_e_char [ 1 ] = 'b';
    _v1.e_c_array_e_boolean = new boolean[ 2 ];
    _v1.e_c_array_e_boolean [ 0 ] = true;
    _v1.e_c_array_e_boolean [ 1 ] = true;
    _v1.e_c_array_e_octet = new byte[ 2 ];
    _v1.e_c_array_e_octet [ 0 ] = 20;
    _v1.e_c_array_e_octet [ 1 ] = 20;
    _v1.e_c_array_e_any = new org.omg.CORBA.Any[ 2 ];
    _v1.e_c_array_e_any [ 0 ] = orb.create_any();
    _v1.e_c_array_e_any [ 0 ].insert_long(-200000);
    _v1.e_c_array_e_any [ 1 ] = orb.create_any();
    _v1.e_c_array_e_any [ 1 ].insert_long(-200000);
    _v1.e_c_array_e_string = new String[ 2 ];
    _v1.e_c_array_e_string [ 0 ] = "def";
    _v1.e_c_array_e_string [ 1 ] = "def";
    _v1.e_c_array_e_Object = new org.omg.CORBA.Object[ 2 ];
    _v1.e_c_array_e_Object [ 0 ] = target;
    _v1.e_c_array_e_Object [ 1 ] = target;
    return (_v1);
  }

  F_union cons_0091()
  {
    F_union _v1;
    _v1 = new F_union();

    C_union _v2;
    _v2 = cons_0090();
    _v1.e_c_union(_v2);
    return (_v1);
  }

  F_except1 cons_0087()
  {
    F_except1 _v1;
    _v1 = new F_except1();
    _v1.v1 = cons_0088();
    _v1.v2 = cons_0091();
    _v1.v3 = cons_0091();
    _v1.v4 = cons_0091();
    _v1.v5 = cons_0091();
    _v1.v6 = cons_0091();
    _v1.v7 = cons_0091();
    _v1.v8 = cons_0091();
    _v1.v9 = cons_0091();
    _v1.v10 = cons_0091();
    _v1.v11 = cons_0091();
    _v1.v12 = cons_0091();
    _v1.v13 = cons_0091();
    _v1.v14 = cons_0091();
    _v1.v15 = cons_0091();
    _v1.v18 = cons_0091();
    _v1.v19 = cons_0091();
    _v1.v20 = cons_0091();
    _v1.v21 = cons_0091();
    _v1.v22 = cons_0091();
    _v1.v23 = cons_0091();
    _v1.v24 = cons_0091();
    _v1.v25 = cons_0091();
    _v1.v26 = cons_0091();
    _v1.v27 = cons_0091();
    _v1.v28 = cons_0091();
    _v1.v29 = cons_0091();
    return (_v1);
  }

  C_struct cons_0093()
  {
    C_struct _v1;
    _v1 = new C_struct();
    _v1.e_short = -200;
    _v1.e_ushort = 200;
    _v1.e_long = -200000;
    _v1.e_ulong = 200000;
    _v1.e_float = 1.234f;
    _v1.e_double = 1.23e4;
    _v1.e_char = 'b';
    _v1.e_boolean = true;
    _v1.e_octet = 20;
    _v1.e_any = orb.create_any();
    _v1.e_any.insert_long(-200000);
    _v1.e_string = "def";
    _v1.e_Object = target;
    return (_v1);
  }

  C_union cons_0094()
  {
    C_union _v1;
    _v1 = new C_union();

    short _v2;
    _v2 = 200;
    _v1.e_ushort(_v2);
    return (_v1);
  }

  F_except2 cons_0092()
  {
    F_except2 _v1;
    _v1 = new F_except2();
    _v1.v32 = new C_struct[ 2 ];
    _v1.v32 [ 0 ] = cons_0093();
    _v1.v32 [ 1 ] = cons_0093();
    _v1.v33 = new C_union[ 2 ];
    _v1.v33 [ 0 ] = cons_0094();
    _v1.v33 [ 1 ] = cons_0094();
    return (_v1);
  }

  C_struct cons_0096()
  {
    C_struct _v1;
    _v1 = new C_struct();
    _v1.e_short = -200;
    _v1.e_ushort = 200;
    _v1.e_long = -200000;
    _v1.e_ulong = 200000;
    _v1.e_float = 1.234f;
    _v1.e_double = 1.23e4;
    _v1.e_char = 'b';
    _v1.e_boolean = true;
    _v1.e_octet = 20;
    _v1.e_any = orb.create_any();
    _v1.e_any.insert_long(-200000);
    _v1.e_string = "def";
    _v1.e_Object = target;
    return (_v1);
  }

  C_union cons_0097()
  {
    C_union _v1;
    _v1 = new C_union();

    short _v2;
    _v2 = 200;
    _v1.e_ushort(_v2);
    return (_v1);
  }

  F_except3 cons_0095()
  {
    F_except3 _v1;
    _v1 = new F_except3();
    _v1.v62 = new C_struct[ 2 ];
    _v1.v62 [ 0 ] = cons_0096();
    _v1.v62 [ 1 ] = cons_0096();
    _v1.v63 = new C_union[ 2 ];
    _v1.v63 [ 0 ] = cons_0097();
    _v1.v63 [ 1 ] = cons_0097();
    return (_v1);
  }

  E_struct cons_0100()
  {
    E_struct _v1;
    _v1 = new E_struct();
    _v1.e_b1 = B.b2;
    _v1.e_b2 = B.b2;
    return (_v1);
  }

  E_union cons_0101()
  {
    E_union _v1;
    _v1 = new E_union();

    B _v2;
    _v2 = B.b2;
    _v1.e_b2(_v2);
    return (_v1);
  }

  G_struct cons_0099()
  {
    G_struct _v1;
    _v1 = new G_struct();
    _v1.e_e_struct = cons_0100();
    _v1.e_e_union = cons_0101();
    _v1.e_e_sequence = new B[ 2 ];
    _v1.e_e_sequence [ 0 ] = B.b2;
    _v1.e_e_sequence [ 1 ] = B.b2;
    _v1.e_e_array = new B[ 2 ];
    _v1.e_e_array [ 0 ] = B.b2;
    _v1.e_e_array [ 1 ] = B.b2;
    return (_v1);
  }

  G_union cons_0102()
  {
    G_union _v1;
    _v1 = new G_union();

    E_union _v2;
    _v2 = cons_0101();
    _v1.e_e_union(_v2);
    return (_v1);
  }

  G_except cons_0098()
  {
    G_except _v1;
    _v1 = new G_except();
    _v1.v1 = cons_0099();
    _v1.v2 = cons_0102();
    _v1.v3 = cons_0102();
    _v1.v4 = cons_0102();
    _v1.v5 = cons_0102();
    _v1.v6 = new E_struct[ 2 ];
    _v1.v6 [ 0 ] = cons_0100();
    _v1.v6 [ 1 ] = cons_0100();
    _v1.v7 = new E_union[ 2 ];
    _v1.v7 [ 0 ] = cons_0101();
    _v1.v7 [ 1 ] = cons_0101();
    _v1.v10 = new E_struct[ 2 ];
    _v1.v10 [ 0 ] = cons_0100();
    _v1.v10 [ 1 ] = cons_0100();
    _v1.v11 = new E_union[ 2 ];
    _v1.v11 [ 0 ] = cons_0101();
    _v1.v11 [ 1 ] = cons_0101();
    return (_v1);
  }

  //operator definitions
  public short op1(short argin, org.omg.CORBA.ShortHolder argout,
                   org.omg.CORBA.ShortHolder arginout
                  )
  {
    if (!(argin == -100))
      {
        fail("argin value error in op1");
      }
    if (!(arginout.value == -100))
      {
        fail("arginout value error in op1");
      }
    argout.value = -200;
    arginout.value = -200;

    short _ret;
    _ret = -200;
    return (_ret);
  }

  public short op2(short argin, org.omg.CORBA.ShortHolder argout,
                   org.omg.CORBA.ShortHolder arginout
                  )
  {
    if (!(argin == 100))
      {
        fail("argin value error in op2");
      }
    if (!(arginout.value == 100))
      {
        fail("arginout value error in op2");
      }
    argout.value = 200;
    arginout.value = 200;

    short _ret;
    _ret = 200;
    return (_ret);
  }

  public int op3(int argin, org.omg.CORBA.IntHolder argout,
                 org.omg.CORBA.IntHolder arginout
                )
  {
    if (!(argin == -100000))
      {
        fail("argin value error in op3");
      }
    if (!(arginout.value == -100000))
      {
        fail("arginout value error in op3");
      }
    argout.value = -200000;
    arginout.value = -200000;

    int _ret;
    _ret = -200000;
    return (_ret);
  }

  public int op4(int argin, org.omg.CORBA.IntHolder argout,
                 org.omg.CORBA.IntHolder arginout
                )
  {
    if (!(argin == 100000))
      {
        fail("argin value error in op4");
      }
    if (!(arginout.value == 100000))
      {
        fail("arginout value error in op4");
      }
    argout.value = 200000;
    arginout.value = 200000;

    int _ret;
    _ret = 200000;
    return (_ret);
  }

  public float op5(float argin, org.omg.CORBA.FloatHolder argout,
                   org.omg.CORBA.FloatHolder arginout
                  )
  {
    if (!(argin == 0.123f))
      {
        fail("argin value error in op5");
      }
    if (!(arginout.value == 0.123f))
      {
        fail("arginout value error in op5");
      }
    argout.value = 1.234f;
    arginout.value = 1.234f;

    float _ret;
    _ret = 1.234f;
    return (_ret);
  }

  public double op6(double argin, org.omg.CORBA.DoubleHolder argout,
                    org.omg.CORBA.DoubleHolder arginout
                   )
  {
    if (!(argin == 0.12e3))
      {
        fail("argin value error in op6");
      }
    if (!(arginout.value == 0.12e3))
      {
        fail("arginout value error in op6");
      }
    argout.value = 1.23e4;
    arginout.value = 1.23e4;

    double _ret;
    _ret = 1.23e4;
    return (_ret);
  }

  public char op7(char argin, org.omg.CORBA.CharHolder argout,
                  org.omg.CORBA.CharHolder arginout
                 )
  {
    if (!(argin == 'a'))
      {
        fail("argin value error in op7");
      }
    if (!(arginout.value == 'a'))
      {
        fail("arginout value error in op7");
      }
    argout.value = 'b';
    arginout.value = 'b';

    char _ret;
    _ret = 'b';
    return (_ret);
  }

  public boolean op8(boolean argin, org.omg.CORBA.BooleanHolder argout,
                     org.omg.CORBA.BooleanHolder arginout
                    )
  {
    if (!(argin == false))
      {
        fail("argin value error in op8");
      }
    if (!(arginout.value == false))
      {
        fail("arginout value error in op8");
      }
    argout.value = true;
    arginout.value = true;

    boolean _ret;
    _ret = true;
    return (_ret);
  }

  public byte op9(byte argin, org.omg.CORBA.ByteHolder argout,
                  org.omg.CORBA.ByteHolder arginout
                 )
  {
    if (!(argin == 10))
      {
        fail("argin value error in op9");
      }
    if (!(arginout.value == 10))
      {
        fail("arginout value error in op9");
      }
    argout.value = 20;
    arginout.value = 20;

    byte _ret;
    _ret = 20;
    return (_ret);
  }

  public org.omg.CORBA.Any op10(org.omg.CORBA.Any argin,
                                org.omg.CORBA.AnyHolder argout,
                                org.omg.CORBA.AnyHolder arginout
                               )
  {
    if (!comp_0080(argin))
      {
        fail("argin value error in op10");
      }
    if (!comp_0080(arginout.value))
      {
        fail("arginout value error in op10");
      }
    argout.value = orb.create_any();
    argout.value.insert_long(-200000);
    arginout.value = orb.create_any();
    arginout.value.insert_long(-200000);

    org.omg.CORBA.Any _ret;
    _ret = orb.create_any();
    _ret.insert_long(-200000);
    return (_ret);
  }

  public String op11(String argin, org.omg.CORBA.StringHolder argout,
                     org.omg.CORBA.StringHolder arginout
                    )
  {
    if (!(argin.equals("abc")))
      {
        fail("argin value error in op11");
      }
    if (!(arginout.value.equals("abc")))
      {
        fail("arginout value error in op11");
      }
    argout.value = "def";
    arginout.value = "def";

    String _ret;
    _ret = "def";
    return (_ret);
  }

  public org.omg.CORBA.Object op12(org.omg.CORBA.Object argin,
                                   org.omg.CORBA.ObjectHolder argout,
                                   org.omg.CORBA.ObjectHolder arginout
                                  )
  {
    if (!(argin._is_equivalent(target)))
      {
        fail("argin value error in op12");
      }
    if (!(arginout.value._is_equivalent(target)))
      {
        fail("arginout value error in op12");
      }
    argout.value = target;
    arginout.value = target;

    org.omg.CORBA.Object _ret;
    _ret = target;
    return (_ret);
  }

  public org.omg.CORBA.TypeCode op13(org.omg.CORBA.TypeCode argin,
                                     org.omg.CORBA.TypeCodeHolder argout,
                                     org.omg.CORBA.TypeCodeHolder arginout
                                    )
  {
    if (!(orb.get_primitive_tc(TCKind.tk_string).equal(argin)))
      {
        fail("argin value error in op13");
      }
    if (!(orb.get_primitive_tc(TCKind.tk_string).equal(arginout.value)))
      {
        fail("arginout value error in op13");
      }
    argout.value = orb.get_primitive_tc(TCKind.tk_long);
    arginout.value = orb.get_primitive_tc(TCKind.tk_long);

    org.omg.CORBA.TypeCode _ret;
    _ret = orb.get_primitive_tc(TCKind.tk_long);
    return (_ret);
  }

  public B op15(B argin, BHolder argout, BHolder arginout)
  {
    if (!(argin == B.b1))
      {
        fail("argin value error in op15");
      }
    if (!(arginout.value == B.b1))
      {
        fail("arginout value error in op15");
      }
    argout.value = B.b2;
    arginout.value = B.b2;

    B _ret;
    _ret = B.b2;
    return (_ret);
  }

  public C_struct op16(C_struct argin, C_structHolder argout,
                       C_structHolder arginout
                      )
  {
    if (!comp_0081(argin))
      {
        fail("argin value error in op16");
      }
    if (!comp_0081(arginout.value))
      {
        fail("arginout value error in op16");
      }
    argout.value = cons_0035();
    arginout.value = cons_0035();

    C_struct _ret;
    _ret = cons_0035();
    return (_ret);
  }

  public C_union op17(C_union argin, C_unionHolder argout,
                      C_unionHolder arginout
                     )
  {
    if (!comp_0083(argin))
      {
        fail("argin value error in op17");
      }
    if (!comp_0083(arginout.value))
      {
        fail("arginout value error in op17");
      }
    argout.value = cons_0036();
    arginout.value = cons_0036();

    C_union _ret;
    _ret = cons_0036();
    return (_ret);
  }

  public short[] op18(short[] argin, C_sequence_e_shortHolder argout,
                      C_sequence_e_shortHolder arginout
                     )
  {
    if (!(true && (argin [ 0 ] == -100) && (argin [ 1 ] == -100)))
      {
        fail("argin value error in op18");
      }
    if (!(
          true && (arginout.value [ 0 ] == -100) &&
          (arginout.value [ 1 ] == -100)
        )
       )
      {
        fail("arginout value error in op18");
      }
    argout.value = new short[ 2 ];
    argout.value [ 0 ] = -200;
    argout.value [ 1 ] = -200;
    arginout.value = new short[ 2 ];
    arginout.value [ 0 ] = -200;
    arginout.value [ 1 ] = -200;

    short[] _ret;
    _ret = new short[ 2 ];
    _ret [ 0 ] = -200;
    _ret [ 1 ] = -200;
    return (_ret);
  }

  public short[] op19(short[] argin, C_sequence_e_ushortHolder argout,
                      C_sequence_e_ushortHolder arginout
                     )
  {
    if (!(true && (argin [ 0 ] == 100) && (argin [ 1 ] == 100)))
      {
        fail("argin value error in op19");
      }
    if (!(
          true && (arginout.value [ 0 ] == 100) &&
          (arginout.value [ 1 ] == 100)
        )
       )
      {
        fail("arginout value error in op19");
      }
    argout.value = new short[ 2 ];
    argout.value [ 0 ] = 200;
    argout.value [ 1 ] = 200;
    arginout.value = new short[ 2 ];
    arginout.value [ 0 ] = 200;
    arginout.value [ 1 ] = 200;

    short[] _ret;
    _ret = new short[ 2 ];
    _ret [ 0 ] = 200;
    _ret [ 1 ] = 200;
    return (_ret);
  }

  public int[] op20(int[] argin, C_sequence_e_longHolder argout,
                    C_sequence_e_longHolder arginout
                   )
  {
    if (!(true && (argin [ 0 ] == -100000) && (argin [ 1 ] == -100000)))
      {
        fail("argin value error in op20");
      }
    if (!(
          true && (arginout.value [ 0 ] == -100000) &&
          (arginout.value [ 1 ] == -100000)
        )
       )
      {
        fail("arginout value error in op20");
      }
    argout.value = new int[ 2 ];
    argout.value [ 0 ] = -200000;
    argout.value [ 1 ] = -200000;
    arginout.value = new int[ 2 ];
    arginout.value [ 0 ] = -200000;
    arginout.value [ 1 ] = -200000;

    int[] _ret;
    _ret = new int[ 2 ];
    _ret [ 0 ] = -200000;
    _ret [ 1 ] = -200000;
    return (_ret);
  }

  public int[] op21(int[] argin, C_sequence_e_ulongHolder argout,
                    C_sequence_e_ulongHolder arginout
                   )
  {
    if (!(true && (argin [ 0 ] == 100000) && (argin [ 1 ] == 100000)))
      {
        fail("argin value error in op21");
      }
    if (!(
          true && (arginout.value [ 0 ] == 100000) &&
          (arginout.value [ 1 ] == 100000)
        )
       )
      {
        fail("arginout value error in op21");
      }
    argout.value = new int[ 2 ];
    argout.value [ 0 ] = 200000;
    argout.value [ 1 ] = 200000;
    arginout.value = new int[ 2 ];
    arginout.value [ 0 ] = 200000;
    arginout.value [ 1 ] = 200000;

    int[] _ret;
    _ret = new int[ 2 ];
    _ret [ 0 ] = 200000;
    _ret [ 1 ] = 200000;
    return (_ret);
  }

  public float[] op22(float[] argin, C_sequence_e_floatHolder argout,
                      C_sequence_e_floatHolder arginout
                     )
  {
    if (!(true && (argin [ 0 ] == 0.123f) && (argin [ 1 ] == 0.123f)))
      {
        fail("argin value error in op22");
      }
    if (!(
          true && (arginout.value [ 0 ] == 0.123f) &&
          (arginout.value [ 1 ] == 0.123f)
        )
       )
      {
        fail("arginout value error in op22");
      }
    argout.value = new float[ 2 ];
    argout.value [ 0 ] = 1.234f;
    argout.value [ 1 ] = 1.234f;
    arginout.value = new float[ 2 ];
    arginout.value [ 0 ] = 1.234f;
    arginout.value [ 1 ] = 1.234f;

    float[] _ret;
    _ret = new float[ 2 ];
    _ret [ 0 ] = 1.234f;
    _ret [ 1 ] = 1.234f;
    return (_ret);
  }

  public double[] op23(double[] argin, C_sequence_e_doubleHolder argout,
                       C_sequence_e_doubleHolder arginout
                      )
  {
    if (!(true && (argin [ 0 ] == 0.12e3) && (argin [ 1 ] == 0.12e3)))
      {
        fail("argin value error in op23");
      }
    if (!(
          true && (arginout.value [ 0 ] == 0.12e3) &&
          (arginout.value [ 1 ] == 0.12e3)
        )
       )
      {
        fail("arginout value error in op23");
      }
    argout.value = new double[ 2 ];
    argout.value [ 0 ] = 1.23e4;
    argout.value [ 1 ] = 1.23e4;
    arginout.value = new double[ 2 ];
    arginout.value [ 0 ] = 1.23e4;
    arginout.value [ 1 ] = 1.23e4;

    double[] _ret;
    _ret = new double[ 2 ];
    _ret [ 0 ] = 1.23e4;
    _ret [ 1 ] = 1.23e4;
    return (_ret);
  }

  public char[] op24(char[] argin, C_sequence_e_charHolder argout,
                     C_sequence_e_charHolder arginout
                    )
  {
    if (!(true && (argin [ 0 ] == 'a') && (argin [ 1 ] == 'a')))
      {
        fail("argin value error in op24");
      }
    if (!(
          true && (arginout.value [ 0 ] == 'a') &&
          (arginout.value [ 1 ] == 'a')
        )
       )
      {
        fail("arginout value error in op24");
      }
    argout.value = new char[ 2 ];
    argout.value [ 0 ] = 'b';
    argout.value [ 1 ] = 'b';
    arginout.value = new char[ 2 ];
    arginout.value [ 0 ] = 'b';
    arginout.value [ 1 ] = 'b';

    char[] _ret;
    _ret = new char[ 2 ];
    _ret [ 0 ] = 'b';
    _ret [ 1 ] = 'b';
    return (_ret);
  }

  public boolean[] op25(boolean[] argin, C_sequence_e_booleanHolder argout,
                        C_sequence_e_booleanHolder arginout
                       )
  {
    if (!(true && (argin [ 0 ] == false) && (argin [ 1 ] == false)))
      {
        fail("argin value error in op25");
      }
    if (!(
          true && (arginout.value [ 0 ] == false) &&
          (arginout.value [ 1 ] == false)
        )
       )
      {
        fail("arginout value error in op25");
      }
    argout.value = new boolean[ 2 ];
    argout.value [ 0 ] = true;
    argout.value [ 1 ] = true;
    arginout.value = new boolean[ 2 ];
    arginout.value [ 0 ] = true;
    arginout.value [ 1 ] = true;

    boolean[] _ret;
    _ret = new boolean[ 2 ];
    _ret [ 0 ] = true;
    _ret [ 1 ] = true;
    return (_ret);
  }

  public byte[] op26(byte[] argin, C_sequence_e_octetHolder argout,
                     C_sequence_e_octetHolder arginout
                    )
  {
    if (!(true && (argin [ 0 ] == 10) && (argin [ 1 ] == 10)))
      {
        fail("argin value error in op26");
      }
    if (!(
          true && (arginout.value [ 0 ] == 10) && (arginout.value [ 1 ] == 10)
        ))
      {
        fail("arginout value error in op26");
      }
    argout.value = new byte[ 2 ];
    argout.value [ 0 ] = 20;
    argout.value [ 1 ] = 20;
    arginout.value = new byte[ 2 ];
    arginout.value [ 0 ] = 20;
    arginout.value [ 1 ] = 20;

    byte[] _ret;
    _ret = new byte[ 2 ];
    _ret [ 0 ] = 20;
    _ret [ 1 ] = 20;
    return (_ret);
  }

  public org.omg.CORBA.Any[] op27(org.omg.CORBA.Any[] argin,
                                  C_sequence_e_anyHolder argout,
                                  C_sequence_e_anyHolder arginout
                                 )
  {
    if (!(true && comp_0084(argin [ 0 ]) && comp_0084(argin [ 1 ])))
      {
        fail("argin value error in op27");
      }
    if (!(
          true && comp_0084(arginout.value [ 0 ]) &&
          comp_0084(arginout.value [ 1 ])
        )
       )
      {
        fail("arginout value error in op27");
      }
    argout.value = new org.omg.CORBA.Any[ 2 ];
    argout.value [ 0 ] = orb.create_any();
    argout.value [ 0 ].insert_long(-200000);
    argout.value [ 1 ] = orb.create_any();
    argout.value [ 1 ].insert_long(-200000);
    arginout.value = new org.omg.CORBA.Any[ 2 ];
    arginout.value [ 0 ] = orb.create_any();
    arginout.value [ 0 ].insert_long(-200000);
    arginout.value [ 1 ] = orb.create_any();
    arginout.value [ 1 ].insert_long(-200000);

    org.omg.CORBA.Any[] _ret;
    _ret = new org.omg.CORBA.Any[ 2 ];
    _ret [ 0 ] = orb.create_any();
    _ret [ 0 ].insert_long(-200000);
    _ret [ 1 ] = orb.create_any();
    _ret [ 1 ].insert_long(-200000);
    return (_ret);
  }

  public String[] op28(String[] argin, C_sequence_e_stringHolder argout,
                       C_sequence_e_stringHolder arginout
                      )
  {
    if (!(true && (argin [ 0 ].equals("abc")) && (argin [ 1 ].equals("abc"))))
      {
        fail("argin value error in op28");
      }
    if (!(
          true && (arginout.value [ 0 ].equals("abc")) &&
          (arginout.value [ 1 ].equals("abc"))
        )
       )
      {
        fail("arginout value error in op28");
      }
    argout.value = new String[ 2 ];
    argout.value [ 0 ] = "def";
    argout.value [ 1 ] = "def";
    arginout.value = new String[ 2 ];
    arginout.value [ 0 ] = "def";
    arginout.value [ 1 ] = "def";

    String[] _ret;
    _ret = new String[ 2 ];
    _ret [ 0 ] = "def";
    _ret [ 1 ] = "def";
    return (_ret);
  }

  public org.omg.CORBA.Object[] op29(org.omg.CORBA.Object[] argin,
                                     C_sequence_e_ObjectHolder argout,
                                     C_sequence_e_ObjectHolder arginout
                                    )
  {
    if (!(
          true && (argin [ 0 ]._is_equivalent(target)) &&
          (argin [ 1 ]._is_equivalent(target))
        )
       )
      {
        fail("argin value error in op29");
      }
    if (!(
          true && (arginout.value [ 0 ]._is_equivalent(target)) &&
          (arginout.value [ 1 ]._is_equivalent(target))
        )
       )
      {
        fail("arginout value error in op29");
      }
    argout.value = new org.omg.CORBA.Object[ 2 ];
    argout.value [ 0 ] = target;
    argout.value [ 1 ] = target;
    arginout.value = new org.omg.CORBA.Object[ 2 ];
    arginout.value [ 0 ] = target;
    arginout.value [ 1 ] = target;

    org.omg.CORBA.Object[] _ret;
    _ret = new org.omg.CORBA.Object[ 2 ];
    _ret [ 0 ] = target;
    _ret [ 1 ] = target;
    return (_ret);
  }

  public short[] op32(short[] argin, C_array_e_shortHolder argout,
                      C_array_e_shortHolder arginout
                     )
  {
    if (!(true && (argin [ 0 ] == -100) && (argin [ 1 ] == -100)))
      {
        fail("argin value error in op32");
      }
    if (!(
          true && (arginout.value [ 0 ] == -100) &&
          (arginout.value [ 1 ] == -100)
        )
       )
      {
        fail("arginout value error in op32");
      }
    argout.value = new short[ 2 ];
    argout.value [ 0 ] = -200;
    argout.value [ 1 ] = -200;
    arginout.value = new short[ 2 ];
    arginout.value [ 0 ] = -200;
    arginout.value [ 1 ] = -200;

    short[] _ret;
    _ret = new short[ 2 ];
    _ret [ 0 ] = -200;
    _ret [ 1 ] = -200;
    return (_ret);
  }

  public short[] op33(short[] argin, C_array_e_ushortHolder argout,
                      C_array_e_ushortHolder arginout
                     )
  {
    if (!(true && (argin [ 0 ] == 100) && (argin [ 1 ] == 100)))
      {
        fail("argin value error in op33");
      }
    if (!(
          true && (arginout.value [ 0 ] == 100) &&
          (arginout.value [ 1 ] == 100)
        )
       )
      {
        fail("arginout value error in op33");
      }
    argout.value = new short[ 2 ];
    argout.value [ 0 ] = 200;
    argout.value [ 1 ] = 200;
    arginout.value = new short[ 2 ];
    arginout.value [ 0 ] = 200;
    arginout.value [ 1 ] = 200;

    short[] _ret;
    _ret = new short[ 2 ];
    _ret [ 0 ] = 200;
    _ret [ 1 ] = 200;
    return (_ret);
  }

  public int[] op34(int[] argin, C_array_e_longHolder argout,
                    C_array_e_longHolder arginout
                   )
  {
    if (!(true && (argin [ 0 ] == -100000) && (argin [ 1 ] == -100000)))
      {
        fail("argin value error in op34");
      }
    if (!(
          true && (arginout.value [ 0 ] == -100000) &&
          (arginout.value [ 1 ] == -100000)
        )
       )
      {
        fail("arginout value error in op34");
      }
    argout.value = new int[ 2 ];
    argout.value [ 0 ] = -200000;
    argout.value [ 1 ] = -200000;
    arginout.value = new int[ 2 ];
    arginout.value [ 0 ] = -200000;
    arginout.value [ 1 ] = -200000;

    int[] _ret;
    _ret = new int[ 2 ];
    _ret [ 0 ] = -200000;
    _ret [ 1 ] = -200000;
    return (_ret);
  }

  public int[] op35(int[] argin, C_array_e_ulongHolder argout,
                    C_array_e_ulongHolder arginout
                   )
  {
    if (!(true && (argin [ 0 ] == 100000) && (argin [ 1 ] == 100000)))
      {
        fail("argin value error in op35");
      }
    if (!(
          true && (arginout.value [ 0 ] == 100000) &&
          (arginout.value [ 1 ] == 100000)
        )
       )
      {
        fail("arginout value error in op35");
      }
    argout.value = new int[ 2 ];
    argout.value [ 0 ] = 200000;
    argout.value [ 1 ] = 200000;
    arginout.value = new int[ 2 ];
    arginout.value [ 0 ] = 200000;
    arginout.value [ 1 ] = 200000;

    int[] _ret;
    _ret = new int[ 2 ];
    _ret [ 0 ] = 200000;
    _ret [ 1 ] = 200000;
    return (_ret);
  }

  public float[] op36(float[] argin, C_array_e_floatHolder argout,
                      C_array_e_floatHolder arginout
                     )
  {
    if (!(true && (argin [ 0 ] == 0.123f) && (argin [ 1 ] == 0.123f)))
      {
        fail("argin value error in op36");
      }
    if (!(
          true && (arginout.value [ 0 ] == 0.123f) &&
          (arginout.value [ 1 ] == 0.123f)
        )
       )
      {
        fail("arginout value error in op36");
      }
    argout.value = new float[ 2 ];
    argout.value [ 0 ] = 1.234f;
    argout.value [ 1 ] = 1.234f;
    arginout.value = new float[ 2 ];
    arginout.value [ 0 ] = 1.234f;
    arginout.value [ 1 ] = 1.234f;

    float[] _ret;
    _ret = new float[ 2 ];
    _ret [ 0 ] = 1.234f;
    _ret [ 1 ] = 1.234f;
    return (_ret);
  }

  public double[] op37(double[] argin, C_array_e_doubleHolder argout,
                       C_array_e_doubleHolder arginout
                      )
  {
    if (!(true && (argin [ 0 ] == 0.12e3) && (argin [ 1 ] == 0.12e3)))
      {
        fail("argin value error in op37");
      }
    if (!(
          true && (arginout.value [ 0 ] == 0.12e3) &&
          (arginout.value [ 1 ] == 0.12e3)
        )
       )
      {
        fail("arginout value error in op37");
      }
    argout.value = new double[ 2 ];
    argout.value [ 0 ] = 1.23e4;
    argout.value [ 1 ] = 1.23e4;
    arginout.value = new double[ 2 ];
    arginout.value [ 0 ] = 1.23e4;
    arginout.value [ 1 ] = 1.23e4;

    double[] _ret;
    _ret = new double[ 2 ];
    _ret [ 0 ] = 1.23e4;
    _ret [ 1 ] = 1.23e4;
    return (_ret);
  }

  public char[] op38(char[] argin, C_array_e_charHolder argout,
                     C_array_e_charHolder arginout
                    )
  {
    if (!(true && (argin [ 0 ] == 'a') && (argin [ 1 ] == 'a')))
      {
        fail("argin value error in op38");
      }
    if (!(
          true && (arginout.value [ 0 ] == 'a') &&
          (arginout.value [ 1 ] == 'a')
        )
       )
      {
        fail("arginout value error in op38");
      }
    argout.value = new char[ 2 ];
    argout.value [ 0 ] = 'b';
    argout.value [ 1 ] = 'b';
    arginout.value = new char[ 2 ];
    arginout.value [ 0 ] = 'b';
    arginout.value [ 1 ] = 'b';

    char[] _ret;
    _ret = new char[ 2 ];
    _ret [ 0 ] = 'b';
    _ret [ 1 ] = 'b';
    return (_ret);
  }

  public boolean[] op39(boolean[] argin, C_array_e_booleanHolder argout,
                        C_array_e_booleanHolder arginout
                       )
  {
    if (!(true && (argin [ 0 ] == false) && (argin [ 1 ] == false)))
      {
        fail("argin value error in op39");
      }
    if (!(
          true && (arginout.value [ 0 ] == false) &&
          (arginout.value [ 1 ] == false)
        )
       )
      {
        fail("arginout value error in op39");
      }
    argout.value = new boolean[ 2 ];
    argout.value [ 0 ] = true;
    argout.value [ 1 ] = true;
    arginout.value = new boolean[ 2 ];
    arginout.value [ 0 ] = true;
    arginout.value [ 1 ] = true;

    boolean[] _ret;
    _ret = new boolean[ 2 ];
    _ret [ 0 ] = true;
    _ret [ 1 ] = true;
    return (_ret);
  }

  public byte[] op40(byte[] argin, C_array_e_octetHolder argout,
                     C_array_e_octetHolder arginout
                    )
  {
    if (!(true && (argin [ 0 ] == 10) && (argin [ 1 ] == 10)))
      {
        fail("argin value error in op40");
      }
    if (!(
          true && (arginout.value [ 0 ] == 10) && (arginout.value [ 1 ] == 10)
        ))
      {
        fail("arginout value error in op40");
      }
    argout.value = new byte[ 2 ];
    argout.value [ 0 ] = 20;
    argout.value [ 1 ] = 20;
    arginout.value = new byte[ 2 ];
    arginout.value [ 0 ] = 20;
    arginout.value [ 1 ] = 20;

    byte[] _ret;
    _ret = new byte[ 2 ];
    _ret [ 0 ] = 20;
    _ret [ 1 ] = 20;
    return (_ret);
  }

  public org.omg.CORBA.Any[] op41(org.omg.CORBA.Any[] argin,
                                  C_array_e_anyHolder argout,
                                  C_array_e_anyHolder arginout
                                 )
  {
    if (!(true && comp_0085(argin [ 0 ]) && comp_0085(argin [ 1 ])))
      {
        fail("argin value error in op41");
      }
    if (!(
          true && comp_0085(arginout.value [ 0 ]) &&
          comp_0085(arginout.value [ 1 ])
        )
       )
      {
        fail("arginout value error in op41");
      }
    argout.value = new org.omg.CORBA.Any[ 2 ];
    argout.value [ 0 ] = orb.create_any();
    argout.value [ 0 ].insert_long(-200000);
    argout.value [ 1 ] = orb.create_any();
    argout.value [ 1 ].insert_long(-200000);
    arginout.value = new org.omg.CORBA.Any[ 2 ];
    arginout.value [ 0 ] = orb.create_any();
    arginout.value [ 0 ].insert_long(-200000);
    arginout.value [ 1 ] = orb.create_any();
    arginout.value [ 1 ].insert_long(-200000);

    org.omg.CORBA.Any[] _ret;
    _ret = new org.omg.CORBA.Any[ 2 ];
    _ret [ 0 ] = orb.create_any();
    _ret [ 0 ].insert_long(-200000);
    _ret [ 1 ] = orb.create_any();
    _ret [ 1 ].insert_long(-200000);
    return (_ret);
  }

  public String[] op42(String[] argin, C_array_e_stringHolder argout,
                       C_array_e_stringHolder arginout
                      )
  {
    if (!(true && (argin [ 0 ].equals("abc")) && (argin [ 1 ].equals("abc"))))
      {
        fail("argin value error in op42");
      }
    if (!(
          true && (arginout.value [ 0 ].equals("abc")) &&
          (arginout.value [ 1 ].equals("abc"))
        )
       )
      {
        fail("arginout value error in op42");
      }
    argout.value = new String[ 2 ];
    argout.value [ 0 ] = "def";
    argout.value [ 1 ] = "def";
    arginout.value = new String[ 2 ];
    arginout.value [ 0 ] = "def";
    arginout.value [ 1 ] = "def";

    String[] _ret;
    _ret = new String[ 2 ];
    _ret [ 0 ] = "def";
    _ret [ 1 ] = "def";
    return (_ret);
  }

  public org.omg.CORBA.Object[] op43(org.omg.CORBA.Object[] argin,
                                     C_array_e_ObjectHolder argout,
                                     C_array_e_ObjectHolder arginout
                                    )
  {
    if (!(
          true && (argin [ 0 ]._is_equivalent(target)) &&
          (argin [ 1 ]._is_equivalent(target))
        )
       )
      {
        fail("argin value error in op43");
      }
    if (!(
          true && (arginout.value [ 0 ]._is_equivalent(target)) &&
          (arginout.value [ 1 ]._is_equivalent(target))
        )
       )
      {
        fail("arginout value error in op43");
      }
    argout.value = new org.omg.CORBA.Object[ 2 ];
    argout.value [ 0 ] = target;
    argout.value [ 1 ] = target;
    arginout.value = new org.omg.CORBA.Object[ 2 ];
    arginout.value [ 0 ] = target;
    arginout.value [ 1 ] = target;

    org.omg.CORBA.Object[] _ret;
    _ret = new org.omg.CORBA.Object[ 2 ];
    _ret [ 0 ] = target;
    _ret [ 1 ] = target;
    return (_ret);
  }

  public D_d_short op46(D_d_short argin, D_d_shortHolder argout,
                        D_d_shortHolder arginout
                       )
  {
    if (!comp_0086(argin))
      {
        fail("argin value error in op46");
      }
    if (!comp_0086(arginout.value))
      {
        fail("arginout value error in op46");
      }
    argout.value = cons_0037();
    arginout.value = cons_0037();

    D_d_short _ret;
    _ret = cons_0037();
    return (_ret);
  }

  public D_d_ushort op47(D_d_ushort argin, D_d_ushortHolder argout,
                         D_d_ushortHolder arginout
                        )
  {
    if (!comp_0087(argin))
      {
        fail("argin value error in op47");
      }
    if (!comp_0087(arginout.value))
      {
        fail("arginout value error in op47");
      }
    argout.value = cons_0038();
    arginout.value = cons_0038();

    D_d_ushort _ret;
    _ret = cons_0038();
    return (_ret);
  }

  public D_d_long op48(D_d_long argin, D_d_longHolder argout,
                       D_d_longHolder arginout
                      )
  {
    if (!comp_0088(argin))
      {
        fail("argin value error in op48");
      }
    if (!comp_0088(arginout.value))
      {
        fail("arginout value error in op48");
      }
    argout.value = cons_0039();
    arginout.value = cons_0039();

    D_d_long _ret;
    _ret = cons_0039();
    return (_ret);
  }

  public D_d_ulong op49(D_d_ulong argin, D_d_ulongHolder argout,
                        D_d_ulongHolder arginout
                       )
  {
    if (!comp_0089(argin))
      {
        fail("argin value error in op49");
      }
    if (!comp_0089(arginout.value))
      {
        fail("arginout value error in op49");
      }
    argout.value = cons_0040();
    arginout.value = cons_0040();

    D_d_ulong _ret;
    _ret = cons_0040();
    return (_ret);
  }

  public D_d_char op50(D_d_char argin, D_d_charHolder argout,
                       D_d_charHolder arginout
                      )
  {
    if (!comp_0090(argin))
      {
        fail("argin value error in op50");
      }
    if (!comp_0090(arginout.value))
      {
        fail("arginout value error in op50");
      }
    argout.value = cons_0041();
    arginout.value = cons_0041();

    D_d_char _ret;
    _ret = cons_0041();
    return (_ret);
  }

  public D_d_boolean op51(D_d_boolean argin, D_d_booleanHolder argout,
                          D_d_booleanHolder arginout
                         )
  {
    if (!comp_0091(argin))
      {
        fail("argin value error in op51");
      }
    if (!comp_0091(arginout.value))
      {
        fail("arginout value error in op51");
      }
    argout.value = cons_0042();
    arginout.value = cons_0042();

    D_d_boolean _ret;
    _ret = cons_0042();
    return (_ret);
  }

  public D_d_B op52(D_d_B argin, D_d_BHolder argout, D_d_BHolder arginout)
  {
    if (!comp_0092(argin))
      {
        fail("argin value error in op52");
      }
    if (!comp_0092(arginout.value))
      {
        fail("arginout value error in op52");
      }
    argout.value = cons_0043();
    arginout.value = cons_0043();

    D_d_B _ret;
    _ret = cons_0043();
    return (_ret);
  }

  public E_struct op53(E_struct argin, E_structHolder argout,
                       E_structHolder arginout
                      )
  {
    if (!comp_0093(argin))
      {
        fail("argin value error in op53");
      }
    if (!comp_0093(arginout.value))
      {
        fail("arginout value error in op53");
      }
    argout.value = cons_0044();
    arginout.value = cons_0044();

    E_struct _ret;
    _ret = cons_0044();
    return (_ret);
  }

  public E_union op54(E_union argin, E_unionHolder argout,
                      E_unionHolder arginout
                     )
  {
    if (!comp_0094(argin))
      {
        fail("argin value error in op54");
      }
    if (!comp_0094(arginout.value))
      {
        fail("arginout value error in op54");
      }
    argout.value = cons_0045();
    arginout.value = cons_0045();

    E_union _ret;
    _ret = cons_0045();
    return (_ret);
  }

  public B[] op55(B[] argin, E_sequenceHolder argout, E_sequenceHolder arginout)
  {
    if (!(true && (argin [ 0 ] == B.b1) && (argin [ 1 ] == B.b1)))
      {
        fail("argin value error in op55");
      }
    if (!(
          true && (arginout.value [ 0 ] == B.b1) &&
          (arginout.value [ 1 ] == B.b1)
        )
       )
      {
        fail("arginout value error in op55");
      }
    argout.value = new B[ 2 ];
    argout.value [ 0 ] = B.b2;
    argout.value [ 1 ] = B.b2;
    arginout.value = new B[ 2 ];
    arginout.value [ 0 ] = B.b2;
    arginout.value [ 1 ] = B.b2;

    B[] _ret;
    _ret = new B[ 2 ];
    _ret [ 0 ] = B.b2;
    _ret [ 1 ] = B.b2;
    return (_ret);
  }

  public B[] op56(B[] argin, E_arrayHolder argout, E_arrayHolder arginout)
  {
    if (!(true && (argin [ 0 ] == B.b1) && (argin [ 1 ] == B.b1)))
      {
        fail("argin value error in op56");
      }
    if (!(
          true && (arginout.value [ 0 ] == B.b1) &&
          (arginout.value [ 1 ] == B.b1)
        )
       )
      {
        fail("arginout value error in op56");
      }
    argout.value = new B[ 2 ];
    argout.value [ 0 ] = B.b2;
    argout.value [ 1 ] = B.b2;
    arginout.value = new B[ 2 ];
    arginout.value [ 0 ] = B.b2;
    arginout.value [ 1 ] = B.b2;

    B[] _ret;
    _ret = new B[ 2 ];
    _ret [ 0 ] = B.b2;
    _ret [ 1 ] = B.b2;
    return (_ret);
  }

  public F_struct op57(F_struct argin, F_structHolder argout,
                       F_structHolder arginout
                      )
  {
    if (!comp_0095(argin))
      {
        fail("argin value error in op57");
      }
    if (!comp_0095(arginout.value))
      {
        fail("arginout value error in op57");
      }
    argout.value = cons_0046();
    arginout.value = cons_0046();

    F_struct _ret;
    _ret = cons_0046();
    return (_ret);
  }

  public F_union op58(F_union argin, F_unionHolder argout,
                      F_unionHolder arginout
                     )
  {
    if (!comp_0099(argin))
      {
        fail("argin value error in op58");
      }
    if (!comp_0099(arginout.value))
      {
        fail("arginout value error in op58");
      }
    argout.value = cons_0049();
    arginout.value = cons_0049();

    F_union _ret;
    _ret = cons_0049();
    return (_ret);
  }

  public C_struct[] op59(C_struct[] argin, F_sequence_e_c_structHolder argout,
                         F_sequence_e_c_structHolder arginout
                        )
  {
    if (!(true && comp_0102(argin [ 0 ]) && comp_0102(argin [ 1 ])))
      {
        fail("argin value error in op59");
      }
    if (!(
          true && comp_0102(arginout.value [ 0 ]) &&
          comp_0102(arginout.value [ 1 ])
        )
       )
      {
        fail("arginout value error in op59");
      }
    argout.value = new C_struct[ 2 ];
    argout.value [ 0 ] = cons_0051();
    argout.value [ 1 ] = cons_0051();
    arginout.value = new C_struct[ 2 ];
    arginout.value [ 0 ] = cons_0051();
    arginout.value [ 1 ] = cons_0051();

    C_struct[] _ret;
    _ret = new C_struct[ 2 ];
    _ret [ 0 ] = cons_0051();
    _ret [ 1 ] = cons_0051();
    return (_ret);
  }

  public C_union[] op60(C_union[] argin, F_sequence_e_c_unionHolder argout,
                        F_sequence_e_c_unionHolder arginout
                       )
  {
    if (!(true && comp_0104(argin [ 0 ]) && comp_0104(argin [ 1 ])))
      {
        fail("argin value error in op60");
      }
    if (!(
          true && comp_0104(arginout.value [ 0 ]) &&
          comp_0104(arginout.value [ 1 ])
        )
       )
      {
        fail("arginout value error in op60");
      }
    argout.value = new C_union[ 2 ];
    argout.value [ 0 ] = cons_0052();
    argout.value [ 1 ] = cons_0052();
    arginout.value = new C_union[ 2 ];
    arginout.value [ 0 ] = cons_0052();
    arginout.value [ 1 ] = cons_0052();

    C_union[] _ret;
    _ret = new C_union[ 2 ];
    _ret [ 0 ] = cons_0052();
    _ret [ 1 ] = cons_0052();
    return (_ret);
  }

  public C_struct[] op89(C_struct[] argin, F_array_e_c_structHolder argout,
                         F_array_e_c_structHolder arginout
                        )
  {
    if (!(true && comp_0105(argin [ 0 ]) && comp_0105(argin [ 1 ])))
      {
        fail("argin value error in op89");
      }
    if (!(
          true && comp_0105(arginout.value [ 0 ]) &&
          comp_0105(arginout.value [ 1 ])
        )
       )
      {
        fail("arginout value error in op89");
      }
    argout.value = new C_struct[ 2 ];
    argout.value [ 0 ] = cons_0053();
    argout.value [ 1 ] = cons_0053();
    arginout.value = new C_struct[ 2 ];
    arginout.value [ 0 ] = cons_0053();
    arginout.value [ 1 ] = cons_0053();

    C_struct[] _ret;
    _ret = new C_struct[ 2 ];
    _ret [ 0 ] = cons_0053();
    _ret [ 1 ] = cons_0053();
    return (_ret);
  }

  public C_union[] op90(C_union[] argin, F_array_e_c_unionHolder argout,
                        F_array_e_c_unionHolder arginout
                       )
  {
    if (!(true && comp_0107(argin [ 0 ]) && comp_0107(argin [ 1 ])))
      {
        fail("argin value error in op90");
      }
    if (!(
          true && comp_0107(arginout.value [ 0 ]) &&
          comp_0107(arginout.value [ 1 ])
        )
       )
      {
        fail("arginout value error in op90");
      }
    argout.value = new C_union[ 2 ];
    argout.value [ 0 ] = cons_0054();
    argout.value [ 1 ] = cons_0054();
    arginout.value = new C_union[ 2 ];
    arginout.value [ 0 ] = cons_0054();
    arginout.value [ 1 ] = cons_0054();

    C_union[] _ret;
    _ret = new C_union[ 2 ];
    _ret [ 0 ] = cons_0054();
    _ret [ 1 ] = cons_0054();
    return (_ret);
  }

  public G_struct op119(G_struct argin, G_structHolder argout,
                        G_structHolder arginout
                       )
  {
    if (!comp_0108(argin))
      {
        fail("argin value error in op119");
      }
    if (!comp_0108(arginout.value))
      {
        fail("arginout value error in op119");
      }
    argout.value = cons_0055();
    arginout.value = cons_0055();

    G_struct _ret;
    _ret = cons_0055();
    return (_ret);
  }

  public G_union op120(G_union argin, G_unionHolder argout,
                       G_unionHolder arginout
                      )
  {
    if (!comp_0111(argin))
      {
        fail("argin value error in op120");
      }
    if (!comp_0111(arginout.value))
      {
        fail("arginout value error in op120");
      }
    argout.value = cons_0058();
    arginout.value = cons_0058();

    G_union _ret;
    _ret = cons_0058();
    return (_ret);
  }

  public E_struct[] op121(E_struct[] argin, G_sequence_e_e_structHolder argout,
                          G_sequence_e_e_structHolder arginout
                         )
  {
    if (!(true && comp_0113(argin [ 0 ]) && comp_0113(argin [ 1 ])))
      {
        fail("argin value error in op121");
      }
    if (!(
          true && comp_0113(arginout.value [ 0 ]) &&
          comp_0113(arginout.value [ 1 ])
        )
       )
      {
        fail("arginout value error in op121");
      }
    argout.value = new E_struct[ 2 ];
    argout.value [ 0 ] = cons_0060();
    argout.value [ 1 ] = cons_0060();
    arginout.value = new E_struct[ 2 ];
    arginout.value [ 0 ] = cons_0060();
    arginout.value [ 1 ] = cons_0060();

    E_struct[] _ret;
    _ret = new E_struct[ 2 ];
    _ret [ 0 ] = cons_0060();
    _ret [ 1 ] = cons_0060();
    return (_ret);
  }

  public E_union[] op122(E_union[] argin, G_sequence_e_e_unionHolder argout,
                         G_sequence_e_e_unionHolder arginout
                        )
  {
    if (!(true && comp_0114(argin [ 0 ]) && comp_0114(argin [ 1 ])))
      {
        fail("argin value error in op122");
      }
    if (!(
          true && comp_0114(arginout.value [ 0 ]) &&
          comp_0114(arginout.value [ 1 ])
        )
       )
      {
        fail("arginout value error in op122");
      }
    argout.value = new E_union[ 2 ];
    argout.value [ 0 ] = cons_0061();
    argout.value [ 1 ] = cons_0061();
    arginout.value = new E_union[ 2 ];
    arginout.value [ 0 ] = cons_0061();
    arginout.value [ 1 ] = cons_0061();

    E_union[] _ret;
    _ret = new E_union[ 2 ];
    _ret [ 0 ] = cons_0061();
    _ret [ 1 ] = cons_0061();
    return (_ret);
  }

  public E_struct[] op125(E_struct[] argin, G_array_e_e_structHolder argout,
                          G_array_e_e_structHolder arginout
                         )
  {
    if (!(true && comp_0115(argin [ 0 ]) && comp_0115(argin [ 1 ])))
      {
        fail("argin value error in op125");
      }
    if (!(
          true && comp_0115(arginout.value [ 0 ]) &&
          comp_0115(arginout.value [ 1 ])
        )
       )
      {
        fail("arginout value error in op125");
      }
    argout.value = new E_struct[ 2 ];
    argout.value [ 0 ] = cons_0062();
    argout.value [ 1 ] = cons_0062();
    arginout.value = new E_struct[ 2 ];
    arginout.value [ 0 ] = cons_0062();
    arginout.value [ 1 ] = cons_0062();

    E_struct[] _ret;
    _ret = new E_struct[ 2 ];
    _ret [ 0 ] = cons_0062();
    _ret [ 1 ] = cons_0062();
    return (_ret);
  }

  public E_union[] op126(E_union[] argin, G_array_e_e_unionHolder argout,
                         G_array_e_e_unionHolder arginout
                        )
  {
    if (!(true && comp_0116(argin [ 0 ]) && comp_0116(argin [ 1 ])))
      {
        fail("argin value error in op126");
      }
    if (!(
          true && comp_0116(arginout.value [ 0 ]) &&
          comp_0116(arginout.value [ 1 ])
        )
       )
      {
        fail("arginout value error in op126");
      }
    argout.value = new E_union[ 2 ];
    argout.value [ 0 ] = cons_0063();
    argout.value [ 1 ] = cons_0063();
    arginout.value = new E_union[ 2 ];
    arginout.value [ 0 ] = cons_0063();
    arginout.value [ 1 ] = cons_0063();

    E_union[] _ret;
    _ret = new E_union[ 2 ];
    _ret [ 0 ] = cons_0063();
    _ret [ 1 ] = cons_0063();
    return (_ret);
  }

  public F_union op129(F_union argin, F_unionHolder argout,
                       F_unionHolder arginout
                      )
  {
    if (!comp_0117(argin))
      {
        fail("argin value error in op129");
      }
    if (!comp_0117(arginout.value))
      {
        fail("arginout value error in op129");
      }
    argout.value = cons_0064();
    arginout.value = cons_0064();

    F_union _ret;
    _ret = cons_0064();
    return (_ret);
  }

  public F_union op130(F_union argin, F_unionHolder argout,
                       F_unionHolder arginout
                      )
  {
    if (!comp_0120(argin))
      {
        fail("argin value error in op130");
      }
    if (!comp_0120(arginout.value))
      {
        fail("arginout value error in op130");
      }
    argout.value = cons_0066();
    arginout.value = cons_0066();

    F_union _ret;
    _ret = cons_0066();
    return (_ret);
  }

  public F_union op131(F_union argin, F_unionHolder argout,
                       F_unionHolder arginout
                      )
  {
    if (!comp_0123(argin))
      {
        fail("argin value error in op131");
      }
    if (!comp_0123(arginout.value))
      {
        fail("arginout value error in op131");
      }
    argout.value = cons_0068();
    arginout.value = cons_0068();

    F_union _ret;
    _ret = cons_0068();
    return (_ret);
  }

  public void excop1()
              throws A_except1
  {
    A_except1 _exc;
    _exc = cons_0070();
    throw (_exc);
  }

  public void excop2()
              throws A_except2
  {
    A_except2 _exc;
    _exc = cons_0071();
    throw (_exc);
  }

  public void excop3()
              throws B_except
  {
    B_except _exc;
    _exc = cons_0072();
    throw (_exc);
  }

  public void excop4()
              throws C_except
  {
    C_except _exc;
    _exc = cons_0073();
    throw (_exc);
  }

  public void excop5()
              throws D_except
  {
    D_except _exc;
    _exc = cons_0076();
    throw (_exc);
  }

  public void excop6()
              throws E_except
  {
    E_except _exc;
    _exc = cons_0084();
    throw (_exc);
  }

  public void excop7()
              throws F_except1
  {
    F_except1 _exc;
    _exc = cons_0087();
    throw (_exc);
  }

  public void excop8()
              throws F_except2
  {
    F_except2 _exc;
    _exc = cons_0092();
    throw (_exc);
  }

  public void excop9()
              throws F_except3
  {
    F_except3 _exc;
    _exc = cons_0095();
    throw (_exc);
  }

  public void excop10()
               throws G_except
  {
    G_except _exc;
    _exc = cons_0098();
    throw (_exc);
  }
}
