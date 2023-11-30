// Tags: not-a-test
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

package gnu.testlet.org.omg.CORBA.ORB.RF11;

public interface rf11Operations
{
  // A
  short op1(short argin, org.omg.CORBA.ShortHolder argout,
            org.omg.CORBA.ShortHolder arginout
           );

  short op2(short argin, org.omg.CORBA.ShortHolder argout,
            org.omg.CORBA.ShortHolder arginout
           );

  int op3(int argin, org.omg.CORBA.IntHolder argout,
          org.omg.CORBA.IntHolder arginout
         );

  int op4(int argin, org.omg.CORBA.IntHolder argout,
          org.omg.CORBA.IntHolder arginout
         );

  float op5(float argin, org.omg.CORBA.FloatHolder argout,
            org.omg.CORBA.FloatHolder arginout
           );

  double op6(double argin, org.omg.CORBA.DoubleHolder argout,
             org.omg.CORBA.DoubleHolder arginout
            );

  char op7(char argin, org.omg.CORBA.CharHolder argout,
           org.omg.CORBA.CharHolder arginout
          );

  boolean op8(boolean argin, org.omg.CORBA.BooleanHolder argout,
              org.omg.CORBA.BooleanHolder arginout
             );

  byte op9(byte argin, org.omg.CORBA.ByteHolder argout,
           org.omg.CORBA.ByteHolder arginout
          );

  org.omg.CORBA.Any op10(org.omg.CORBA.Any argin,
                         org.omg.CORBA.AnyHolder argout,
                         org.omg.CORBA.AnyHolder arginout
                        );

  String op11(String argin, org.omg.CORBA.StringHolder argout,
              org.omg.CORBA.StringHolder arginout
             );

  org.omg.CORBA.Object op12(org.omg.CORBA.Object argin,
                            org.omg.CORBA.ObjectHolder argout,
                            org.omg.CORBA.ObjectHolder arginout
                           );

  // B
  B op15(B argin, BHolder argout, BHolder arginout);

  // C
  C_struct op16(C_struct argin, C_structHolder argout, C_structHolder arginout);

  C_union op17(C_union argin, C_unionHolder argout, C_unionHolder arginout);

  short[] op18(short[] argin, C_sequence_e_shortHolder argout,
               C_sequence_e_shortHolder arginout
              );

  short[] op19(short[] argin, C_sequence_e_ushortHolder argout,
               C_sequence_e_ushortHolder arginout
              );

  int[] op20(int[] argin, C_sequence_e_longHolder argout,
             C_sequence_e_longHolder arginout
            );

  int[] op21(int[] argin, C_sequence_e_ulongHolder argout,
             C_sequence_e_ulongHolder arginout
            );

  float[] op22(float[] argin, C_sequence_e_floatHolder argout,
               C_sequence_e_floatHolder arginout
              );

  double[] op23(double[] argin, C_sequence_e_doubleHolder argout,
                C_sequence_e_doubleHolder arginout
               );

  char[] op24(char[] argin, C_sequence_e_charHolder argout,
              C_sequence_e_charHolder arginout
             );

  boolean[] op25(boolean[] argin, C_sequence_e_booleanHolder argout,
                 C_sequence_e_booleanHolder arginout
                );

  byte[] op26(byte[] argin, C_sequence_e_octetHolder argout,
              C_sequence_e_octetHolder arginout
             );

  org.omg.CORBA.Any[] op27(org.omg.CORBA.Any[] argin,
                           C_sequence_e_anyHolder argout,
                           C_sequence_e_anyHolder arginout
                          );

  String[] op28(String[] argin, C_sequence_e_stringHolder argout,
                C_sequence_e_stringHolder arginout
               );

  org.omg.CORBA.Object[] op29(org.omg.CORBA.Object[] argin,
                              C_sequence_e_ObjectHolder argout,
                              C_sequence_e_ObjectHolder arginout
                             );

  short[] op32(short[] argin, C_array_e_shortHolder argout,
               C_array_e_shortHolder arginout
              );

  short[] op33(short[] argin, C_array_e_ushortHolder argout,
               C_array_e_ushortHolder arginout
              );

  int[] op34(int[] argin, C_array_e_longHolder argout,
             C_array_e_longHolder arginout
            );

  int[] op35(int[] argin, C_array_e_ulongHolder argout,
             C_array_e_ulongHolder arginout
            );

  float[] op36(float[] argin, C_array_e_floatHolder argout,
               C_array_e_floatHolder arginout
              );

  double[] op37(double[] argin, C_array_e_doubleHolder argout,
                C_array_e_doubleHolder arginout
               );

  char[] op38(char[] argin, C_array_e_charHolder argout,
              C_array_e_charHolder arginout
             );

  boolean[] op39(boolean[] argin, C_array_e_booleanHolder argout,
                 C_array_e_booleanHolder arginout
                );

  byte[] op40(byte[] argin, C_array_e_octetHolder argout,
              C_array_e_octetHolder arginout
             );

  org.omg.CORBA.Any[] op41(org.omg.CORBA.Any[] argin,
                           C_array_e_anyHolder argout,
                           C_array_e_anyHolder arginout
                          );

  String[] op42(String[] argin, C_array_e_stringHolder argout,
                C_array_e_stringHolder arginout
               );

  org.omg.CORBA.Object[] op43(org.omg.CORBA.Object[] argin,
                              C_array_e_ObjectHolder argout,
                              C_array_e_ObjectHolder arginout
                             );

  // D
  D_d_short op46(D_d_short argin, D_d_shortHolder argout,
                 D_d_shortHolder arginout
                );

  D_d_ushort op47(D_d_ushort argin, D_d_ushortHolder argout,
                  D_d_ushortHolder arginout
                 );

  D_d_long op48(D_d_long argin, D_d_longHolder argout, D_d_longHolder arginout);

  D_d_ulong op49(D_d_ulong argin, D_d_ulongHolder argout,
                 D_d_ulongHolder arginout
                );

  D_d_char op50(D_d_char argin, D_d_charHolder argout, D_d_charHolder arginout);

  D_d_boolean op51(D_d_boolean argin, D_d_booleanHolder argout,
                   D_d_booleanHolder arginout
                  );

  D_d_B op52(D_d_B argin, D_d_BHolder argout, D_d_BHolder arginout);

  // E
  E_struct op53(E_struct argin, E_structHolder argout, E_structHolder arginout);

  E_union op54(E_union argin, E_unionHolder argout, E_unionHolder arginout);

  B[] op55(B[] argin, E_sequenceHolder argout, E_sequenceHolder arginout);

  B[] op56(B[] argin, E_arrayHolder argout, E_arrayHolder arginout);

  // F
  F_struct op57(F_struct argin, F_structHolder argout, F_structHolder arginout);

  F_union op58(F_union argin, F_unionHolder argout, F_unionHolder arginout);

  C_struct[] op59(C_struct[] argin, F_sequence_e_c_structHolder argout,
                  F_sequence_e_c_structHolder arginout
                 );

  C_union[] op60(C_union[] argin, F_sequence_e_c_unionHolder argout,
                 F_sequence_e_c_unionHolder arginout
                );

  C_struct[] op89(C_struct[] argin, F_array_e_c_structHolder argout,
                  F_array_e_c_structHolder arginout
                 );

  C_union[] op90(C_union[] argin, F_array_e_c_unionHolder argout,
                 F_array_e_c_unionHolder arginout
                );

  // G
  G_struct op119(G_struct argin, G_structHolder argout, G_structHolder arginout);

  G_union op120(G_union argin, G_unionHolder argout, G_unionHolder arginout);

  E_struct[] op121(E_struct[] argin, G_sequence_e_e_structHolder argout,
                   G_sequence_e_e_structHolder arginout
                  );

  E_union[] op122(E_union[] argin, G_sequence_e_e_unionHolder argout,
                  G_sequence_e_e_unionHolder arginout
                 );

  E_struct[] op125(E_struct[] argin, G_array_e_e_structHolder argout,
                   G_array_e_e_structHolder arginout
                  );

  E_union[] op126(E_union[] argin, G_array_e_e_unionHolder argout,
                  G_array_e_e_unionHolder arginout
                 );

  // rest of F
  F_union op129(F_union argin, F_unionHolder argout, F_unionHolder arginout);

  F_union op130(F_union argin, F_unionHolder argout, F_unionHolder arginout);

  F_union op131(F_union argin, F_unionHolder argout, F_unionHolder arginout);

  // pragma: exception=A_except1
  void excop1()
       throws A_except1;

  // pragma: exception=A_except2
  void excop2()
       throws A_except2;

  // pragma: exception=B_except
  void excop3()
       throws B_except;

  // pragma: exception=C_except
  void excop4()
       throws C_except;

  // pragma: exception=D_except
  void excop5()
       throws D_except;

  // pragma: exception=E_except
  void excop6()
       throws E_except;

  // pragma: exception=F_except1
  void excop7()
       throws F_except1;

  // pragma: exception=F_except2
  void excop8()
       throws F_except2;

  // pragma: exception=F_except3
  void excop9()
       throws F_except3;

  // pragma: exception=G_except
  void excop10()
        throws G_except;
} // interface rf11Operations
