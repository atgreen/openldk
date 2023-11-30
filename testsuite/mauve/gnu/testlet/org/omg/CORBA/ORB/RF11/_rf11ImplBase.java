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

public abstract class _rf11ImplBase
  extends org.omg.CORBA.portable.ObjectImpl
  implements NEC_RF11, org.omg.CORBA.portable.InvokeHandler
{
  // Constructors
  public _rf11ImplBase()
  {
  }

  private static java.util.Hashtable _methods = new java.util.Hashtable();

  static
  {
    _methods.put("op1", new java.lang.Integer(0));
    _methods.put("op2", new java.lang.Integer(1));
    _methods.put("op3", new java.lang.Integer(2));
    _methods.put("op4", new java.lang.Integer(3));
    _methods.put("op5", new java.lang.Integer(4));
    _methods.put("op6", new java.lang.Integer(5));
    _methods.put("op7", new java.lang.Integer(6));
    _methods.put("op8", new java.lang.Integer(7));
    _methods.put("op9", new java.lang.Integer(8));
    _methods.put("op10", new java.lang.Integer(9));
    _methods.put("op11", new java.lang.Integer(10));
    _methods.put("op12", new java.lang.Integer(11));
    _methods.put("op15", new java.lang.Integer(12));
    _methods.put("op16", new java.lang.Integer(13));
    _methods.put("op17", new java.lang.Integer(14));
    _methods.put("op18", new java.lang.Integer(15));
    _methods.put("op19", new java.lang.Integer(16));
    _methods.put("op20", new java.lang.Integer(17));
    _methods.put("op21", new java.lang.Integer(18));
    _methods.put("op22", new java.lang.Integer(19));
    _methods.put("op23", new java.lang.Integer(20));
    _methods.put("op24", new java.lang.Integer(21));
    _methods.put("op25", new java.lang.Integer(22));
    _methods.put("op26", new java.lang.Integer(23));
    _methods.put("op27", new java.lang.Integer(24));
    _methods.put("op28", new java.lang.Integer(25));
    _methods.put("op29", new java.lang.Integer(26));
    _methods.put("op32", new java.lang.Integer(27));
    _methods.put("op33", new java.lang.Integer(28));
    _methods.put("op34", new java.lang.Integer(29));
    _methods.put("op35", new java.lang.Integer(30));
    _methods.put("op36", new java.lang.Integer(31));
    _methods.put("op37", new java.lang.Integer(32));
    _methods.put("op38", new java.lang.Integer(33));
    _methods.put("op39", new java.lang.Integer(34));
    _methods.put("op40", new java.lang.Integer(35));
    _methods.put("op41", new java.lang.Integer(36));
    _methods.put("op42", new java.lang.Integer(37));
    _methods.put("op43", new java.lang.Integer(38));
    _methods.put("op46", new java.lang.Integer(39));
    _methods.put("op47", new java.lang.Integer(40));
    _methods.put("op48", new java.lang.Integer(41));
    _methods.put("op49", new java.lang.Integer(42));
    _methods.put("op50", new java.lang.Integer(43));
    _methods.put("op51", new java.lang.Integer(44));
    _methods.put("op52", new java.lang.Integer(45));
    _methods.put("op53", new java.lang.Integer(46));
    _methods.put("op54", new java.lang.Integer(47));
    _methods.put("op55", new java.lang.Integer(48));
    _methods.put("op56", new java.lang.Integer(49));
    _methods.put("op57", new java.lang.Integer(50));
    _methods.put("op58", new java.lang.Integer(51));
    _methods.put("op59", new java.lang.Integer(52));
    _methods.put("op60", new java.lang.Integer(53));
    _methods.put("op89", new java.lang.Integer(54));
    _methods.put("op90", new java.lang.Integer(55));
    _methods.put("op119", new java.lang.Integer(56));
    _methods.put("op120", new java.lang.Integer(57));
    _methods.put("op121", new java.lang.Integer(58));
    _methods.put("op122", new java.lang.Integer(59));
    _methods.put("op125", new java.lang.Integer(60));
    _methods.put("op126", new java.lang.Integer(61));
    _methods.put("op129", new java.lang.Integer(62));
    _methods.put("op130", new java.lang.Integer(63));
    _methods.put("op131", new java.lang.Integer(64));
    _methods.put("excop1", new java.lang.Integer(65));
    _methods.put("excop2", new java.lang.Integer(66));
    _methods.put("excop3", new java.lang.Integer(67));
    _methods.put("excop4", new java.lang.Integer(68));
    _methods.put("excop5", new java.lang.Integer(69));
    _methods.put("excop6", new java.lang.Integer(70));
    _methods.put("excop7", new java.lang.Integer(71));
    _methods.put("excop8", new java.lang.Integer(72));
    _methods.put("excop9", new java.lang.Integer(73));
    _methods.put("excop10", new java.lang.Integer(74));
  }

  public org.omg.CORBA.portable.OutputStream _invoke(String method,
                                                     org.omg.CORBA.portable.InputStream in,
                                                     org.omg.CORBA.portable.ResponseHandler rh
                                                    )
  {
    org.omg.CORBA.portable.OutputStream out = null;
    java.lang.Integer __method = (java.lang.Integer) _methods.get(method);
    if (__method == null)
      throw new org.omg.CORBA.BAD_OPERATION(0,
                                            org.omg.CORBA.CompletionStatus.COMPLETED_MAYBE
                                           );

    switch (__method.intValue())
      {
        // A
        case 0 : // rf11/op1
        {
          short argin = in.read_short();
          org.omg.CORBA.ShortHolder argout = new org.omg.CORBA.ShortHolder();
          org.omg.CORBA.ShortHolder arginout = new org.omg.CORBA.ShortHolder();
          arginout.value = in.read_short();

          short __result = (short) 0;
          __result = this.op1(argin, argout, arginout);
          out = rh.createReply();
          out.write_short(__result);
          out.write_short(argout.value);
          out.write_short(arginout.value);
          break;
        }

        case 1 : // rf11/op2
        {
          short argin = in.read_ushort();
          org.omg.CORBA.ShortHolder argout = new org.omg.CORBA.ShortHolder();
          org.omg.CORBA.ShortHolder arginout = new org.omg.CORBA.ShortHolder();
          arginout.value = in.read_ushort();

          short __result = (short) 0;
          __result = this.op2(argin, argout, arginout);
          out = rh.createReply();
          out.write_ushort(__result);
          out.write_ushort(argout.value);
          out.write_ushort(arginout.value);
          break;
        }

        case 2 : // rf11/op3
        {
          int argin = in.read_long();
          org.omg.CORBA.IntHolder argout = new org.omg.CORBA.IntHolder();
          org.omg.CORBA.IntHolder arginout = new org.omg.CORBA.IntHolder();
          arginout.value = in.read_long();

          int __result = (int) 0;
          __result = this.op3(argin, argout, arginout);
          out = rh.createReply();
          out.write_long(__result);
          out.write_long(argout.value);
          out.write_long(arginout.value);
          break;
        }

        case 3 : // rf11/op4
        {
          int argin = in.read_ulong();
          org.omg.CORBA.IntHolder argout = new org.omg.CORBA.IntHolder();
          org.omg.CORBA.IntHolder arginout = new org.omg.CORBA.IntHolder();
          arginout.value = in.read_ulong();

          int __result = (int) 0;
          __result = this.op4(argin, argout, arginout);
          out = rh.createReply();
          out.write_ulong(__result);
          out.write_ulong(argout.value);
          out.write_ulong(arginout.value);
          break;
        }

        case 4 : // rf11/op5
        {
          float argin = in.read_float();
          org.omg.CORBA.FloatHolder argout = new org.omg.CORBA.FloatHolder();
          org.omg.CORBA.FloatHolder arginout = new org.omg.CORBA.FloatHolder();
          arginout.value = in.read_float();

          float __result = (float) 0;
          __result = this.op5(argin, argout, arginout);
          out = rh.createReply();
          out.write_float(__result);
          out.write_float(argout.value);
          out.write_float(arginout.value);
          break;
        }

        case 5 : // rf11/op6
        {
          double argin = in.read_double();
          org.omg.CORBA.DoubleHolder argout = new org.omg.CORBA.DoubleHolder();
          org.omg.CORBA.DoubleHolder arginout =
            new org.omg.CORBA.DoubleHolder();
          arginout.value = in.read_double();

          double __result = (double) 0;
          __result = this.op6(argin, argout, arginout);
          out = rh.createReply();
          out.write_double(__result);
          out.write_double(argout.value);
          out.write_double(arginout.value);
          break;
        }

        case 6 : // rf11/op7
        {
          char argin = in.read_char();
          org.omg.CORBA.CharHolder argout = new org.omg.CORBA.CharHolder();
          org.omg.CORBA.CharHolder arginout = new org.omg.CORBA.CharHolder();
          arginout.value = in.read_char();

          char __result = (char) 0;
          __result = this.op7(argin, argout, arginout);
          out = rh.createReply();
          out.write_char(__result);
          out.write_char(argout.value);
          out.write_char(arginout.value);
          break;
        }

        case 7 : // rf11/op8
        {
          boolean argin = in.read_boolean();
          org.omg.CORBA.BooleanHolder argout =
            new org.omg.CORBA.BooleanHolder();
          org.omg.CORBA.BooleanHolder arginout =
            new org.omg.CORBA.BooleanHolder();
          arginout.value = in.read_boolean();

          boolean __result = false;
          __result = this.op8(argin, argout, arginout);
          out = rh.createReply();
          out.write_boolean(__result);
          out.write_boolean(argout.value);
          out.write_boolean(arginout.value);
          break;
        }

        case 8 : // rf11/op9
        {
          byte argin = in.read_octet();
          org.omg.CORBA.ByteHolder argout = new org.omg.CORBA.ByteHolder();
          org.omg.CORBA.ByteHolder arginout = new org.omg.CORBA.ByteHolder();
          arginout.value = in.read_octet();

          byte __result = (byte) 0;
          __result = this.op9(argin, argout, arginout);
          out = rh.createReply();
          out.write_octet(__result);
          out.write_octet(argout.value);
          out.write_octet(arginout.value);
          break;
        }

        case 9 : // rf11/op10
        {
          org.omg.CORBA.Any argin = in.read_any();
          org.omg.CORBA.AnyHolder argout = new org.omg.CORBA.AnyHolder();
          org.omg.CORBA.AnyHolder arginout = new org.omg.CORBA.AnyHolder();
          arginout.value = in.read_any();

          org.omg.CORBA.Any __result = null;
          __result = this.op10(argin, argout, arginout);
          out = rh.createReply();
          out.write_any(__result);
          out.write_any(argout.value);
          out.write_any(arginout.value);
          break;
        }

        case 10 : // rf11/op11
        {
          String argin = in.read_string();
          org.omg.CORBA.StringHolder argout = new org.omg.CORBA.StringHolder();
          org.omg.CORBA.StringHolder arginout =
            new org.omg.CORBA.StringHolder();
          arginout.value = in.read_string();

          String __result = null;
          __result = this.op11(argin, argout, arginout);
          out = rh.createReply();
          out.write_string(__result);
          out.write_string(argout.value);
          out.write_string(arginout.value);
          break;
        }

        case 11 : // rf11/op12
        {
          org.omg.CORBA.Object argin = org.omg.CORBA.ObjectHelper.read(in);
          org.omg.CORBA.ObjectHolder argout = new org.omg.CORBA.ObjectHolder();
          org.omg.CORBA.ObjectHolder arginout =
            new org.omg.CORBA.ObjectHolder();
          arginout.value = org.omg.CORBA.ObjectHelper.read(in);

          org.omg.CORBA.Object __result = null;
          __result = this.op12(argin, argout, arginout);
          out = rh.createReply();
          org.omg.CORBA.ObjectHelper.write(out, __result);
          org.omg.CORBA.ObjectHelper.write(out, argout.value);
          org.omg.CORBA.ObjectHelper.write(out, arginout.value);
          break;
        }

        // B
        case 12 : // rf11/op15
        {
          B argin = BHelper.read(in);
          BHolder argout = new BHolder();
          BHolder arginout = new BHolder();
          arginout.value = BHelper.read(in);

          B __result = null;
          __result = this.op15(argin, argout, arginout);
          out = rh.createReply();
          BHelper.write(out, __result);
          BHelper.write(out, argout.value);
          BHelper.write(out, arginout.value);
          break;
        }

        // C
        case 13 : // rf11/op16
        {
          C_struct argin = C_structHelper.read(in);
          C_structHolder argout = new C_structHolder();
          C_structHolder arginout = new C_structHolder();
          arginout.value = C_structHelper.read(in);

          C_struct __result = null;
          __result = this.op16(argin, argout, arginout);
          out = rh.createReply();
          C_structHelper.write(out, __result);
          C_structHelper.write(out, argout.value);
          C_structHelper.write(out, arginout.value);
          break;
        }

        case 14 : // rf11/op17
        {
          C_union argin = C_unionHelper.read(in);
          C_unionHolder argout = new C_unionHolder();
          C_unionHolder arginout = new C_unionHolder();
          arginout.value = C_unionHelper.read(in);

          C_union __result = null;
          __result = this.op17(argin, argout, arginout);
          out = rh.createReply();
          C_unionHelper.write(out, __result);
          C_unionHelper.write(out, argout.value);
          C_unionHelper.write(out, arginout.value);
          break;
        }

        case 15 : // rf11/op18
        {
          short[] argin = C_sequence_e_shortHelper.read(in);
          C_sequence_e_shortHolder argout = new C_sequence_e_shortHolder();
          C_sequence_e_shortHolder arginout = new C_sequence_e_shortHolder();
          arginout.value = C_sequence_e_shortHelper.read(in);

          short[] __result = null;
          __result = this.op18(argin, argout, arginout);
          out = rh.createReply();
          C_sequence_e_shortHelper.write(out, __result);
          C_sequence_e_shortHelper.write(out, argout.value);
          C_sequence_e_shortHelper.write(out, arginout.value);
          break;
        }

        case 16 : // rf11/op19
        {
          short[] argin = C_sequence_e_ushortHelper.read(in);
          C_sequence_e_ushortHolder argout = new C_sequence_e_ushortHolder();
          C_sequence_e_ushortHolder arginout = new C_sequence_e_ushortHolder();
          arginout.value = C_sequence_e_ushortHelper.read(in);

          short[] __result = null;
          __result = this.op19(argin, argout, arginout);
          out = rh.createReply();
          C_sequence_e_ushortHelper.write(out, __result);
          C_sequence_e_ushortHelper.write(out, argout.value);
          C_sequence_e_ushortHelper.write(out, arginout.value);
          break;
        }

        case 17 : // rf11/op20
        {
          int[] argin = C_sequence_e_longHelper.read(in);
          C_sequence_e_longHolder argout = new C_sequence_e_longHolder();
          C_sequence_e_longHolder arginout = new C_sequence_e_longHolder();
          arginout.value = C_sequence_e_longHelper.read(in);

          int[] __result = null;
          __result = this.op20(argin, argout, arginout);
          out = rh.createReply();
          C_sequence_e_longHelper.write(out, __result);
          C_sequence_e_longHelper.write(out, argout.value);
          C_sequence_e_longHelper.write(out, arginout.value);
          break;
        }

        case 18 : // rf11/op21
        {
          int[] argin = C_sequence_e_ulongHelper.read(in);
          C_sequence_e_ulongHolder argout = new C_sequence_e_ulongHolder();
          C_sequence_e_ulongHolder arginout = new C_sequence_e_ulongHolder();
          arginout.value = C_sequence_e_ulongHelper.read(in);

          int[] __result = null;
          __result = this.op21(argin, argout, arginout);
          out = rh.createReply();
          C_sequence_e_ulongHelper.write(out, __result);
          C_sequence_e_ulongHelper.write(out, argout.value);
          C_sequence_e_ulongHelper.write(out, arginout.value);
          break;
        }

        case 19 : // rf11/op22
        {
          float[] argin = C_sequence_e_floatHelper.read(in);
          C_sequence_e_floatHolder argout = new C_sequence_e_floatHolder();
          C_sequence_e_floatHolder arginout = new C_sequence_e_floatHolder();
          arginout.value = C_sequence_e_floatHelper.read(in);

          float[] __result = null;
          __result = this.op22(argin, argout, arginout);
          out = rh.createReply();
          C_sequence_e_floatHelper.write(out, __result);
          C_sequence_e_floatHelper.write(out, argout.value);
          C_sequence_e_floatHelper.write(out, arginout.value);
          break;
        }

        case 20 : // rf11/op23
        {
          double[] argin = C_sequence_e_doubleHelper.read(in);
          C_sequence_e_doubleHolder argout = new C_sequence_e_doubleHolder();
          C_sequence_e_doubleHolder arginout = new C_sequence_e_doubleHolder();
          arginout.value = C_sequence_e_doubleHelper.read(in);

          double[] __result = null;
          __result = this.op23(argin, argout, arginout);
          out = rh.createReply();
          C_sequence_e_doubleHelper.write(out, __result);
          C_sequence_e_doubleHelper.write(out, argout.value);
          C_sequence_e_doubleHelper.write(out, arginout.value);
          break;
        }

        case 21 : // rf11/op24
        {
          char[] argin = C_sequence_e_charHelper.read(in);
          C_sequence_e_charHolder argout = new C_sequence_e_charHolder();
          C_sequence_e_charHolder arginout = new C_sequence_e_charHolder();
          arginout.value = C_sequence_e_charHelper.read(in);

          char[] __result = null;
          __result = this.op24(argin, argout, arginout);
          out = rh.createReply();
          C_sequence_e_charHelper.write(out, __result);
          C_sequence_e_charHelper.write(out, argout.value);
          C_sequence_e_charHelper.write(out, arginout.value);
          break;
        }

        case 22 : // rf11/op25
        {
          boolean[] argin = C_sequence_e_booleanHelper.read(in);
          C_sequence_e_booleanHolder argout = new C_sequence_e_booleanHolder();
          C_sequence_e_booleanHolder arginout =
            new C_sequence_e_booleanHolder();
          arginout.value = C_sequence_e_booleanHelper.read(in);

          boolean[] __result = null;
          __result = this.op25(argin, argout, arginout);
          out = rh.createReply();
          C_sequence_e_booleanHelper.write(out, __result);
          C_sequence_e_booleanHelper.write(out, argout.value);
          C_sequence_e_booleanHelper.write(out, arginout.value);
          break;
        }

        case 23 : // rf11/op26
        {
          byte[] argin = C_sequence_e_octetHelper.read(in);
          C_sequence_e_octetHolder argout = new C_sequence_e_octetHolder();
          C_sequence_e_octetHolder arginout = new C_sequence_e_octetHolder();
          arginout.value = C_sequence_e_octetHelper.read(in);

          byte[] __result = null;
          __result = this.op26(argin, argout, arginout);
          out = rh.createReply();
          C_sequence_e_octetHelper.write(out, __result);
          C_sequence_e_octetHelper.write(out, argout.value);
          C_sequence_e_octetHelper.write(out, arginout.value);
          break;
        }

        case 24 : // rf11/op27
        {
          org.omg.CORBA.Any[] argin = C_sequence_e_anyHelper.read(in);
          C_sequence_e_anyHolder argout = new C_sequence_e_anyHolder();
          C_sequence_e_anyHolder arginout = new C_sequence_e_anyHolder();
          arginout.value = C_sequence_e_anyHelper.read(in);

          org.omg.CORBA.Any[] __result = null;
          __result = this.op27(argin, argout, arginout);
          out = rh.createReply();
          C_sequence_e_anyHelper.write(out, __result);
          C_sequence_e_anyHelper.write(out, argout.value);
          C_sequence_e_anyHelper.write(out, arginout.value);
          break;
        }

        case 25 : // rf11/op28
        {
          String[] argin = C_sequence_e_stringHelper.read(in);
          C_sequence_e_stringHolder argout = new C_sequence_e_stringHolder();
          C_sequence_e_stringHolder arginout = new C_sequence_e_stringHolder();
          arginout.value = C_sequence_e_stringHelper.read(in);

          String[] __result = null;
          __result = this.op28(argin, argout, arginout);
          out = rh.createReply();
          C_sequence_e_stringHelper.write(out, __result);
          C_sequence_e_stringHelper.write(out, argout.value);
          C_sequence_e_stringHelper.write(out, arginout.value);
          break;
        }

        case 26 : // rf11/op29
        {
          org.omg.CORBA.Object[] argin = C_sequence_e_ObjectHelper.read(in);
          C_sequence_e_ObjectHolder argout = new C_sequence_e_ObjectHolder();
          C_sequence_e_ObjectHolder arginout = new C_sequence_e_ObjectHolder();
          arginout.value = C_sequence_e_ObjectHelper.read(in);

          org.omg.CORBA.Object[] __result = null;
          __result = this.op29(argin, argout, arginout);
          out = rh.createReply();
          C_sequence_e_ObjectHelper.write(out, __result);
          C_sequence_e_ObjectHelper.write(out, argout.value);
          C_sequence_e_ObjectHelper.write(out, arginout.value);
          break;
        }

        case 27 : // rf11/op32
        {
          short[] argin = C_array_e_shortHelper.read(in);
          C_array_e_shortHolder argout = new C_array_e_shortHolder();
          C_array_e_shortHolder arginout = new C_array_e_shortHolder();
          arginout.value = C_array_e_shortHelper.read(in);

          short[] __result = null;
          __result = this.op32(argin, argout, arginout);
          out = rh.createReply();
          C_array_e_shortHelper.write(out, __result);
          C_array_e_shortHelper.write(out, argout.value);
          C_array_e_shortHelper.write(out, arginout.value);
          break;
        }

        case 28 : // rf11/op33
        {
          short[] argin = C_array_e_ushortHelper.read(in);
          C_array_e_ushortHolder argout = new C_array_e_ushortHolder();
          C_array_e_ushortHolder arginout = new C_array_e_ushortHolder();
          arginout.value = C_array_e_ushortHelper.read(in);

          short[] __result = null;
          __result = this.op33(argin, argout, arginout);
          out = rh.createReply();
          C_array_e_ushortHelper.write(out, __result);
          C_array_e_ushortHelper.write(out, argout.value);
          C_array_e_ushortHelper.write(out, arginout.value);
          break;
        }

        case 29 : // rf11/op34
        {
          int[] argin = C_array_e_longHelper.read(in);
          C_array_e_longHolder argout = new C_array_e_longHolder();
          C_array_e_longHolder arginout = new C_array_e_longHolder();
          arginout.value = C_array_e_longHelper.read(in);

          int[] __result = null;
          __result = this.op34(argin, argout, arginout);
          out = rh.createReply();
          C_array_e_longHelper.write(out, __result);
          C_array_e_longHelper.write(out, argout.value);
          C_array_e_longHelper.write(out, arginout.value);
          break;
        }

        case 30 : // rf11/op35
        {
          int[] argin = C_array_e_ulongHelper.read(in);
          C_array_e_ulongHolder argout = new C_array_e_ulongHolder();
          C_array_e_ulongHolder arginout = new C_array_e_ulongHolder();
          arginout.value = C_array_e_ulongHelper.read(in);

          int[] __result = null;
          __result = this.op35(argin, argout, arginout);
          out = rh.createReply();
          C_array_e_ulongHelper.write(out, __result);
          C_array_e_ulongHelper.write(out, argout.value);
          C_array_e_ulongHelper.write(out, arginout.value);
          break;
        }

        case 31 : // rf11/op36
        {
          float[] argin = C_array_e_floatHelper.read(in);
          C_array_e_floatHolder argout = new C_array_e_floatHolder();
          C_array_e_floatHolder arginout = new C_array_e_floatHolder();
          arginout.value = C_array_e_floatHelper.read(in);

          float[] __result = null;
          __result = this.op36(argin, argout, arginout);
          out = rh.createReply();
          C_array_e_floatHelper.write(out, __result);
          C_array_e_floatHelper.write(out, argout.value);
          C_array_e_floatHelper.write(out, arginout.value);
          break;
        }

        case 32 : // rf11/op37
        {
          double[] argin = C_array_e_doubleHelper.read(in);
          C_array_e_doubleHolder argout = new C_array_e_doubleHolder();
          C_array_e_doubleHolder arginout = new C_array_e_doubleHolder();
          arginout.value = C_array_e_doubleHelper.read(in);

          double[] __result = null;
          __result = this.op37(argin, argout, arginout);
          out = rh.createReply();
          C_array_e_doubleHelper.write(out, __result);
          C_array_e_doubleHelper.write(out, argout.value);
          C_array_e_doubleHelper.write(out, arginout.value);
          break;
        }

        case 33 : // rf11/op38
        {
          char[] argin = C_array_e_charHelper.read(in);
          C_array_e_charHolder argout = new C_array_e_charHolder();
          C_array_e_charHolder arginout = new C_array_e_charHolder();
          arginout.value = C_array_e_charHelper.read(in);

          char[] __result = null;
          __result = this.op38(argin, argout, arginout);
          out = rh.createReply();
          C_array_e_charHelper.write(out, __result);
          C_array_e_charHelper.write(out, argout.value);
          C_array_e_charHelper.write(out, arginout.value);
          break;
        }

        case 34 : // rf11/op39
        {
          boolean[] argin = C_array_e_booleanHelper.read(in);
          C_array_e_booleanHolder argout = new C_array_e_booleanHolder();
          C_array_e_booleanHolder arginout = new C_array_e_booleanHolder();
          arginout.value = C_array_e_booleanHelper.read(in);

          boolean[] __result = null;
          __result = this.op39(argin, argout, arginout);
          out = rh.createReply();
          C_array_e_booleanHelper.write(out, __result);
          C_array_e_booleanHelper.write(out, argout.value);
          C_array_e_booleanHelper.write(out, arginout.value);
          break;
        }

        case 35 : // rf11/op40
        {
          byte[] argin = C_array_e_octetHelper.read(in);
          C_array_e_octetHolder argout = new C_array_e_octetHolder();
          C_array_e_octetHolder arginout = new C_array_e_octetHolder();
          arginout.value = C_array_e_octetHelper.read(in);

          byte[] __result = null;
          __result = this.op40(argin, argout, arginout);
          out = rh.createReply();
          C_array_e_octetHelper.write(out, __result);
          C_array_e_octetHelper.write(out, argout.value);
          C_array_e_octetHelper.write(out, arginout.value);
          break;
        }

        case 36 : // rf11/op41
        {
          org.omg.CORBA.Any[] argin = C_array_e_anyHelper.read(in);
          C_array_e_anyHolder argout = new C_array_e_anyHolder();
          C_array_e_anyHolder arginout = new C_array_e_anyHolder();
          arginout.value = C_array_e_anyHelper.read(in);

          org.omg.CORBA.Any[] __result = null;
          __result = this.op41(argin, argout, arginout);
          out = rh.createReply();
          C_array_e_anyHelper.write(out, __result);
          C_array_e_anyHelper.write(out, argout.value);
          C_array_e_anyHelper.write(out, arginout.value);
          break;
        }

        case 37 : // rf11/op42
        {
          String[] argin = C_array_e_stringHelper.read(in);
          C_array_e_stringHolder argout = new C_array_e_stringHolder();
          C_array_e_stringHolder arginout = new C_array_e_stringHolder();
          arginout.value = C_array_e_stringHelper.read(in);

          String[] __result = null;
          __result = this.op42(argin, argout, arginout);
          out = rh.createReply();
          C_array_e_stringHelper.write(out, __result);
          C_array_e_stringHelper.write(out, argout.value);
          C_array_e_stringHelper.write(out, arginout.value);
          break;
        }

        case 38 : // rf11/op43
        {
          org.omg.CORBA.Object[] argin = C_array_e_ObjectHelper.read(in);
          C_array_e_ObjectHolder argout = new C_array_e_ObjectHolder();
          C_array_e_ObjectHolder arginout = new C_array_e_ObjectHolder();
          arginout.value = C_array_e_ObjectHelper.read(in);

          org.omg.CORBA.Object[] __result = null;
          __result = this.op43(argin, argout, arginout);
          out = rh.createReply();
          C_array_e_ObjectHelper.write(out, __result);
          C_array_e_ObjectHelper.write(out, argout.value);
          C_array_e_ObjectHelper.write(out, arginout.value);
          break;
        }

        // D
        case 39 : // rf11/op46
        {
          D_d_short argin = D_d_shortHelper.read(in);
          D_d_shortHolder argout = new D_d_shortHolder();
          D_d_shortHolder arginout = new D_d_shortHolder();
          arginout.value = D_d_shortHelper.read(in);

          D_d_short __result = null;
          __result = this.op46(argin, argout, arginout);
          out = rh.createReply();
          D_d_shortHelper.write(out, __result);
          D_d_shortHelper.write(out, argout.value);
          D_d_shortHelper.write(out, arginout.value);
          break;
        }

        case 40 : // rf11/op47
        {
          D_d_ushort argin = D_d_ushortHelper.read(in);
          D_d_ushortHolder argout = new D_d_ushortHolder();
          D_d_ushortHolder arginout = new D_d_ushortHolder();
          arginout.value = D_d_ushortHelper.read(in);

          D_d_ushort __result = null;
          __result = this.op47(argin, argout, arginout);
          out = rh.createReply();
          D_d_ushortHelper.write(out, __result);
          D_d_ushortHelper.write(out, argout.value);
          D_d_ushortHelper.write(out, arginout.value);
          break;
        }

        case 41 : // rf11/op48
        {
          D_d_long argin = D_d_longHelper.read(in);
          D_d_longHolder argout = new D_d_longHolder();
          D_d_longHolder arginout = new D_d_longHolder();
          arginout.value = D_d_longHelper.read(in);

          D_d_long __result = null;
          __result = this.op48(argin, argout, arginout);
          out = rh.createReply();
          D_d_longHelper.write(out, __result);
          D_d_longHelper.write(out, argout.value);
          D_d_longHelper.write(out, arginout.value);
          break;
        }

        case 42 : // rf11/op49
        {
          D_d_ulong argin = D_d_ulongHelper.read(in);
          D_d_ulongHolder argout = new D_d_ulongHolder();
          D_d_ulongHolder arginout = new D_d_ulongHolder();
          arginout.value = D_d_ulongHelper.read(in);

          D_d_ulong __result = null;
          __result = this.op49(argin, argout, arginout);
          out = rh.createReply();
          D_d_ulongHelper.write(out, __result);
          D_d_ulongHelper.write(out, argout.value);
          D_d_ulongHelper.write(out, arginout.value);
          break;
        }

        case 43 : // rf11/op50
        {
          D_d_char argin = D_d_charHelper.read(in);
          D_d_charHolder argout = new D_d_charHolder();
          D_d_charHolder arginout = new D_d_charHolder();
          arginout.value = D_d_charHelper.read(in);

          D_d_char __result = null;
          __result = this.op50(argin, argout, arginout);
          out = rh.createReply();
          D_d_charHelper.write(out, __result);
          D_d_charHelper.write(out, argout.value);
          D_d_charHelper.write(out, arginout.value);
          break;
        }

        case 44 : // rf11/op51
        {
          D_d_boolean argin = D_d_booleanHelper.read(in);
          D_d_booleanHolder argout = new D_d_booleanHolder();
          D_d_booleanHolder arginout = new D_d_booleanHolder();
          arginout.value = D_d_booleanHelper.read(in);

          D_d_boolean __result = null;
          __result = this.op51(argin, argout, arginout);
          out = rh.createReply();
          D_d_booleanHelper.write(out, __result);
          D_d_booleanHelper.write(out, argout.value);
          D_d_booleanHelper.write(out, arginout.value);
          break;
        }

        case 45 : // rf11/op52
        {
          D_d_B argin = D_d_BHelper.read(in);
          D_d_BHolder argout = new D_d_BHolder();
          D_d_BHolder arginout = new D_d_BHolder();
          arginout.value = D_d_BHelper.read(in);

          D_d_B __result = null;
          __result = this.op52(argin, argout, arginout);
          out = rh.createReply();
          D_d_BHelper.write(out, __result);
          D_d_BHelper.write(out, argout.value);
          D_d_BHelper.write(out, arginout.value);
          break;
        }

        // E
        case 46 : // rf11/op53
        {
          E_struct argin = E_structHelper.read(in);
          E_structHolder argout = new E_structHolder();
          E_structHolder arginout = new E_structHolder();
          arginout.value = E_structHelper.read(in);

          E_struct __result = null;
          __result = this.op53(argin, argout, arginout);
          out = rh.createReply();
          E_structHelper.write(out, __result);
          E_structHelper.write(out, argout.value);
          E_structHelper.write(out, arginout.value);
          break;
        }

        case 47 : // rf11/op54
        {
          E_union argin = E_unionHelper.read(in);
          E_unionHolder argout = new E_unionHolder();
          E_unionHolder arginout = new E_unionHolder();
          arginout.value = E_unionHelper.read(in);

          E_union __result = null;
          __result = this.op54(argin, argout, arginout);
          out = rh.createReply();
          E_unionHelper.write(out, __result);
          E_unionHelper.write(out, argout.value);
          E_unionHelper.write(out, arginout.value);
          break;
        }

        case 48 : // rf11/op55
        {
          B[] argin = E_sequenceHelper.read(in);
          E_sequenceHolder argout = new E_sequenceHolder();
          E_sequenceHolder arginout = new E_sequenceHolder();
          arginout.value = E_sequenceHelper.read(in);

          B[] __result = null;
          __result = this.op55(argin, argout, arginout);
          out = rh.createReply();
          E_sequenceHelper.write(out, __result);
          E_sequenceHelper.write(out, argout.value);
          E_sequenceHelper.write(out, arginout.value);
          break;
        }

        case 49 : // rf11/op56
        {
          B[] argin = E_arrayHelper.read(in);
          E_arrayHolder argout = new E_arrayHolder();
          E_arrayHolder arginout = new E_arrayHolder();
          arginout.value = E_arrayHelper.read(in);

          B[] __result = null;
          __result = this.op56(argin, argout, arginout);
          out = rh.createReply();
          E_arrayHelper.write(out, __result);
          E_arrayHelper.write(out, argout.value);
          E_arrayHelper.write(out, arginout.value);
          break;
        }

        // F
        case 50 : // rf11/op57
        {
          F_struct argin = F_structHelper.read(in);
          F_structHolder argout = new F_structHolder();
          F_structHolder arginout = new F_structHolder();
          arginout.value = F_structHelper.read(in);

          F_struct __result = null;
          __result = this.op57(argin, argout, arginout);
          out = rh.createReply();
          F_structHelper.write(out, __result);
          F_structHelper.write(out, argout.value);
          F_structHelper.write(out, arginout.value);
          break;
        }

        case 51 : // rf11/op58
        {
          F_union argin = F_unionHelper.read(in);
          F_unionHolder argout = new F_unionHolder();
          F_unionHolder arginout = new F_unionHolder();
          arginout.value = F_unionHelper.read(in);

          F_union __result = null;
          __result = this.op58(argin, argout, arginout);
          out = rh.createReply();
          F_unionHelper.write(out, __result);
          F_unionHelper.write(out, argout.value);
          F_unionHelper.write(out, arginout.value);
          break;
        }

        case 52 : // rf11/op59
        {
          C_struct[] argin = F_sequence_e_c_structHelper.read(in);
          F_sequence_e_c_structHolder argout =
            new F_sequence_e_c_structHolder();
          F_sequence_e_c_structHolder arginout =
            new F_sequence_e_c_structHolder();
          arginout.value = F_sequence_e_c_structHelper.read(in);

          C_struct[] __result = null;
          __result = this.op59(argin, argout, arginout);
          out = rh.createReply();
          F_sequence_e_c_structHelper.write(out, __result);
          F_sequence_e_c_structHelper.write(out, argout.value);
          F_sequence_e_c_structHelper.write(out, arginout.value);
          break;
        }

        case 53 : // rf11/op60
        {
          C_union[] argin = F_sequence_e_c_unionHelper.read(in);
          F_sequence_e_c_unionHolder argout = new F_sequence_e_c_unionHolder();
          F_sequence_e_c_unionHolder arginout =
            new F_sequence_e_c_unionHolder();
          arginout.value = F_sequence_e_c_unionHelper.read(in);

          C_union[] __result = null;
          __result = this.op60(argin, argout, arginout);
          out = rh.createReply();
          F_sequence_e_c_unionHelper.write(out, __result);
          F_sequence_e_c_unionHelper.write(out, argout.value);
          F_sequence_e_c_unionHelper.write(out, arginout.value);
          break;
        }

        case 54 : // rf11/op89
        {
          C_struct[] argin = F_array_e_c_structHelper.read(in);
          F_array_e_c_structHolder argout = new F_array_e_c_structHolder();
          F_array_e_c_structHolder arginout = new F_array_e_c_structHolder();
          arginout.value = F_array_e_c_structHelper.read(in);

          C_struct[] __result = null;
          __result = this.op89(argin, argout, arginout);
          out = rh.createReply();
          F_array_e_c_structHelper.write(out, __result);
          F_array_e_c_structHelper.write(out, argout.value);
          F_array_e_c_structHelper.write(out, arginout.value);
          break;
        }

        case 55 : // rf11/op90
        {
          C_union[] argin = F_array_e_c_unionHelper.read(in);
          F_array_e_c_unionHolder argout = new F_array_e_c_unionHolder();
          F_array_e_c_unionHolder arginout = new F_array_e_c_unionHolder();
          arginout.value = F_array_e_c_unionHelper.read(in);

          C_union[] __result = null;
          __result = this.op90(argin, argout, arginout);
          out = rh.createReply();
          F_array_e_c_unionHelper.write(out, __result);
          F_array_e_c_unionHelper.write(out, argout.value);
          F_array_e_c_unionHelper.write(out, arginout.value);
          break;
        }

        // G
        case 56 : // rf11/op119
        {
          G_struct argin = G_structHelper.read(in);
          G_structHolder argout = new G_structHolder();
          G_structHolder arginout = new G_structHolder();
          arginout.value = G_structHelper.read(in);

          G_struct __result = null;
          __result = this.op119(argin, argout, arginout);
          out = rh.createReply();
          G_structHelper.write(out, __result);
          G_structHelper.write(out, argout.value);
          G_structHelper.write(out, arginout.value);
          break;
        }

        case 57 : // rf11/op120
        {
          G_union argin = G_unionHelper.read(in);
          G_unionHolder argout = new G_unionHolder();
          G_unionHolder arginout = new G_unionHolder();
          arginout.value = G_unionHelper.read(in);

          G_union __result = null;
          __result = this.op120(argin, argout, arginout);
          out = rh.createReply();
          G_unionHelper.write(out, __result);
          G_unionHelper.write(out, argout.value);
          G_unionHelper.write(out, arginout.value);
          break;
        }

        case 58 : // rf11/op121
        {
          E_struct[] argin = G_sequence_e_e_structHelper.read(in);
          G_sequence_e_e_structHolder argout =
            new G_sequence_e_e_structHolder();
          G_sequence_e_e_structHolder arginout =
            new G_sequence_e_e_structHolder();
          arginout.value = G_sequence_e_e_structHelper.read(in);

          E_struct[] __result = null;
          __result = this.op121(argin, argout, arginout);
          out = rh.createReply();
          G_sequence_e_e_structHelper.write(out, __result);
          G_sequence_e_e_structHelper.write(out, argout.value);
          G_sequence_e_e_structHelper.write(out, arginout.value);
          break;
        }

        case 59 : // rf11/op122
        {
          E_union[] argin = G_sequence_e_e_unionHelper.read(in);
          G_sequence_e_e_unionHolder argout = new G_sequence_e_e_unionHolder();
          G_sequence_e_e_unionHolder arginout =
            new G_sequence_e_e_unionHolder();
          arginout.value = G_sequence_e_e_unionHelper.read(in);

          E_union[] __result = null;
          __result = this.op122(argin, argout, arginout);
          out = rh.createReply();
          G_sequence_e_e_unionHelper.write(out, __result);
          G_sequence_e_e_unionHelper.write(out, argout.value);
          G_sequence_e_e_unionHelper.write(out, arginout.value);
          break;
        }

        case 60 : // rf11/op125
        {
          E_struct[] argin = G_array_e_e_structHelper.read(in);
          G_array_e_e_structHolder argout = new G_array_e_e_structHolder();
          G_array_e_e_structHolder arginout = new G_array_e_e_structHolder();
          arginout.value = G_array_e_e_structHelper.read(in);

          E_struct[] __result = null;
          __result = this.op125(argin, argout, arginout);
          out = rh.createReply();
          G_array_e_e_structHelper.write(out, __result);
          G_array_e_e_structHelper.write(out, argout.value);
          G_array_e_e_structHelper.write(out, arginout.value);
          break;
        }

        case 61 : // rf11/op126
        {
          E_union[] argin = G_array_e_e_unionHelper.read(in);
          G_array_e_e_unionHolder argout = new G_array_e_e_unionHolder();
          G_array_e_e_unionHolder arginout = new G_array_e_e_unionHolder();
          arginout.value = G_array_e_e_unionHelper.read(in);

          E_union[] __result = null;
          __result = this.op126(argin, argout, arginout);
          out = rh.createReply();
          G_array_e_e_unionHelper.write(out, __result);
          G_array_e_e_unionHelper.write(out, argout.value);
          G_array_e_e_unionHelper.write(out, arginout.value);
          break;
        }

        // rest of F
        case 62 : // rf11/op129
        {
          F_union argin = F_unionHelper.read(in);
          F_unionHolder argout = new F_unionHolder();
          F_unionHolder arginout = new F_unionHolder();
          arginout.value = F_unionHelper.read(in);

          F_union __result = null;
          __result = this.op129(argin, argout, arginout);
          out = rh.createReply();
          F_unionHelper.write(out, __result);
          F_unionHelper.write(out, argout.value);
          F_unionHelper.write(out, arginout.value);
          break;
        }

        case 63 : // rf11/op130
        {
          F_union argin = F_unionHelper.read(in);
          F_unionHolder argout = new F_unionHolder();
          F_unionHolder arginout = new F_unionHolder();
          arginout.value = F_unionHelper.read(in);

          F_union __result = null;
          __result = this.op130(argin, argout, arginout);
          out = rh.createReply();
          F_unionHelper.write(out, __result);
          F_unionHelper.write(out, argout.value);
          F_unionHelper.write(out, arginout.value);
          break;
        }

        case 64 : // rf11/op131
        {
          F_union argin = F_unionHelper.read(in);
          F_unionHolder argout = new F_unionHolder();
          F_unionHolder arginout = new F_unionHolder();
          arginout.value = F_unionHelper.read(in);

          F_union __result = null;
          __result = this.op131(argin, argout, arginout);
          out = rh.createReply();
          F_unionHelper.write(out, __result);
          F_unionHelper.write(out, argout.value);
          F_unionHelper.write(out, arginout.value);
          break;
        }

        // pragma: exception=A_except1
        case 65 : // rf11/excop1
        {
          try
            {
              this.excop1();
              out = rh.createReply();
            }
          catch (A_except1 __ex)
            {
              out = rh.createExceptionReply();
              A_except1Helper.write(out, __ex);
            }
          break;
        }

        // pragma: exception=A_except2
        case 66 : // rf11/excop2
        {
          try
            {
              this.excop2();
              out = rh.createReply();
            }
          catch (A_except2 __ex)
            {
              out = rh.createExceptionReply();
              A_except2Helper.write(out, __ex);
            }
          break;
        }

        // pragma: exception=B_except
        case 67 : // rf11/excop3
        {
          try
            {
              this.excop3();
              out = rh.createReply();
            }
          catch (B_except __ex)
            {
              out = rh.createExceptionReply();
              B_exceptHelper.write(out, __ex);
            }
          break;
        }

        // pragma: exception=C_except
        case 68 : // rf11/excop4
        {
          try
            {
              this.excop4();
              out = rh.createReply();
            }
          catch (C_except __ex)
            {
              out = rh.createExceptionReply();
              C_exceptHelper.write(out, __ex);
            }
          break;
        }

        // pragma: exception=D_except
        case 69 : // rf11/excop5
        {
          try
            {
              this.excop5();
              out = rh.createReply();
            }
          catch (D_except __ex)
            {
              out = rh.createExceptionReply();
              D_exceptHelper.write(out, __ex);
            }
          break;
        }

        // pragma: exception=E_except
        case 70 : // rf11/excop6
        {
          try
            {
              this.excop6();
              out = rh.createReply();
            }
          catch (E_except __ex)
            {
              out = rh.createExceptionReply();
              E_exceptHelper.write(out, __ex);
            }
          break;
        }

        // pragma: exception=F_except1
        case 71 : // rf11/excop7
        {
          try
            {
              this.excop7();
              out = rh.createReply();
            }
          catch (F_except1 __ex)
            {
              out = rh.createExceptionReply();
              F_except1Helper.write(out, __ex);
            }
          break;
        }

        // pragma: exception=F_except2
        case 72 : // rf11/excop8
        {
          try
            {
              this.excop8();
              out = rh.createReply();
            }
          catch (F_except2 __ex)
            {
              out = rh.createExceptionReply();
              F_except2Helper.write(out, __ex);
            }
          break;
        }

        // pragma: exception=F_except3
        case 73 : // rf11/excop9
        {
          try
            {
              this.excop9();
              out = rh.createReply();
            }
          catch (F_except3 __ex)
            {
              out = rh.createExceptionReply();
              F_except3Helper.write(out, __ex);
            }
          break;
        }

        // pragma: exception=G_except
        case 74 : // rf11/excop10
        {
          try
            {
              this.excop10();
              out = rh.createReply();
            }
          catch (G_except __ex)
            {
              out = rh.createExceptionReply();
              G_exceptHelper.write(out, __ex);
            }
          break;
        }

        default :
          throw new org.omg.CORBA.BAD_OPERATION(0,
                                                org.omg.CORBA.CompletionStatus.COMPLETED_MAYBE
                                               );
      }

    return out;
  } // _invoke

  // Type-specific CORBA::Object operations
  private static String[] __ids = { "IDL:gnu/testlet/org/omg/CORBA/ORB/RF11/rf11:1.0" };

  public String[] _ids()
  {
    return __ids;
  }
} // class _rf11ImplBase
