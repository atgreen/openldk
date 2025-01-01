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

public class _rf11Stub
  extends org.omg.CORBA.portable.ObjectImpl
  implements NEC_RF11
{
  // Constructors
  // NOTE:  If the default constructor is used, the
  //        object is useless until _set_delegate (...)
  //        is called.
  public _rf11Stub()
  {
    super();
  }

  public _rf11Stub(org.omg.CORBA.portable.Delegate delegate)
  {
    super();
    _set_delegate(delegate);
  }

  // A
  public short op1(short argin, org.omg.CORBA.ShortHolder argout,
                   org.omg.CORBA.ShortHolder arginout
                  )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op1", true);
        _out.write_short(argin);
        _out.write_short(arginout.value);
        _in = _invoke(_out);

        short __result = _in.read_short();
        argout.value = _in.read_short();
        arginout.value = _in.read_short();
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op1(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op1

  public short op2(short argin, org.omg.CORBA.ShortHolder argout,
                   org.omg.CORBA.ShortHolder arginout
                  )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op2", true);
        _out.write_ushort(argin);
        _out.write_ushort(arginout.value);
        _in = _invoke(_out);

        short __result = _in.read_ushort();
        argout.value = _in.read_ushort();
        arginout.value = _in.read_ushort();
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op2(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op2

  public int op3(int argin, org.omg.CORBA.IntHolder argout,
                 org.omg.CORBA.IntHolder arginout
                )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op3", true);
        _out.write_long(argin);
        _out.write_long(arginout.value);
        _in = _invoke(_out);

        int __result = _in.read_long();
        argout.value = _in.read_long();
        arginout.value = _in.read_long();
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op3(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op3

  public int op4(int argin, org.omg.CORBA.IntHolder argout,
                 org.omg.CORBA.IntHolder arginout
                )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op4", true);
        _out.write_ulong(argin);
        _out.write_ulong(arginout.value);
        _in = _invoke(_out);

        int __result = _in.read_ulong();
        argout.value = _in.read_ulong();
        arginout.value = _in.read_ulong();
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op4(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op4

  public float op5(float argin, org.omg.CORBA.FloatHolder argout,
                   org.omg.CORBA.FloatHolder arginout
                  )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op5", true);
        _out.write_float(argin);
        _out.write_float(arginout.value);
        _in = _invoke(_out);

        float __result = _in.read_float();
        argout.value = _in.read_float();
        arginout.value = _in.read_float();
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op5(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op5

  public double op6(double argin, org.omg.CORBA.DoubleHolder argout,
                    org.omg.CORBA.DoubleHolder arginout
                   )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op6", true);
        _out.write_double(argin);
        _out.write_double(arginout.value);
        _in = _invoke(_out);

        double __result = _in.read_double();
        argout.value = _in.read_double();
        arginout.value = _in.read_double();
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op6(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op6

  public char op7(char argin, org.omg.CORBA.CharHolder argout,
                  org.omg.CORBA.CharHolder arginout
                 )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op7", true);
        _out.write_char(argin);
        _out.write_char(arginout.value);
        _in = _invoke(_out);

        char __result = _in.read_char();
        argout.value = _in.read_char();
        arginout.value = _in.read_char();
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op7(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op7

  public boolean op8(boolean argin, org.omg.CORBA.BooleanHolder argout,
                     org.omg.CORBA.BooleanHolder arginout
                    )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op8", true);
        _out.write_boolean(argin);
        _out.write_boolean(arginout.value);
        _in = _invoke(_out);

        boolean __result = _in.read_boolean();
        argout.value = _in.read_boolean();
        arginout.value = _in.read_boolean();
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op8(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op8

  public byte op9(byte argin, org.omg.CORBA.ByteHolder argout,
                  org.omg.CORBA.ByteHolder arginout
                 )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op9", true);
        _out.write_octet(argin);
        _out.write_octet(arginout.value);
        _in = _invoke(_out);

        byte __result = _in.read_octet();
        argout.value = _in.read_octet();
        arginout.value = _in.read_octet();
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op9(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op9

  public org.omg.CORBA.Any op10(org.omg.CORBA.Any argin,
                                org.omg.CORBA.AnyHolder argout,
                                org.omg.CORBA.AnyHolder arginout
                               )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op10", true);
        _out.write_any(argin);
        _out.write_any(arginout.value);
        _in = _invoke(_out);

        org.omg.CORBA.Any __result = _in.read_any();
        argout.value = _in.read_any();
        arginout.value = _in.read_any();
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op10(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op10

  public String op11(String argin, org.omg.CORBA.StringHolder argout,
                     org.omg.CORBA.StringHolder arginout
                    )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op11", true);
        _out.write_string(argin);
        _out.write_string(arginout.value);
        _in = _invoke(_out);

        String __result = _in.read_string();
        argout.value = _in.read_string();
        arginout.value = _in.read_string();
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op11(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op11

  public org.omg.CORBA.Object op12(org.omg.CORBA.Object argin,
                                   org.omg.CORBA.ObjectHolder argout,
                                   org.omg.CORBA.ObjectHolder arginout
                                  )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op12", true);
        org.omg.CORBA.ObjectHelper.write(_out, argin);
        org.omg.CORBA.ObjectHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        org.omg.CORBA.Object __result = org.omg.CORBA.ObjectHelper.read(_in);
        argout.value = org.omg.CORBA.ObjectHelper.read(_in);
        arginout.value = org.omg.CORBA.ObjectHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op12(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op12

  // B
  public B op15(B argin, BHolder argout, BHolder arginout)
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op15", true);
        BHelper.write(_out, argin);
        BHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        B __result = BHelper.read(_in);
        argout.value = BHelper.read(_in);
        arginout.value = BHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op15(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op15

  // C
  public C_struct op16(C_struct argin, C_structHolder argout,
                       C_structHolder arginout
                      )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op16", true);
        C_structHelper.write(_out, argin);
        C_structHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        C_struct __result = C_structHelper.read(_in);
        argout.value = C_structHelper.read(_in);
        arginout.value = C_structHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op16(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op16

  public C_union op17(C_union argin, C_unionHolder argout,
                      C_unionHolder arginout
                     )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op17", true);
        C_unionHelper.write(_out, argin);
        C_unionHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        C_union __result = C_unionHelper.read(_in);
        argout.value = C_unionHelper.read(_in);
        arginout.value = C_unionHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op17(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op17

  public short[] op18(short[] argin, C_sequence_e_shortHolder argout,
                      C_sequence_e_shortHolder arginout
                     )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op18", true);
        C_sequence_e_shortHelper.write(_out, argin);
        C_sequence_e_shortHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        short[] __result = C_sequence_e_shortHelper.read(_in);
        argout.value = C_sequence_e_shortHelper.read(_in);
        arginout.value = C_sequence_e_shortHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op18(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op18

  public short[] op19(short[] argin, C_sequence_e_ushortHolder argout,
                      C_sequence_e_ushortHolder arginout
                     )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op19", true);
        C_sequence_e_ushortHelper.write(_out, argin);
        C_sequence_e_ushortHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        short[] __result = C_sequence_e_ushortHelper.read(_in);
        argout.value = C_sequence_e_ushortHelper.read(_in);
        arginout.value = C_sequence_e_ushortHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op19(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op19

  public int[] op20(int[] argin, C_sequence_e_longHolder argout,
                    C_sequence_e_longHolder arginout
                   )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op20", true);
        C_sequence_e_longHelper.write(_out, argin);
        C_sequence_e_longHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        int[] __result = C_sequence_e_longHelper.read(_in);
        argout.value = C_sequence_e_longHelper.read(_in);
        arginout.value = C_sequence_e_longHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op20(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op20

  public int[] op21(int[] argin, C_sequence_e_ulongHolder argout,
                    C_sequence_e_ulongHolder arginout
                   )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op21", true);
        C_sequence_e_ulongHelper.write(_out, argin);
        C_sequence_e_ulongHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        int[] __result = C_sequence_e_ulongHelper.read(_in);
        argout.value = C_sequence_e_ulongHelper.read(_in);
        arginout.value = C_sequence_e_ulongHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op21(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op21

  public float[] op22(float[] argin, C_sequence_e_floatHolder argout,
                      C_sequence_e_floatHolder arginout
                     )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op22", true);
        C_sequence_e_floatHelper.write(_out, argin);
        C_sequence_e_floatHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        float[] __result = C_sequence_e_floatHelper.read(_in);
        argout.value = C_sequence_e_floatHelper.read(_in);
        arginout.value = C_sequence_e_floatHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op22(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op22

  public double[] op23(double[] argin, C_sequence_e_doubleHolder argout,
                       C_sequence_e_doubleHolder arginout
                      )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op23", true);
        C_sequence_e_doubleHelper.write(_out, argin);
        C_sequence_e_doubleHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        double[] __result = C_sequence_e_doubleHelper.read(_in);
        argout.value = C_sequence_e_doubleHelper.read(_in);
        arginout.value = C_sequence_e_doubleHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op23(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op23

  public char[] op24(char[] argin, C_sequence_e_charHolder argout,
                     C_sequence_e_charHolder arginout
                    )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op24", true);
        C_sequence_e_charHelper.write(_out, argin);
        C_sequence_e_charHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        char[] __result = C_sequence_e_charHelper.read(_in);
        argout.value = C_sequence_e_charHelper.read(_in);
        arginout.value = C_sequence_e_charHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op24(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op24

  public boolean[] op25(boolean[] argin, C_sequence_e_booleanHolder argout,
                        C_sequence_e_booleanHolder arginout
                       )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op25", true);
        C_sequence_e_booleanHelper.write(_out, argin);
        C_sequence_e_booleanHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        boolean[] __result = C_sequence_e_booleanHelper.read(_in);
        argout.value = C_sequence_e_booleanHelper.read(_in);
        arginout.value = C_sequence_e_booleanHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op25(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op25

  public byte[] op26(byte[] argin, C_sequence_e_octetHolder argout,
                     C_sequence_e_octetHolder arginout
                    )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op26", true);
        C_sequence_e_octetHelper.write(_out, argin);
        C_sequence_e_octetHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        byte[] __result = C_sequence_e_octetHelper.read(_in);
        argout.value = C_sequence_e_octetHelper.read(_in);
        arginout.value = C_sequence_e_octetHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op26(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op26

  public org.omg.CORBA.Any[] op27(org.omg.CORBA.Any[] argin,
                                  C_sequence_e_anyHolder argout,
                                  C_sequence_e_anyHolder arginout
                                 )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op27", true);
        C_sequence_e_anyHelper.write(_out, argin);
        C_sequence_e_anyHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        org.omg.CORBA.Any[] __result = C_sequence_e_anyHelper.read(_in);
        argout.value = C_sequence_e_anyHelper.read(_in);
        arginout.value = C_sequence_e_anyHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op27(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op27

  public String[] op28(String[] argin, C_sequence_e_stringHolder argout,
                       C_sequence_e_stringHolder arginout
                      )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op28", true);
        C_sequence_e_stringHelper.write(_out, argin);
        C_sequence_e_stringHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        String[] __result = C_sequence_e_stringHelper.read(_in);
        argout.value = C_sequence_e_stringHelper.read(_in);
        arginout.value = C_sequence_e_stringHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op28(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op28

  public org.omg.CORBA.Object[] op29(org.omg.CORBA.Object[] argin,
                                     C_sequence_e_ObjectHolder argout,
                                     C_sequence_e_ObjectHolder arginout
                                    )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op29", true);
        C_sequence_e_ObjectHelper.write(_out, argin);
        C_sequence_e_ObjectHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        org.omg.CORBA.Object[] __result = C_sequence_e_ObjectHelper.read(_in);
        argout.value = C_sequence_e_ObjectHelper.read(_in);
        arginout.value = C_sequence_e_ObjectHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op29(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op29

  public short[] op32(short[] argin, C_array_e_shortHolder argout,
                      C_array_e_shortHolder arginout
                     )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op32", true);
        C_array_e_shortHelper.write(_out, argin);
        C_array_e_shortHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        short[] __result = C_array_e_shortHelper.read(_in);
        argout.value = C_array_e_shortHelper.read(_in);
        arginout.value = C_array_e_shortHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op32(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op32

  public short[] op33(short[] argin, C_array_e_ushortHolder argout,
                      C_array_e_ushortHolder arginout
                     )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op33", true);
        C_array_e_ushortHelper.write(_out, argin);
        C_array_e_ushortHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        short[] __result = C_array_e_ushortHelper.read(_in);
        argout.value = C_array_e_ushortHelper.read(_in);
        arginout.value = C_array_e_ushortHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op33(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op33

  public int[] op34(int[] argin, C_array_e_longHolder argout,
                    C_array_e_longHolder arginout
                   )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op34", true);
        C_array_e_longHelper.write(_out, argin);
        C_array_e_longHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        int[] __result = C_array_e_longHelper.read(_in);
        argout.value = C_array_e_longHelper.read(_in);
        arginout.value = C_array_e_longHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op34(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op34

  public int[] op35(int[] argin, C_array_e_ulongHolder argout,
                    C_array_e_ulongHolder arginout
                   )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op35", true);
        C_array_e_ulongHelper.write(_out, argin);
        C_array_e_ulongHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        int[] __result = C_array_e_ulongHelper.read(_in);
        argout.value = C_array_e_ulongHelper.read(_in);
        arginout.value = C_array_e_ulongHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op35(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op35

  public float[] op36(float[] argin, C_array_e_floatHolder argout,
                      C_array_e_floatHolder arginout
                     )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op36", true);
        C_array_e_floatHelper.write(_out, argin);
        C_array_e_floatHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        float[] __result = C_array_e_floatHelper.read(_in);
        argout.value = C_array_e_floatHelper.read(_in);
        arginout.value = C_array_e_floatHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op36(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op36

  public double[] op37(double[] argin, C_array_e_doubleHolder argout,
                       C_array_e_doubleHolder arginout
                      )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op37", true);
        C_array_e_doubleHelper.write(_out, argin);
        C_array_e_doubleHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        double[] __result = C_array_e_doubleHelper.read(_in);
        argout.value = C_array_e_doubleHelper.read(_in);
        arginout.value = C_array_e_doubleHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op37(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op37

  public char[] op38(char[] argin, C_array_e_charHolder argout,
                     C_array_e_charHolder arginout
                    )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op38", true);
        C_array_e_charHelper.write(_out, argin);
        C_array_e_charHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        char[] __result = C_array_e_charHelper.read(_in);
        argout.value = C_array_e_charHelper.read(_in);
        arginout.value = C_array_e_charHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op38(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op38

  public boolean[] op39(boolean[] argin, C_array_e_booleanHolder argout,
                        C_array_e_booleanHolder arginout
                       )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op39", true);
        C_array_e_booleanHelper.write(_out, argin);
        C_array_e_booleanHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        boolean[] __result = C_array_e_booleanHelper.read(_in);
        argout.value = C_array_e_booleanHelper.read(_in);
        arginout.value = C_array_e_booleanHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op39(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op39

  public byte[] op40(byte[] argin, C_array_e_octetHolder argout,
                     C_array_e_octetHolder arginout
                    )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op40", true);
        C_array_e_octetHelper.write(_out, argin);
        C_array_e_octetHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        byte[] __result = C_array_e_octetHelper.read(_in);
        argout.value = C_array_e_octetHelper.read(_in);
        arginout.value = C_array_e_octetHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op40(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op40

  public org.omg.CORBA.Any[] op41(org.omg.CORBA.Any[] argin,
                                  C_array_e_anyHolder argout,
                                  C_array_e_anyHolder arginout
                                 )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op41", true);
        C_array_e_anyHelper.write(_out, argin);
        C_array_e_anyHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        org.omg.CORBA.Any[] __result = C_array_e_anyHelper.read(_in);
        argout.value = C_array_e_anyHelper.read(_in);
        arginout.value = C_array_e_anyHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op41(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op41

  public String[] op42(String[] argin, C_array_e_stringHolder argout,
                       C_array_e_stringHolder arginout
                      )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op42", true);
        C_array_e_stringHelper.write(_out, argin);
        C_array_e_stringHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        String[] __result = C_array_e_stringHelper.read(_in);
        argout.value = C_array_e_stringHelper.read(_in);
        arginout.value = C_array_e_stringHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op42(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op42

  public org.omg.CORBA.Object[] op43(org.omg.CORBA.Object[] argin,
                                     C_array_e_ObjectHolder argout,
                                     C_array_e_ObjectHolder arginout
                                    )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op43", true);
        C_array_e_ObjectHelper.write(_out, argin);
        C_array_e_ObjectHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        org.omg.CORBA.Object[] __result = C_array_e_ObjectHelper.read(_in);
        argout.value = C_array_e_ObjectHelper.read(_in);
        arginout.value = C_array_e_ObjectHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op43(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op43

  // D
  public D_d_short op46(D_d_short argin, D_d_shortHolder argout,
                        D_d_shortHolder arginout
                       )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op46", true);
        D_d_shortHelper.write(_out, argin);
        D_d_shortHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        D_d_short __result = D_d_shortHelper.read(_in);
        argout.value = D_d_shortHelper.read(_in);
        arginout.value = D_d_shortHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op46(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op46

  public D_d_ushort op47(D_d_ushort argin, D_d_ushortHolder argout,
                         D_d_ushortHolder arginout
                        )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op47", true);
        D_d_ushortHelper.write(_out, argin);
        D_d_ushortHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        D_d_ushort __result = D_d_ushortHelper.read(_in);
        argout.value = D_d_ushortHelper.read(_in);
        arginout.value = D_d_ushortHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op47(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op47

  public D_d_long op48(D_d_long argin, D_d_longHolder argout,
                       D_d_longHolder arginout
                      )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op48", true);
        D_d_longHelper.write(_out, argin);
        D_d_longHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        D_d_long __result = D_d_longHelper.read(_in);
        argout.value = D_d_longHelper.read(_in);
        arginout.value = D_d_longHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op48(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op48

  public D_d_ulong op49(D_d_ulong argin, D_d_ulongHolder argout,
                        D_d_ulongHolder arginout
                       )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op49", true);
        D_d_ulongHelper.write(_out, argin);
        D_d_ulongHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        D_d_ulong __result = D_d_ulongHelper.read(_in);
        argout.value = D_d_ulongHelper.read(_in);
        arginout.value = D_d_ulongHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op49(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op49

  public D_d_char op50(D_d_char argin, D_d_charHolder argout,
                       D_d_charHolder arginout
                      )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op50", true);
        D_d_charHelper.write(_out, argin);
        D_d_charHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        D_d_char __result = D_d_charHelper.read(_in);
        argout.value = D_d_charHelper.read(_in);
        arginout.value = D_d_charHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op50(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op50

  public D_d_boolean op51(D_d_boolean argin, D_d_booleanHolder argout,
                          D_d_booleanHolder arginout
                         )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op51", true);
        D_d_booleanHelper.write(_out, argin);
        D_d_booleanHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        D_d_boolean __result = D_d_booleanHelper.read(_in);
        argout.value = D_d_booleanHelper.read(_in);
        arginout.value = D_d_booleanHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op51(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op51

  public D_d_B op52(D_d_B argin, D_d_BHolder argout, D_d_BHolder arginout)
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op52", true);
        D_d_BHelper.write(_out, argin);
        D_d_BHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        D_d_B __result = D_d_BHelper.read(_in);
        argout.value = D_d_BHelper.read(_in);
        arginout.value = D_d_BHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op52(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op52

  // E
  public E_struct op53(E_struct argin, E_structHolder argout,
                       E_structHolder arginout
                      )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op53", true);
        E_structHelper.write(_out, argin);
        E_structHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        E_struct __result = E_structHelper.read(_in);
        argout.value = E_structHelper.read(_in);
        arginout.value = E_structHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op53(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op53

  public E_union op54(E_union argin, E_unionHolder argout,
                      E_unionHolder arginout
                     )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op54", true);
        E_unionHelper.write(_out, argin);
        E_unionHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        E_union __result = E_unionHelper.read(_in);
        argout.value = E_unionHelper.read(_in);
        arginout.value = E_unionHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op54(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op54

  public B[] op55(B[] argin, E_sequenceHolder argout, E_sequenceHolder arginout)
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op55", true);
        E_sequenceHelper.write(_out, argin);
        E_sequenceHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        B[] __result = E_sequenceHelper.read(_in);
        argout.value = E_sequenceHelper.read(_in);
        arginout.value = E_sequenceHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op55(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op55

  public B[] op56(B[] argin, E_arrayHolder argout, E_arrayHolder arginout)
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op56", true);
        E_arrayHelper.write(_out, argin);
        E_arrayHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        B[] __result = E_arrayHelper.read(_in);
        argout.value = E_arrayHelper.read(_in);
        arginout.value = E_arrayHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op56(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op56

  // F
  public F_struct op57(F_struct argin, F_structHolder argout,
                       F_structHolder arginout
                      )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op57", true);
        F_structHelper.write(_out, argin);
        F_structHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        F_struct __result = F_structHelper.read(_in);
        argout.value = F_structHelper.read(_in);
        arginout.value = F_structHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op57(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op57

  public F_union op58(F_union argin, F_unionHolder argout,
                      F_unionHolder arginout
                     )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op58", true);
        F_unionHelper.write(_out, argin);
        F_unionHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        F_union __result = F_unionHelper.read(_in);
        argout.value = F_unionHelper.read(_in);
        arginout.value = F_unionHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op58(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op58

  public C_struct[] op59(C_struct[] argin, F_sequence_e_c_structHolder argout,
                         F_sequence_e_c_structHolder arginout
                        )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op59", true);
        F_sequence_e_c_structHelper.write(_out, argin);
        F_sequence_e_c_structHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        C_struct[] __result = F_sequence_e_c_structHelper.read(_in);
        argout.value = F_sequence_e_c_structHelper.read(_in);
        arginout.value = F_sequence_e_c_structHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op59(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op59

  public C_union[] op60(C_union[] argin, F_sequence_e_c_unionHolder argout,
                        F_sequence_e_c_unionHolder arginout
                       )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op60", true);
        F_sequence_e_c_unionHelper.write(_out, argin);
        F_sequence_e_c_unionHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        C_union[] __result = F_sequence_e_c_unionHelper.read(_in);
        argout.value = F_sequence_e_c_unionHelper.read(_in);
        arginout.value = F_sequence_e_c_unionHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op60(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op60

  public C_struct[] op89(C_struct[] argin, F_array_e_c_structHolder argout,
                         F_array_e_c_structHolder arginout
                        )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op89", true);
        F_array_e_c_structHelper.write(_out, argin);
        F_array_e_c_structHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        C_struct[] __result = F_array_e_c_structHelper.read(_in);
        argout.value = F_array_e_c_structHelper.read(_in);
        arginout.value = F_array_e_c_structHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op89(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op89

  public C_union[] op90(C_union[] argin, F_array_e_c_unionHolder argout,
                        F_array_e_c_unionHolder arginout
                       )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op90", true);
        F_array_e_c_unionHelper.write(_out, argin);
        F_array_e_c_unionHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        C_union[] __result = F_array_e_c_unionHelper.read(_in);
        argout.value = F_array_e_c_unionHelper.read(_in);
        arginout.value = F_array_e_c_unionHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op90(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op90

  // G
  public G_struct op119(G_struct argin, G_structHolder argout,
                        G_structHolder arginout
                       )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op119", true);
        G_structHelper.write(_out, argin);
        G_structHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        G_struct __result = G_structHelper.read(_in);
        argout.value = G_structHelper.read(_in);
        arginout.value = G_structHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op119(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op119

  public G_union op120(G_union argin, G_unionHolder argout,
                       G_unionHolder arginout
                      )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op120", true);
        G_unionHelper.write(_out, argin);
        G_unionHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        G_union __result = G_unionHelper.read(_in);
        argout.value = G_unionHelper.read(_in);
        arginout.value = G_unionHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op120(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op120

  public E_struct[] op121(E_struct[] argin, G_sequence_e_e_structHolder argout,
                          G_sequence_e_e_structHolder arginout
                         )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op121", true);
        G_sequence_e_e_structHelper.write(_out, argin);
        G_sequence_e_e_structHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        E_struct[] __result = G_sequence_e_e_structHelper.read(_in);
        argout.value = G_sequence_e_e_structHelper.read(_in);
        arginout.value = G_sequence_e_e_structHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op121(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op121

  public E_union[] op122(E_union[] argin, G_sequence_e_e_unionHolder argout,
                         G_sequence_e_e_unionHolder arginout
                        )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op122", true);
        G_sequence_e_e_unionHelper.write(_out, argin);
        G_sequence_e_e_unionHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        E_union[] __result = G_sequence_e_e_unionHelper.read(_in);
        argout.value = G_sequence_e_e_unionHelper.read(_in);
        arginout.value = G_sequence_e_e_unionHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op122(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op122

  public E_struct[] op125(E_struct[] argin, G_array_e_e_structHolder argout,
                          G_array_e_e_structHolder arginout
                         )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op125", true);
        G_array_e_e_structHelper.write(_out, argin);
        G_array_e_e_structHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        E_struct[] __result = G_array_e_e_structHelper.read(_in);
        argout.value = G_array_e_e_structHelper.read(_in);
        arginout.value = G_array_e_e_structHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op125(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op125

  public E_union[] op126(E_union[] argin, G_array_e_e_unionHolder argout,
                         G_array_e_e_unionHolder arginout
                        )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op126", true);
        G_array_e_e_unionHelper.write(_out, argin);
        G_array_e_e_unionHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        E_union[] __result = G_array_e_e_unionHelper.read(_in);
        argout.value = G_array_e_e_unionHelper.read(_in);
        arginout.value = G_array_e_e_unionHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op126(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op126

  // rest of F
  public F_union op129(F_union argin, F_unionHolder argout,
                       F_unionHolder arginout
                      )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op129", true);
        F_unionHelper.write(_out, argin);
        F_unionHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        F_union __result = F_unionHelper.read(_in);
        argout.value = F_unionHelper.read(_in);
        arginout.value = F_unionHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op129(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op129

  public F_union op130(F_union argin, F_unionHolder argout,
                       F_unionHolder arginout
                      )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op130", true);
        F_unionHelper.write(_out, argin);
        F_unionHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        F_union __result = F_unionHelper.read(_in);
        argout.value = F_unionHelper.read(_in);
        arginout.value = F_unionHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op130(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op130

  public F_union op131(F_union argin, F_unionHolder argout,
                       F_unionHolder arginout
                      )
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("op131", true);
        F_unionHelper.write(_out, argin);
        F_unionHelper.write(_out, arginout.value);
        _in = _invoke(_out);

        F_union __result = F_unionHelper.read(_in);
        argout.value = F_unionHelper.read(_in);
        arginout.value = F_unionHelper.read(_in);
        return __result;
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        return op131(argin, argout, arginout);
      }
    finally
      {
        _releaseReply(_in);
      }
  } // op131

  // pragma: exception=A_except1
  public void excop1()
              throws A_except1
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("excop1", true);
        _in = _invoke(_out);
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        if (_id.equals("IDL:gnu/testlet/org/omg/CORBA/ORB/RF11/A_except1:1.0"))
          throw A_except1Helper.read(_in);
        else
          throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        excop1();
      }
    finally
      {
        _releaseReply(_in);
      }
  } // excop1

  // pragma: exception=A_except2
  public void excop2()
              throws A_except2
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("excop2", true);
        _in = _invoke(_out);
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        if (_id.equals("IDL:gnu/testlet/org/omg/CORBA/ORB/RF11/A_except2:1.0"))
          throw A_except2Helper.read(_in);
        else
          throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        excop2();
      }
    finally
      {
        _releaseReply(_in);
      }
  } // excop2

  // pragma: exception=B_except
  public void excop3()
              throws B_except
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("excop3", true);
        _in = _invoke(_out);
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        if (_id.equals("IDL:gnu/testlet/org/omg/CORBA/ORB/RF11/B_except:1.0"))
          throw B_exceptHelper.read(_in);
        else
          throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        excop3();
      }
    finally
      {
        _releaseReply(_in);
      }
  } // excop3

  // pragma: exception=C_except
  public void excop4()
              throws C_except
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("excop4", true);
        _in = _invoke(_out);
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        if (_id.equals("IDL:gnu/testlet/org/omg/CORBA/ORB/RF11/C_except:1.0"))
          throw C_exceptHelper.read(_in);
        else
          throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        excop4();
      }
    finally
      {
        _releaseReply(_in);
      }
  } // excop4

  // pragma: exception=D_except
  public void excop5()
              throws D_except
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("excop5", true);
        _in = _invoke(_out);
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        if (_id.equals("IDL:gnu/testlet/org/omg/CORBA/ORB/RF11/D_except:1.0"))
          throw D_exceptHelper.read(_in);
        else
          throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        excop5();
      }
    finally
      {
        _releaseReply(_in);
      }
  } // excop5

  // pragma: exception=E_except
  public void excop6()
              throws E_except
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("excop6", true);
        _in = _invoke(_out);
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        if (_id.equals("IDL:gnu/testlet/org/omg/CORBA/ORB/RF11/E_except:1.0"))
          throw E_exceptHelper.read(_in);
        else
          throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        excop6();
      }
    finally
      {
        _releaseReply(_in);
      }
  } // excop6

  // pragma: exception=F_except1
  public void excop7()
              throws F_except1
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("excop7", true);
        _in = _invoke(_out);
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        if (_id.equals("IDL:gnu/testlet/org/omg/CORBA/ORB/RF11/F_except1:1.0"))
          throw F_except1Helper.read(_in);
        else
          throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        excop7();
      }
    finally
      {
        _releaseReply(_in);
      }
  } // excop7

  // pragma: exception=F_except2
  public void excop8()
              throws F_except2
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("excop8", true);
        _in = _invoke(_out);
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        if (_id.equals("IDL:gnu/testlet/org/omg/CORBA/ORB/RF11/F_except2:1.0"))
          throw F_except2Helper.read(_in);
        else
          throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        excop8();
      }
    finally
      {
        _releaseReply(_in);
      }
  } // excop8

  // pragma: exception=F_except3
  public void excop9()
              throws F_except3
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("excop9", true);
        _in = _invoke(_out);
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        if (_id.equals("IDL:gnu/testlet/org/omg/CORBA/ORB/RF11/F_except3:1.0"))
          throw F_except3Helper.read(_in);
        else
          throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        excop9();
      }
    finally
      {
        _releaseReply(_in);
      }
  } // excop9

  // pragma: exception=G_except
  public void excop10()
               throws G_except
  {
    org.omg.CORBA.portable.InputStream _in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream _out = _request("excop10", true);
        _in = _invoke(_out);
      }
    catch (org.omg.CORBA.portable.ApplicationException _ex)
      {
        _in = _ex.getInputStream();

        String _id = _ex.getId();
        if (_id.equals("IDL:gnu/testlet/org/omg/CORBA/ORB/RF11/G_except:1.0"))
          throw G_exceptHelper.read(_in);
        else
          throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException _rm)
      {
        excop10();
      }
    finally
      {
        _releaseReply(_in);
      }
  } // excop10

  // Type-specific CORBA::Object operations
  private static String[] __ids = { "IDL:gnu/testlet/org/omg/CORBA/ORB/RF11/rf11:1.0" };

  public String[] _ids()
  {
    return (String[]) __ids.clone();
  }

  private void readObject(java.io.ObjectInputStream s)
  {
    try
      {
        String str = s.readUTF();
        org.omg.CORBA.Object obj =
          org.omg.CORBA.ORB.init().string_to_object(str);
        org.omg.CORBA.portable.Delegate delegate =
          ((org.omg.CORBA.portable.ObjectImpl) obj)._get_delegate();
        _set_delegate(delegate);
      }
    catch (java.io.IOException e)
      {
      }
  }

  private void writeObject(java.io.ObjectOutputStream s)
  {
    try
      {
        String str = org.omg.CORBA.ORB.init().object_to_string(this);
        s.writeUTF(str);
      }
    catch (java.io.IOException e)
      {
      }
  }
} // class _rf11Stub
