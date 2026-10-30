// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.io.*;
import java.nio.*;

public class DataCharacterProbe {
    static final int[] UNITS = {0, 1, 97, 255, 256, 32767, 32768, 0xe2d2, 0xd800, 0xdc00, 65535};
    static void check(int actual, int expected, String message) {
        if (actual != expected) throw new AssertionError(message + ": " + actual + " expected " + expected);
    }
    public static void main(String[] args) throws Exception {
        ByteArrayOutputStream bytes = new ByteArrayOutputStream();
        DataOutputStream output = new DataOutputStream(bytes);
        for (int unit : UNITS) output.writeChar(unit);
        DataInputStream input = new DataInputStream(new ByteArrayInputStream(bytes.toByteArray()));
        for (int unit : UNITS) {
            char c = input.readChar();
            check(c, unit, "readChar numeric promotion");
            check(c + 7, unit + 7, "character arithmetic");
            check(Character.valueOf(c).charValue(), unit, "character boxing");
        }
        input = new DataInputStream(new ByteArrayInputStream(bytes.toByteArray()));
        for (int unit : UNITS) check(input.readUnsignedShort(), unit, "unsigned short agreement");
        for (ByteOrder order : new ByteOrder[] {ByteOrder.BIG_ENDIAN, ByteOrder.LITTLE_ENDIAN}) {
            ByteBuffer heap = ByteBuffer.allocate(2 * UNITS.length + 1).order(order);
            ByteBuffer direct = ByteBuffer.allocateDirect(heap.capacity()).order(order);
            heap.position(1); direct.position(1);
            for (int unit : UNITS) { heap.putChar((char)unit); direct.putChar((char)unit); }
            heap.position(1); direct.position(1);
            for (int unit : UNITS) {
                check(heap.getChar(), unit, "heap char view");
                check(direct.getChar(), unit, "direct char view");
            }
        }
        bytes.reset();
        try (ObjectOutputStream objects = new ObjectOutputStream(bytes)) {
            for (int unit : UNITS) objects.writeChar(unit);
        }
        try (ObjectInputStream objects = new ObjectInputStream(new ByteArrayInputStream(bytes.toByteArray()))) {
            for (int unit : UNITS) check(objects.readChar(), unit, "object stream char");
        }
        String text = new String(new char[] {'a', 0, '\ue2d2', '\ud800', '\udc00', '\uffff'});
        bytes.reset(); output = new DataOutputStream(bytes); output.writeUTF(text);
        input = new DataInputStream(new ByteArrayInputStream(bytes.toByteArray()));
        String restored = input.readUTF();
        check(restored.length(), text.length(), "modified UTF length");
        for (int i = 0; i < text.length(); i++) check(restored.charAt(i), text.charAt(i), "modified UTF unit");
        System.out.println("Data and object stream UTF-16 units, numeric promotion, endian buffers, and modified UTF passed");
    }
}
