// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.ReadOnlyBufferException;

public class BufferScalarProbe {
    public static void main(String[] args) {
        for (ByteOrder order : new ByteOrder[] {ByteOrder.BIG_ENDIAN, ByteOrder.LITTLE_ENDIAN}) {
            ByteBuffer buffer = ByteBuffer.allocate(64).order(order);
            buffer.position(1);
            buffer.putInt(0x12345678).putLong(0x123456789abcdef0L).putShort((short) 0x9876)
                  .putChar('\u20ac').putFloat(-12.5f).putDouble(123.25);
            int[] expected = order == ByteOrder.BIG_ENDIAN
                    ? new int[] {0x12, 0x34, 0x56, 0x78} : new int[] {0x78, 0x56, 0x34, 0x12};
            for (int i = 0; i < 4; i++)
                if ((buffer.array()[i + 1] & 255) != expected[i]) throw new AssertionError("byte order");
            buffer.position(1);
            if (buffer.getInt() != 0x12345678 || buffer.getLong() != 0x123456789abcdef0L
                    || buffer.getShort() != (short) 0x9876 || buffer.getChar() != '\u20ac'
                    || buffer.getFloat() != -12.5f || buffer.getDouble() != 123.25)
                throw new AssertionError("scalar values");
            buffer.position(1);
            ByteBuffer slice = buffer.slice().order(order);
            slice.putInt(0, 0x10203040);
            if (buffer.getInt(1) != 0x10203040) throw new AssertionError("shared slice");
            try {
                buffer.asReadOnlyBuffer().putInt(1, 0);
                throw new AssertionError("read-only write accepted");
            } catch (ReadOnlyBufferException expectedFailure) { }
        }
        System.out.println("ByteBuffer unaligned scalar access, byte order, slices, and read-only checks passed");
    }
}
