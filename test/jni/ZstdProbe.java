// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import com.github.luben.zstd.Zstd;
import java.nio.ByteBuffer;
import java.util.Arrays;

// Optional integration with an unmodified third-party JNI library.
public class ZstdProbe {
    public static void main(String[] args) {
        byte[] input = new byte[16384];
        for (int i = 0; i < input.length; i++) input[i] = (byte)(i % 17);
        byte[] compressed = Zstd.compress(input);
        if (!Arrays.equals(input, Zstd.decompress(compressed, input.length)))
            throw new AssertionError("zstd byte-array round trip");
        ByteBuffer source = ByteBuffer.allocateDirect(input.length);
        source.put(input).flip();
        ByteBuffer destination = ByteBuffer.allocateDirect((int)Zstd.compressBound(input.length));
        long count = Zstd.compressDirectByteBuffer(destination, 0, destination.capacity(), source, 0, input.length, 3);
        if (Zstd.isError(count)) throw new AssertionError(Zstd.getErrorName(count));
        ByteBuffer output = ByteBuffer.allocateDirect(input.length);
        long size = Zstd.decompressDirectByteBuffer(output, 0, output.capacity(), destination, 0, (int)count);
        if (size != input.length) throw new AssertionError("zstd direct buffer size");
        for (int i = 0; i < input.length; i++)
            if (output.get(i) != input[i]) throw new AssertionError("zstd direct buffer data");
        System.out.println("Third-party zstd JNI passed");
    }
}
