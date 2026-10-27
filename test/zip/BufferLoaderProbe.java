// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.io.File;
import java.net.*;
import java.nio.ByteBuffer;
import java.util.jar.JarFile;

public class BufferLoaderProbe {
    static class Loader extends URLClassLoader {
        Loader(URL url) { super(new URL[]{url}, null); }
        Class<?> define(ByteBuffer buffer) { return defineClass("ZipPayload", buffer, (java.security.ProtectionDomain) null); }
    }
    public static void main(String[] args) throws Exception {
        File file = new File(args[0]);
        byte[] bytes;
        try (JarFile jar = new JarFile(file)) {
            bytes = jar.getInputStream(jar.getJarEntry("ZipPayload.class")).readAllBytes();
        }
        for (int mode = 0; mode < 4; mode++) {
            try (Loader loader = new Loader(file.toURI().toURL())) {
                ByteBuffer buffer = mode == 3 ? ByteBuffer.allocate(bytes.length + 12)
                                              : ByteBuffer.allocateDirect(bytes.length + 12);
                buffer.position(5);
                buffer.put(bytes);
                buffer.limit(5 + bytes.length);
                buffer.position(5);
                if (mode == 1) buffer = buffer.slice();
                if (mode >= 2) buffer = buffer.asReadOnlyBuffer();
                int position = buffer.position(), limit = buffer.limit();
                Class<?> type = loader.define(buffer);
                // The JDK's non-direct read-only path copies with a relative
                // get; native direct-buffer definitions preserve the position.
                if ((mode == 3 ? limit : position) != buffer.position() || limit != buffer.limit())
                    throw new AssertionError("Class definition changed the buffer state");
                if (type.getClassLoader() != loader || type.getModule() != loader.getUnnamedModule()
                    || !Integer.valueOf(42).equals(type.getMethod("answer").invoke(null)))
                    throw new AssertionError("ByteBuffer class definition identity or result");
            }
        }
        System.out.println("Direct, sliced, and read-only ByteBuffer class definitions passed");
    }
}
