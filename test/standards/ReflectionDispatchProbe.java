// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.lang.reflect.Method;
import java.io.*;
import dispatch.a.Base;
import dispatch.b.Foreign;
import dispatch.b.AfterBridge;
public class ReflectionDispatchProbe {
    static int checks;
    interface Secret { private String token() { return "interface-private"; } }
    static class PublicToken implements Secret {
        public String token() { return "class-public"; }
    }
    static void check(Method m, Object receiver, String expected) throws Exception {
        m.setAccessible(true);
        for (int i = 0; i < 3; i++) {
            checks++;
            Object got = m.invoke(receiver);
            if (!expected.equals(got)) throw new AssertionError(m + " expected " + expected + " got " + got);
        }
    }
    static class FixtureLoader extends ClassLoader {
        Class<?> fixture(String name, boolean soften) throws Exception {
            byte[] bytes;
            try (InputStream in = Base.class.getResourceAsStream("/" + name.replace('.', '/') + ".class")) {
                bytes = in.readAllBytes();
            }
            if (soften) {
                // The JVM permits weaker access in class files even though javac
                // rejects it. Exercise the transitive override rule in JVMS 5.4.5.
                ByteArrayInputStream raw = new ByteArrayInputStream(bytes);
                DataInputStream in = new DataInputStream(raw);
                in.skipNBytes(8);
                String[] utf = new String[in.readUnsignedShort()];
                for (int i = 1; i < utf.length; i++) {
                    switch (in.readUnsignedByte()) {
                        case 1 -> utf[i] = in.readUTF();
                        case 3, 4, 9, 10, 11, 12, 17, 18 -> in.skipNBytes(4);
                        case 5, 6 -> { in.skipNBytes(8); i++; }
                        case 7, 8, 16, 19, 20 -> in.skipNBytes(2);
                        case 15 -> in.skipNBytes(3);
                        default -> throw new AssertionError("constant pool tag");
                    }
                }
                in.skipNBytes(6);
                in.skipNBytes(2L * in.readUnsignedShort());
                int fields = in.readUnsignedShort();
                for (int i = 0; i < fields; i++) { in.skipNBytes(6); attributes(in); }
                int methods = in.readUnsignedShort();
                boolean changed = false;
                for (int i = 0; i < methods; i++) {
                    int offset = bytes.length - raw.available();
                    int flags = in.readUnsignedShort();
                    String method = utf[in.readUnsignedShort()];
                    in.readUnsignedShort();
                    if (method.equals("m")) {
                        if ((flags & 7) != 1) throw new AssertionError("fixture access");
                        bytes[offset + 1] &= ~1;
                        changed = true;
                    }
                    attributes(in);
                }
                if (!changed) throw new AssertionError("fixture not patched");
            }
            return defineClass(name, bytes, 0, bytes.length);
        }
    }
    static void attributes(DataInputStream in) throws IOException {
        int count = in.readUnsignedShort();
        for (int i = 0; i < count; i++) {
            in.readUnsignedShort(); in.skipNBytes(Integer.toUnsignedLong(in.readInt()));
        }
    }
    public static void main(String[] args) throws Exception {
        Method m = Base.class.getDeclaredMethod("m");
        check(m, new Base(), "base");
        check(m, new Foreign(), "base");
        check(m, new Base.Same(), "same");
        check(m, new Base.Bridge(), "bridge");
        check(m, new AfterBridge(), "after");
        check(m, new Base.Back(), "back");
        check(Base.class.getDeclaredMethod("hidden"), new Foreign(), "private-base");
        check(Foreign.class.getDeclaredMethod("hidden"), new Foreign(), "public-foreign");
        check(Foreign.class.getDeclaredMethod("m"), new Base.Back(), "back");
        check(Secret.class.getDeclaredMethod("token"), new PublicToken(), "interface-private");
        FixtureLoader loader = new FixtureLoader();
        Class<?> isolated = loader.fixture("dispatch.a.Isolated", false);
        check(m, isolated.getConstructor().newInstance(), "base");
        Class<?> soft = loader.fixture("dispatch.a.Soft", true);
        Class<?> after = loader.fixture("dispatch.b.AfterSoft", false);
        Object receiver = after.getConstructor().newInstance();
        check(m, receiver, "after-soft");
        check(soft.getDeclaredMethod("m"), receiver, "soft");
        System.out.println("Reflection dispatch passed: " + checks);
    }
}
