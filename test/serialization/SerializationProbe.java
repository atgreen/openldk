// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.io.*;
import java.net.*;
import java.nio.file.*;
import java.lang.reflect.*;
import java.util.*;

public class SerializationProbe {
    public static final String TEXT = "zardoz";
    public static final String UNICODE = "a\u0000\u20ac\ud83d\ude00";
    static class Plain implements Serializable { int value = 42; }
    static class Initialized implements Serializable { static int number = 7; int value = 9; }
    static class Child extends Initialized { int extra = 11; }
    static class Base {
        static int calls;
        transient int initialized = 17;
        Base() { calls++; }
    }
    static class Derived extends Base implements Serializable {
        static int calls;
        int value = 88;
        Derived() { calls++; }
    }
    public static class Payload implements Serializable {
        private static final long serialVersionUID = 1;
        public int value = 73;
        public static void roundTrip() throws Exception {
            Payload copy = (Payload) copy(new Payload());
            check(copy.value == 73, "custom-loader value");
            check(copy.getClass().getClassLoader() == Payload.class.getClassLoader(), "custom-loader identity");
        }
    }
    static Object copy(Object value) throws Exception {
        ByteArrayOutputStream bytes = new ByteArrayOutputStream();
        try (ObjectOutputStream out = new ObjectOutputStream(bytes)) { out.writeObject(value); }
        try (ObjectInputStream in = new ObjectInputStream(new ByteArrayInputStream(bytes.toByteArray()))) {
            return in.readObject();
        }
    }
    static void check(boolean value, String message) {
        if (!value) throw new AssertionError(message);
    }
    public static void throwPlain() throws Throwable { throw new Throwable("plain"); }
    public static void main(String[] args) throws Exception {
        check(SerializationProbe.class.getField("TEXT").get(null) == TEXT, "interned constant");
        check(SerializationProbe.class.getField("UNICODE").get(null) == UNICODE, "unicode constant");
        try {
            SerializationProbe.class.getMethod("throwPlain").invoke(null);
            throw new AssertionError("missing target exception");
        } catch (InvocationTargetException e) {
            check(e.getCause().getClass() == Throwable.class, "Throwable identity");
            check(e.getCause().getMessage().equals("plain"), "Throwable message");
        }
        System.out.println("uids=" + ObjectStreamClass.lookup(Plain.class).getSerialVersionUID() + ","
            + ObjectStreamClass.lookup(Initialized.class).getSerialVersionUID() + ","
            + ObjectStreamClass.lookup(Child.class).getSerialVersionUID());
        check(((Plain) copy(new Plain())).value == 42, "plain fields");
        Child child = (Child) copy(new Child());
        check(child.value == 9 && child.extra == 11, "inherited fields");
        Derived original = new Derived();
        Derived restored = (Derived) copy(original);
        check(Base.calls == 2 && Derived.calls == 1, "serialization constructors");
        check(restored.initialized == 17 && restored.value == 88, "superclass initialization");
        check(Initialized.class.getDeclaredMethods().length == 0, "initializer hidden from reflection");
        ArrayList<Object> cycle = new ArrayList<>();
        cycle.add(cycle); cycle.add(TEXT); cycle.add(Integer.valueOf(99));
        ArrayList<?> copied = (ArrayList<?>) copy(cycle);
        check(copied.get(0) == copied && copied.get(1).equals(TEXT) && copied.get(2).equals(99), "graph identity");
        URL location = Path.of(args[0]).toUri().toURL();
        try (URLClassLoader loader = new URLClassLoader(new URL[] {location}, null)) {
            Class<?> payload = Class.forName("SerializationProbe$Payload", true, loader);
            payload.getMethod("roundTrip").invoke(null);
        }
        System.out.println("serialization/reflection passed");
    }
}
