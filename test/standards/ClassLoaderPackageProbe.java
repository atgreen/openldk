// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0

import java.io.InputStream;
import java.net.URL;

public class ClassLoaderPackageProbe {
    static int checks;
    static void check(boolean value, String message) {
        checks++;
        if (!value) throw new AssertionError(message);
    }
    static class Loader extends ClassLoader {
        Loader(ClassLoader parent) { super(parent); }
        Package inherited(String name) { return getPackage(name); }
        Class<?> define(String name, byte[] bytes) { return defineClass(name, bytes, 0, bytes.length); }
        Package metadata(String name) throws Exception {
            return definePackage(name, "spec", "1", "vendor", "impl", "2", "maker",
                                 new URL("file:/sealed/"));
        }
    }
    static boolean listed(ClassLoader loader, String name) {
        for (Package p : loader.getDefinedPackages()) {
            if (p.getName().equals(name)) return true;
        }
        return false;
    }
    public static void main(String[] args) throws Exception {
        ClassLoader system = ClassLoader.getSystemClassLoader();
        Loader child = new Loader(system);
        check(system.getDefinedPackage("") != null, "main class's default package");
        check(system.getDefinedPackage("alpha.fixture") == null, "unloaded package");
        check(!listed(system, "alpha.fixture"), "unloaded package not listed");
        check(child.inherited("alpha.fixture") == null, "unloaded parent package");
        Class<?> first = Class.forName("alpha.fixture.One", false, system);
        Package alpha = system.getDefinedPackage("alpha.fixture");
        check(alpha != null, "class loading defines package before Class.getPackage");
        check(alpha.getName().equals("alpha.fixture"), "package name");
        check(listed(system, "alpha.fixture"), "package enumeration");
        check(first.getClassLoader() == system, "system loader identity");
        check(first.getPackage() == alpha, "class and loader share package identity");
        check(system.loadClass("alpha.fixture.One").getPackage() == alpha, "stable package identity");
        check(child.inherited("alpha.fixture") == alpha, "parent package inheritance");
        check(child.loadClass("alpha.fixture.One") == first, "delegation");
        check(child.getDefinedPackage("alpha.fixture") == null, "delegation does not define child package");
        check(!listed(child, "alpha.fixture"), "delegated package not listed on child");
        check(system.getDefinedPackage("beta.fixture") == null, "other classpath package stays undefined");
        Class.forName("beta.fixture.Two", false, system);
        check(system.getDefinedPackage("beta.fixture") != null, "second package");
        check(system.getDefinedPackage("java.lang") == null, "bootstrap package not defined on app loader");
        check(child.inherited("java.lang") != null, "bootstrap package inherited");
        check(String[].class.getPackage() == null && int.class.getPackage() == null,
              "array and primitive packages");

        byte[] bytes;
        try (InputStream input = system.getResourceAsStream("alpha/fixture/One.class")) {
            check(input != null, "class bytes");
            bytes = input.readAllBytes();
        }
        Loader isolated = new Loader(null);
        Package explicit = isolated.metadata("alpha.fixture");
        Class<?> own = isolated.define("alpha.fixture.One", bytes);
        check(own != first && own.getClassLoader() == isolated, "independent definition");
        check(own.getPackage() == explicit && isolated.getDefinedPackage("alpha.fixture") == explicit,
              "explicit package metadata retained");
        check(explicit.getSpecificationTitle().equals("spec") &&
              explicit.getSpecificationVendor().equals("vendor") &&
              explicit.getImplementationVersion().equals("2") && explicit.isSealed(), "metadata");
        Loader sibling = new Loader(null);
        Class<?> other = sibling.define("alpha.fixture.One", bytes);
        check(other.getPackage() != explicit && other.getPackage() != alpha,
              "package identity scoped to defining loader");
        check(sibling.getDefinedPackage("alpha.fixture") == other.getPackage(), "implicit user package");

        String[] invalid = {"", ".", "..", "gnu", "gnu.", ".alpha.fixture.One",
            "java/lang/String", ".alpha/fixture.One", "/alpha/fixture/One", "[", "[[", "[]",
            "[I", "[Ljava.lang.String;", "L;", "L.", "L[", "[L;", "[L[;", "[L.;",
            "[Lgnu;", "[Lgnu.;", "alpha.fixture.One;", "alpha.fixture.One\u0000", "int", "void"};
        for (String name : invalid) {
            try {
                system.loadClass(name);
                throw new AssertionError("accepted invalid class name: " + name);
            } catch (ClassNotFoundException expected) { checks++; }
        }
        for (String name : new String[] {"alpha..fixture.One", "alpha/fixture/One", "alpha/fixture.One", "alpha.fixture/One"}) {
            try {
                system.loadClass(name);
                throw new AssertionError("accepted class bytes under the wrong binary name: " + name);
            } catch (NoClassDefFoundError expected) { checks++; }
        }
        Loader mismatched = new Loader(null);
        try {
            mismatched.define("alpha.fixture.Wrong", bytes);
            throw new AssertionError("accepted mismatched defineClass name");
        } catch (NoClassDefFoundError expected) { checks++; }
        check(mismatched.getDefinedPackage("alpha.fixture") == null,
              "failed definition does not publish package");
        check(mismatched.define("alpha.fixture.One", bytes).getName().equals("alpha.fixture.One"),
              "valid definition after failed name check");
        check(system.loadClass("java.lang.String") == String.class, "normal name still resolves");
        System.out.println("Class-loader names and package bookkeeping: " + checks + " checks passed");
    }
}
