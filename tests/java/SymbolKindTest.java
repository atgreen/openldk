// Copyright (C) 2025 Anthony Green <green@moxielogic.com>
//
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
//
// Stand-alone sanity checks that exercise OpenJDK's javac front-end directly.
// These tests let us compare OpenLDK's behaviour with the reference
// implementation without any Lisp machinery involved.

import com.sun.tools.javac.code.Kinds;
import com.sun.tools.javac.code.Scope;
import com.sun.tools.javac.code.Symbol;
import com.sun.tools.javac.code.Type;
import com.sun.tools.javac.code.TypeTag;
import com.sun.tools.javac.code.Types;
import com.sun.tools.javac.code.Symbol.ClassSymbol;
import com.sun.tools.javac.code.Symbol.MethodSymbol;
import com.sun.tools.javac.code.Symbol.TypeSymbol;
import com.sun.tools.javac.code.Symbol.VarSymbol;
import com.sun.tools.javac.api.JavacTaskImpl;
import com.sun.tools.javac.api.JavacTool;
import com.sun.tools.javac.util.Context;
import com.sun.tools.javac.util.List;
import com.sun.tools.javac.util.Name;
import com.sun.tools.javac.util.Names;

import javax.tools.DiagnosticCollector;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileManager;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.StandardLocation;
import javax.tools.ToolProvider;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;

/**
 * Run with the JDK's {@code tools.jar} on the classpath (JDK 8) or simply with
 * the JDK modules on the module-path (JDK 9+).
 *
 * Example (JDK 8):
 *
 * <pre>
 *   javac -classpath $JAVA_HOME/lib/tools.jar tests/java/SymbolKindTest.java
 *   java  -classpath $JAVA_HOME/lib/tools.jar:. SymbolKindTest
 * </pre>
 */
public final class SymbolKindTest {

    public static void main(String[] args) throws Exception {
        SymbolKindTest self = new SymbolKindTest();
        self.inspectPlatformClass("java/lang/Comparable");
        self.inspectPlatformClass("java/util/Map$Entry");
        self.inspectGeneratedNestedClass();
        System.out.println("SymbolKindTest: all checks passed.");
    }

    private JavacTaskImpl newTask(String extraClassPath) {
        JavacTool tool = JavacTool.create();
        StandardJavaFileManager fileManager = tool.getStandardFileManager(null, null, null);
        String sep = System.getProperty("path.separator");
        String cp = System.getProperty("java.class.path", "");
        String boot = System.getProperty("sun.boot.class.path", "");
        if (extraClassPath != null && !extraClassPath.isEmpty()) {
            cp = cp.isEmpty() ? extraClassPath : cp + sep + extraClassPath;
        }
        try {
            if (!cp.isEmpty()) {
                fileManager.setLocation(StandardLocation.CLASS_PATH, toFiles(cp, sep));
            }
            if (!boot.isEmpty()) {
                fileManager.setLocation(StandardLocation.PLATFORM_CLASS_PATH, toFiles(boot, sep));
            }
        } catch (IOException ex) {
            throw new IllegalStateException("Failed to configure file manager locations", ex);
        }
        java.util.List<String> opts = new ArrayList<>();
        if (!cp.isEmpty()) {
            opts.addAll(Arrays.asList("-classpath", cp));
        }
        if (!boot.isEmpty()) {
            opts.addAll(Arrays.asList("-bootclasspath", boot));
        }
        opts.add("-XDignore.symbol.file");
        @SuppressWarnings({"unchecked", "rawtypes"})
        JavacTaskImpl task = (JavacTaskImpl) tool.getTask(
            null, fileManager, null, opts, null, Collections.<JavaFileObject>emptyList());
        return task;
    }

    private static Iterable<File> toFiles(String pathList, String separator) {
        java.util.List<File> files = new ArrayList<>();
        for (String entry : pathList.split(java.util.regex.Pattern.quote(separator))) {
            if (!entry.isEmpty()) {
                files.add(new File(entry));
            }
        }
        return files;
    }

    /**
     * Resolve a well-known JDK class via the javac front-end and assert that
     * the symbol table exposes the expected kinds and canonical types.
     */
    private void inspectPlatformClass(String binaryName) throws Exception {
        JavacTaskImpl task = newTask(null);
        Context ctx = task.getContext();
        Names names = Names.instance(ctx);
        String canonicalName = binaryName.replace('/', '.').replace('$', '.');
        Symbol.ClassSymbol sym = (Symbol.ClassSymbol) task.getElements().getTypeElement(canonicalName);
        if (sym == null) {
            throw new AssertionError("Failed to resolve " + canonicalName);
        }

        assertKind(sym, Kinds.TYP, "class symbol kind for " + binaryName);
        assertSame(sym.type, sym.type.tsym.type, "class symbol canonical type for " + binaryName);

        // Walk all immediate members and ensure their kind bits line up with the reference JDK.
        for (Symbol member : sym.members().getElements()) {
            if (member instanceof MethodSymbol) {
                assertKind(member, Kinds.MTH, binaryName + "." + member);
            } else if (member instanceof VarSymbol) {
                assertKind(member, Kinds.VAR, binaryName + "." + member);
            } else if (member instanceof ClassSymbol) {
                // Nested types must also report TYP.
                assertKind(member, Kinds.TYP, binaryName + "." + member);
            }
        }
    }

    /**
     * Compile a tiny source snippet containing a generic outer class and a
     * non-static inner class. This exercises the same code paths that currently
     * trigger {@code setEnclosingType} failures in OpenLDK.
     */
    private void inspectGeneratedNestedClass() throws Exception {
        String source =
            "class Host<X> {\n" +
            "  class Inner<Y> {\n" +
            "    Y field;\n" +
            "    Host<X> enclosing() { return Host.this; }\n" +
            "  }\n" +
            "}\n";

        Path temp = Files.createTempDirectory("symbol-kind-test");
        compileSource(temp, "Host", source);

        JavacTaskImpl task = newTask(temp.toString());
        Context ctx = task.getContext();
        Names names = Names.instance(ctx);

        ClassSymbol inner = (ClassSymbol) task.getElements().getTypeElement("Host.Inner");
        if (inner == null) {
            throw new AssertionError("Failed to resolve Host.Inner");
        }

        assertKind(inner, Kinds.TYP, "Host$Inner kind");
        assertSame(inner.type, inner.type.tsym.type, "Host$Inner canonical type");

        Type enclosing = ((Type.ClassType) inner.type).getEnclosingType();
        if (!enclosing.hasTag(TypeTag.CLASS)) {
            throw new AssertionError("Expected enclosing type for Host$Inner, found " + enclosing);
        }

        // Verify the field and method inside Inner retain the expected kinds.
        Scope scope = inner.members();
        for (Symbol member : scope.getElements()) {
            if (member instanceof VarSymbol && member.name.contentEquals("field")) {
                assertKind(member, Kinds.VAR, "Host$Inner.field kind");
            } else if (member instanceof MethodSymbol && member.name.contentEquals("enclosing")) {
                assertKind(member, Kinds.MTH, "Host$Inner.enclosing() kind");
            }
        }
    }

    private static void compileSource(Path outputDir, String className, String source) throws IOException {
        JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        if (compiler == null) {
            throw new IllegalStateException("No system Java compiler found. Run on a JDK, not a JRE.");
        }
        DiagnosticCollector<JavaFileObject> diagnostics = new DiagnosticCollector<>();
        try (StandardJavaFileManager fm = compiler.getStandardFileManager(diagnostics, null, null)) {
            fm.setLocation(StandardLocation.CLASS_OUTPUT, Collections.singletonList(outputDir.toFile()));
            JavaFileObject file = new SourceFileObject(className, source);
            JavaCompiler.CompilationTask task =
                compiler.getTask(null, fm, diagnostics, Arrays.asList("-g:none"), null, Collections.singletonList(file));
            if (!task.call()) {
                throw new IllegalStateException("Compilation failed: " + diagnostics.getDiagnostics());
            }
        }
    }

    private static void assertKind(Symbol sym, int expected, String message) {
        if (sym.kind != expected) {
            throw new AssertionError(message + ": expected kind mask " + expected + " but found " + sym.kind);
        }
    }

    private static void assertSame(Object a, Object b, String message) {
        if (a != b) {
            throw new AssertionError(message + ": expected identity equality");
        }
    }

    /**
     * In-memory source file for the temporary compilation step.
     */
    private static final class SourceFileObject extends SimpleJavaFileObject {
        private final String contents;

        private SourceFileObject(String className, String contents) {
            super(URI.create("string:///" + className.replace('.', '/') + Kind.SOURCE.extension), Kind.SOURCE);
            this.contents = contents;
        }

        @Override
        public CharSequence getCharContent(boolean ignoreEncodingErrors) {
            return contents;
        }
    }

}
