/* SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0 */
import com.sun.source.tree.ClassTree;
import com.sun.source.util.JavacTask;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.util.*;
import javax.tools.*;

/** Compile source-launch input; never execute application code in the host JDK. */
class SourceCompiler {
    public static void main(String[] args) throws Exception {
        Path source = Path.of(args[0]).toAbsolutePath();
        Path output = Path.of(args[1]);
        boolean preview = Boolean.parseBoolean(args[3]);
        String text = Files.readString(source);
        if (text.startsWith("#!")) {
            int newline = text.indexOf('\n');
            text = newline < 0 ? "" : text.substring(newline);
        }
        final String content = text;
        JavaFileObject input = new SimpleJavaFileObject(source.toUri(), JavaFileObject.Kind.SOURCE) {
            @Override public CharSequence getCharContent(boolean ignoreEncodingErrors) { return content; }
            @Override public boolean isNameCompatible(String simpleName, Kind kind) { return kind == Kind.SOURCE; }
        };
        JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        try (var files = compiler.getStandardFileManager(null, null, StandardCharsets.UTF_8)) {
            var options = new ArrayList<>(List.of("-proc:none", "--release", args[4], "-d", output.toString(),
                    "-classpath", args[2], "-encoding", "UTF-8"));
            if (preview) options.add("--enable-preview");
            for (int i = 5; i < args.length; i++) {
                options.add("--add-modules");
                options.add(args[i]);
            }
            DiagnosticCollector<JavaFileObject> diagnostics = new DiagnosticCollector<>();
            JavacTask task = (JavacTask)compiler.getTask(null, files, diagnostics, options, null, List.of(input));
            var unit = task.parse().iterator().next();
            String name = null;
            for (var declaration : unit.getTypeDecls()) {
                if (declaration instanceof ClassTree type) { name = type.getSimpleName().toString(); break; }
            }
            if (name == null) {
                System.err.println("Source contains no launchable class: " + source);
                System.exit(1);
            }
            // Source dependencies are resolved relative to the package's source root.
            Path root = source.getParent();
            if (unit.getPackageName() != null) {
                String[] parts = unit.getPackageName().toString().split("\\.");
                Path candidate = root;
                boolean matches = true;
                for (int i = parts.length - 1; i >= 0; i--) {
                    if (candidate == null || !candidate.getFileName().toString().equals(parts[i])) { matches = false; break; }
                    candidate = candidate.getParent();
                }
                if (matches) root = candidate;
                name = unit.getPackageName() + "." + name;
            }
            files.setLocationFromPaths(StandardLocation.SOURCE_PATH, List.of(root));
            task.generate();
            boolean failed = false;
            for (var diagnostic : diagnostics.getDiagnostics()) {
                System.err.println(diagnostic);
                failed |= diagnostic.getKind() == Diagnostic.Kind.ERROR;
            }
            if (failed) System.exit(1);
            Files.writeString(output.resolve("main-class"), name, StandardCharsets.UTF_8);
        }
    }
}
