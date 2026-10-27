// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
package org.openldk.eclipse.test;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.eclipse.jdt.core.*;
import org.eclipse.jdt.launching.JavaRuntime;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.*;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.texteditor.ITextEditor;

/** Exercises the real editor, workspace, JDT builder, and problem markers. */
public class SmokeTest implements IStartup {
    @Override public void earlyStartup() {
        PlatformUI.getWorkbench().getDisplay().asyncExec(() -> {
            try {
                IWorkspace workspace = ResourcesPlugin.getWorkspace();
                IProject project = workspace.getRoot().getProject("OpenLDKSmoke");
                if (project.exists()) project.delete(true, true, null);
                project.create(null);
                project.open(null);
                IProjectDescription description = project.getDescription();
                description.setNatureIds(new String[]{JavaCore.NATURE_ID});
                project.setDescription(description, null);
                IFolder src = project.getFolder("src"), bin = project.getFolder("bin");
                src.create(true, true, null);
                bin.create(true, true, null);
                IJavaProject javaProject = JavaCore.create(project);
                javaProject.setOutputLocation(bin.getFullPath(), null);
                javaProject.setRawClasspath(new IClasspathEntry[]{
                    JavaCore.newSourceEntry(src.getFullPath()),
                    JavaRuntime.getDefaultJREContainerEntry()}, null);
                Map<String, String> options = javaProject.getOptions(false);
                JavaCore.setComplianceOptions(JavaCore.VERSION_25, options);
                javaProject.setOptions(options);
                System.out.println("ECLIPSE CHECK: Java project configured");
                IFile file = src.getFile("Hello.java");
                file.create(new ByteArrayInputStream("public class Hello {}".getBytes(StandardCharsets.UTF_8)), true, null);
                IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
                if (window == null || !window.getShell().isVisible())
                    throw new AssertionError("Workbench window is not visible");
                var introManager = PlatformUI.getWorkbench().getIntroManager();
                var intro = introManager.getIntro();
                if (intro != null) introManager.closeIntro(intro);
                IEditorPart part = IDE.openEditor(window.getActivePage(), file);
                if (!(part instanceof ITextEditor)) throw new AssertionError("Java text editor did not open");
                ITextEditor editor = (ITextEditor) part;
                IDocument document = editor.getDocumentProvider().getDocument(editor.getEditorInput());
                System.out.println("ECLIPSE CHECK: Java editor opened in visible workbench");
                document.set("public class Hello { int broken = \"wrong type\"; }\n");
                editor.doSave(null);
                project.build(IncrementalProjectBuilder.FULL_BUILD, null);
                int brokenErrors = errors(project);
                if (brokenErrors == 0) throw new AssertionError("JDT missed a Java type error");
                System.out.println("ECLIPSE CHECK: saved invalid source produced " + brokenErrors + " error markers");
                document.set("public class Hello {\n    public static void main(String[] args) {\n        System.out.println(\"Hello from Eclipse on OpenLDK\");\n    }\n}\n");
                editor.doSave(null);
                project.build(IncrementalProjectBuilder.FULL_BUILD, null);
                if (errors(project) != 0) throw new AssertionError("JDT did not clear compilation errors");
                if (!bin.getFile("Hello.class").exists()) throw new AssertionError("No compiled Hello.class");
                System.out.println("ECLIPSE CHECK: corrected source saved and built without errors");
                PlatformUI.getWorkbench().getDisplay().timerExec(1000, () -> {
                    report("PASS: workbench, Java editor, save, error diagnostics, corrected build\n");
                    if (System.getProperty("openldk.eclipse.close") != null) awaitClose();
                });
            } catch (Throwable error) {
                StringWriter detail = new StringWriter();
                error.printStackTrace(new PrintWriter(detail));
                report("FAIL: " + detail);
            }
        });
    }

    private static void awaitClose() {
        PlatformUI.getWorkbench().getDisplay().timerExec(200, () -> {
            if (new File(System.getProperty("openldk.eclipse.close")).isFile())
                PlatformUI.getWorkbench().close();
            else awaitClose();
        });
    }

    private static int errors(IProject project) throws CoreException {
        int count = 0;
        for (IMarker marker : project.findMarkers(IMarker.PROBLEM, true, IResource.DEPTH_INFINITE))
            if (marker.getAttribute(IMarker.SEVERITY, 0) == IMarker.SEVERITY_ERROR) count++;
        return count;
    }

    private static void report(String text) {
        System.out.print(text);
        try (FileOutputStream output = new FileOutputStream(System.getProperty("openldk.eclipse.result"))) {
            output.write(text.getBytes(StandardCharsets.UTF_8));
        } catch (IOException error) { error.printStackTrace(); }
    }
}
