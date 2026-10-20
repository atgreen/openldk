// Custom ABCL entry point for OpenLDK.
// The stock org.armedbear.lisp.Main spawns a new thread with a 4MB stack
// to run the interpreter. OpenLDK needs it to run on the main thread.
// See: https://github.com/atgreen/openldk/issues/4

package org.armedbear.lisp;

//
// In a warm image (make abcl, docs/IMAGES.md) the interpreter was already
// created and initialized at build time, so reuse it instead of returning
// without a REPL, as createDefaultInstance does when an instance exists.
// Its command-line arguments were processed then, not now.

public final class ABCLMain {
    public static void main(final String[] args) {
        try {
            Interpreter interpreter = Interpreter.getInstance();
            if (interpreter == null)
                interpreter = Interpreter.createDefaultInstance(args);
            if (interpreter != null)
                interpreter.run();
        } catch (ProcessingTerminated e) {
            System.exit(e.getStatus());
        }
    }
}
