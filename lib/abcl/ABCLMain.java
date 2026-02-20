// Custom ABCL entry point for OpenLDK.
// The stock org.armedbear.lisp.Main spawns a new thread with a 4MB stack
// to run the interpreter. OpenLDK needs it to run on the main thread.
// See: https://github.com/atgreen/openldk/issues/4

package org.armedbear.lisp;

public final class ABCLMain {
    public static void main(final String[] args) {
        try {
            Interpreter interpreter = Interpreter.createDefaultInstance(args);
            if (interpreter != null)
                interpreter.run();
        } catch (ProcessingTerminated e) {
            System.exit(e.getStatus());
        }
    }
}
