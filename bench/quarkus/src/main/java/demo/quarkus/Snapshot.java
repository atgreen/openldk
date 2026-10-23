package demo.quarkus;

import io.quarkus.arc.Arc;
import io.quarkus.runtime.Application;

// Main class of the OpenLDK snapshot image.  The first warmup run starts
// Quarkus (the build-generated io.quarkus.runner.ApplicationImpl) and keeps
// it in a static field; the image is saved with :reset-classes nil, so each
// run of it finds Quarkus started and only runs the application, as a CRaC
// restore would.
public class Snapshot {
    static Application app;

    public static void main(String[] args) throws Exception {
        if (app == null) {
            app = (Application) Class.forName("io.quarkus.runner.ApplicationImpl")
                .getDeclaredConstructor().newInstance();
            app.start(args);
        }
        Arc.container().instance(QuarkusApp.class).get().run(args);
    }
}
