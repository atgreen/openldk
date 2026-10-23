package demo.boot;

import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.context.ConfigurableApplicationContext;

// Main class of the OpenLDK snapshot image.  The first warmup run starts Boot
// and keeps the context in a static field; the image is saved with
// :reset-classes nil, so each run of it finds the context started and only
// reruns the CommandLineRunners, as a CRaC restore would.
public class Snapshot {
    static ConfigurableApplicationContext ctx;

    public static void main(String[] args) throws Exception {
        if (ctx == null) {
            ctx = SpringApplication.run(BootApp.class, args);
        } else {
            for (CommandLineRunner runner : ctx.getBeansOfType(CommandLineRunner.class).values()) {
                runner.run(args);
            }
        }
    }
}
