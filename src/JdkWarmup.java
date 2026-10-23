package openldk.warmup;

import java.io.File;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.file.Files;
import java.security.MessageDigest;
import java.security.SecureRandom;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.UUID;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.jar.JarOutputStream;
import java.util.regex.Pattern;

// make-image runs this once while building the openldk executable, so the
// JDK code it calls -- code many programs run once, which takes OpenLDK
// seconds to compile on first use -- is already compiled in the image.  It
// then resets the classes holding state that belongs to a run (random seeds,
// the default time zone): the image keeps their compiled code, not that
// state.  See %WARM-UP-JDK.
public class JdkWarmup {
    public static void main(String[] args) throws Exception {
        new SecureRandom().nextLong();
        UUID.randomUUID();
        MessageDigest.getInstance("SHA-256").digest(new byte[64]);
        ZoneId.systemDefault();
        LocalDateTime.now();
        ZonedDateTime.now().format(DateTimeFormatter.ISO_OFFSET_DATE_TIME);
        String.format("%d %s %.2f %x", 1, "a", 2.5, 255);
        Pattern.compile("a+(b|c)*").matcher("aabcb").matches();
        new BigDecimal("1.25").multiply(new BigDecimal("3")).toPlainString();
        BigInteger.valueOf(2).pow(100).toString();
        File jar = File.createTempFile("warmup", ".jar");
        try (JarOutputStream out = new JarOutputStream(Files.newOutputStream(jar.toPath()))) {
            out.putNextEntry(new JarEntry("a/b.txt"));
            out.write("hi".getBytes());
            out.closeEntry();
        }
        try (JarFile file = new JarFile(jar)) {
            file.getJarEntry("a/b.txt");
            file.entries().hasMoreElements();
            file.getInputStream(file.getJarEntry("a/b.txt")).readAllBytes();
        }
        // A thread's ThreadLocalMap grows, rehashing and expunging stale
        // entries, once it holds a dozen or so locals; a program's main
        // thread gets there, but the thread building the image already has.
        Thread thread = new Thread(() -> {
            for (int i = 0; i < 40; i++) {
                new ThreadLocal<Integer>().set(i);
            }
        });
        thread.start();
        thread.join();
        Files.readAllLines(jar.toPath(), java.nio.charset.StandardCharsets.ISO_8859_1);
        jar.delete();
    }
}
