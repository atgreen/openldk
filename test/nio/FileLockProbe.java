// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.nio.channels.*;
import java.nio.file.*;
import java.io.RandomAccessFile;
import java.io.FileDescriptor;
import java.io.FileOutputStream;
import java.io.SyncFailedException;

public class FileLockProbe {
    public static void main(String[] args) throws Exception {
        try { new FileDescriptor().sync(); throw new AssertionError("Invalid descriptor synchronized"); }
        catch (SyncFailedException expected) { }
        Path synced = Path.of(args[0] + ".sync");
        FileDescriptor descriptor;
        try (FileOutputStream output = new FileOutputStream(synced.toFile())) {
            descriptor = output.getFD();
            output.write(new byte[]{1, 2, 3});
            descriptor.sync();
            if (!java.util.Arrays.equals(Files.readAllBytes(synced), new byte[]{1, 2, 3}))
                throw new AssertionError("Sync did not flush buffered output");
        }
        try { descriptor.sync(); throw new AssertionError("Closed descriptor synchronized"); }
        catch (SyncFailedException expected) { }
        Files.delete(synced);
        // Equinox reopens existing lock files through RandomAccessFile.
        for (String mode : new String[]{"rw", "rws", "rwd", "r"}) {
            try (RandomAccessFile file = new RandomAccessFile(args[0], mode)) {
                if (file.length() != 64) throw new AssertionError("Existing file was truncated");
                file.seek(64);
                if (file.read(new byte[1]) != -1 || file.read(new byte[0]) != 0)
                    throw new AssertionError("RandomAccessFile EOF");
            }
        }
        try (FileChannel channel = FileChannel.open(Path.of(args[0]), StandardOpenOption.READ, StandardOpenOption.WRITE)) {
            boolean shared = args[1].equals("shared");
            FileLock lock = channel.tryLock(0, 16, shared);
            if (args[1].equals("blocked")) {
                if (lock != null) { lock.release(); throw new AssertionError("Contended lock acquired"); }
                System.out.println("CONTENDED");
                return;
            }
            if (lock == null) throw new AssertionError("Uncontended lock failed");
            try (FileChannel other = FileChannel.open(Path.of(args[0]), StandardOpenOption.READ, StandardOpenOption.WRITE)) {
                try {
                    FileLock overlap = other.tryLock(0, 16, shared);
                    if (overlap != null) overlap.release();
                    throw new AssertionError("Missing overlapping lock exception");
                } catch (OverlappingFileLockException expected) { }
                // POSIX closes release process locks, so keep this second
                // descriptor open throughout the inter-process checks.
                System.out.println("LOCKED");
                System.out.flush();
                System.in.read();
                lock.release();
                System.out.println("RELEASED");
                System.out.flush();
                System.in.read();
            }
        }
    }
}
