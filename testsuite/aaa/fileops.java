/* { dg-output "OK" } */

package aaa;

import java.io.File;

// File operations backed by UnixFileSystem natives: temporary files
// (createFileExclusively0), directories, renaming, timestamps, permissions.
public class fileops
{
    public static void main(String[] args) throws Exception
    {
        StringBuilder fails = new StringBuilder();
        File tmp = File.createTempFile("openldk", ".txt");
        if (!tmp.exists()) fails.append(" createTempFile");
        if (tmp.createNewFile()) fails.append(" createNewFile-existing");
        File dir = new File(tmp.getParentFile(), tmp.getName() + ".d");
        if (!dir.mkdir() || !dir.isDirectory()) fails.append(" mkdir");
        File moved = new File(dir, "moved.txt");
        if (!tmp.renameTo(moved) || tmp.exists() || !moved.exists()) fails.append(" renameTo");
        if (!moved.setLastModified(1000000000000L) || moved.lastModified() != 1000000000000L)
            fails.append(" setLastModified");
        if (!moved.setReadOnly() || moved.canWrite()) fails.append(" setReadOnly");
        if (!moved.setWritable(true) || !moved.canWrite()) fails.append(" setWritable");
        if (!moved.delete() || !dir.delete()) fails.append(" delete");
        System.out.println(fails.length() == 0 ? "OK" : "FAIL:" + fails);
    }
}
