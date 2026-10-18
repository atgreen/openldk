/* { dg-output "49f68a5c8493ec2c0bf489821c21fc3b" } */

package aaa;

import java.security.MessageDigest;

// MessageDigest initializes java.security.Security, which reads its master
// file through java.nio.file and so loads the nio library.
public class digest
{
    public static void main(String[] args) throws Exception
    {
        byte[] d = MessageDigest.getInstance("MD5").digest(new byte[] {104, 105});
        StringBuilder sb = new StringBuilder();
        for (byte b : d)
            sb.append(String.format("%02x", b & 0xff));
        System.out.println(sb);
    }
}
