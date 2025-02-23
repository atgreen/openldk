import java.io.*;

public class euc_jp
{
    public static void main(String argv[])
    {
            write("¡¡¡¢¡£¡¤¡¥¡¦¡§¡¨¡©¡ª¡«¡¬¡­¡®¡¯¡°¡±¡²¡³¡´¡µ¡¶¡·¡¸¡¹¡º¡»¡¼¡½¡¾¡¿¡À¡Á¡Â¡Ã");
    }

    public static void write(String s)
    {
        StringBuffer sb=new StringBuffer();
        for(int i=0;i<s.length();i++)
                sb.append(" "+(int)s.charAt(i));
        sb.append("\n");
        System.out.println(sb.toString());
    }

}
