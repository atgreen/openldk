import java.io.*;

public class utf8
{
    public static void main(String argv[])
    {
            write("¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾<BR>\r\n");
            write("ĀāĂăĄąĆćĈĉĊċČčĎďĐđĒē<BR>\r\n");
            write("ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡ<BR>\r\n");
            write("абвгдежзийклмнопрстуфхцчшщъыьэюя<BR>\r\n");
            write("אבגדהוזחטיךכלםמןנסעףפץצקרש<BR>\r\n");
            write("ءآأؤإئابةتثجحخدذرزسشصضطظع<BR>\r\n");
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
