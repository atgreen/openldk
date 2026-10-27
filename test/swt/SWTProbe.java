// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.*;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.graphics.*;
import java.nio.file.Files;
import java.nio.file.Path;
public class SWTProbe {
    public static void main(String[] args) throws Exception {
        System.out.println("SWT version: " + SWT.getVersion());
        Display display = new Display();
        Path png = null;
        Image image = null;
        try {
            png = Files.createTempFile("openldk-swt-", ".png");
            ImageData pixels = new ImageData(2, 2, 24, new PaletteData(0xff0000, 0xff00, 0xff));
            for (int y = 0; y < 2; y++)
                for (int x = 0; x < 2; x++) pixels.setPixel(x, y, 0x7f7f7f);
            ImageLoader writer = new ImageLoader();
            writer.data = new ImageData[]{pixels};
            writer.save(png.toString(), SWT.IMAGE_PNG);
            Image loadedImage = new Image(display, png.toString());
            image = loadedImage;
            Shell shell = new Shell(display);
            shell.setImage(loadedImage);
            shell.setText("OpenLDK SWT headless probe");
            shell.setLayout(new FillLayout());
            Text editor = new Text(shell, SWT.MULTI | SWT.BORDER);
            editor.setText("Hello from OpenLDK and SWT");
            shell.setSize(400, 200);
            shell.open();
            display.timerExec(300, () -> {
                if (!editor.getText().equals("Hello from OpenLDK and SWT"))
                    throw new AssertionError("Text round trip failed");
                ImageData loaded = loadedImage.getImageData();
                if (!loaded.palette.getRGB(loaded.getPixel(0, 0)).equals(new RGB(127, 127, 127)))
                    throw new AssertionError("PNG file image round trip failed: "
                        + loaded.palette.getRGB(loaded.getPixel(0, 0)));
                System.out.println("SWT PNG file image: PASS");
                System.out.println("SWT window, text widget, timer callback: PASS");
                shell.dispose();
            });
            while (!shell.isDisposed()) {
                if (!display.readAndDispatch()) display.sleep();
            }
        } finally {
            if (image != null && !image.isDisposed()) image.dispose();
            display.dispose();
            if (png != null) Files.deleteIfExists(png);
        }
    }
}
