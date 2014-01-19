package clojure;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import com.google.devtools.j2objc.J2ObjC;
import com.google.devtools.j2objc.Options;
import com.google.devtools.j2objc.Plugin;

public class Translate extends J2ObjC {

  public static void translate(String path, ArrayList<String> files)
      throws IOException {
    File root = new File(path);
    File[] list = root.listFiles();

    if (list == null)
      return;

    for (File f : list) {
      if (f.isDirectory()) {
        translate(f.getAbsolutePath(), files);
      } else if (f.getName().endsWith(".java")) {
        files.add(f.getCanonicalPath());
      }
    }
  }

  public static void main(String args[]) {
    translate(args, "target/objc", "target/classes");
  }

  public static void translate(String[] args, String out, String cp) {
    try {
      ArrayList<String> files = new ArrayList<String>();
      if (args.length > 0) {
        for (String arg : args) {
          translate(arg, files);
        }
      } else {
        translate(new File("src/jvm").getAbsolutePath(), files);
        translate(new File("target/gen").getAbsolutePath(), files);
        // translate(new File("test/java").getAbsolutePath(), files);
      }
      Options.load(new String[] { "-d", out, "-classpath", cp,
          (String) files.get(0) });

      try {
        initPlugins(Options.getPluginPathEntries(),
            Options.getPluginOptionString());
      } catch (IOException e) {
        error(e);
      }

      for (String f : removeDeadCode(files.toArray(new String[files.size()]))) {
        System.out.println(f);
        translate(f);
      }

      for (Plugin plugin : Options.getPlugins()) {
        plugin.endProcessing(Options.getOutputDirectory());
      }
    } catch (IOException e) {
      e.printStackTrace();
    }
  }
}
