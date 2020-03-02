/*
 * Copyright (C) 2020 spellcard199 <spellcard199@protonmail.com>
 * This is free software;  for terms and warranty disclaimer see ./COPYING.
 */

package kawageiser;

public class StartKawaWithGeiserSupport {

    public static void main(String[] args) {
        if (args.length == 0) {
            int defaultPort = 37146;
            System.out.println(
                    String.format(
                            "No port specified. Starting kawa server on default port (%d)...",
                            defaultPort));
            startKawaServerWithGeiserSupport(defaultPort);
        } else if (args.length == 1 && args[0].matches("[0-9]+")) {
            int port = Integer.parseInt(args[0]);
            startKawaServerWithGeiserSupport(port);
        } else if (args.length == 1 && args[0].equals("--no-server")) {
            System.out.println("Starting kawa repl in current terminal...");
            startKawaReplWithGeiserSupport();
        } else {
            System.out.println(
                    "You must pass at most 1 argument and it can be only one of:\n"
                            + "- a port number"
                            + "- --no-server"
            );
        }
    }

    public static void startKawaReplWithGeiserSupport() {
        String[] interpArgs = new String[]{
                "-e", "(require <kawageiser.Geiser>)",
                "--",
        };
        runSchemeAsApplication(interpArgs);
    }

    public static void startKawaServerWithGeiserSupport(int port) {
        String[] interpArgs = new String[]{
                "-e", "(require <kawageiser.Geiser>)",
                "--server", String.valueOf(port)};
        runSchemeAsApplication(interpArgs);
    }

    public static void runSchemeAsApplication(String[] args) {
        kawa.standard.Scheme scheme = kawa.standard.Scheme.getInstance();
        scheme.runAsApplication(args);
    }

}
