/*
 * Copyright (C) 2020 spellcard199 <spellcard199@protonmail.com>
 * This is free software;  for terms and warranty disclaimer see ./COPYING.
 */

package kawageiser;

import gnu.expr.Language;
import kawa.TelnetRepl;
import kawa.standard.Scheme;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.ServerSocket;

public class StartKawaWithGeiserSupport {

    public static void main(String[] args) throws IOException {
        Scheme scheme = new Scheme();
        if (args.length == 0) {
            int defaultPort = 37146;
            System.out.println(
                    String.format(
                            "No port specified. Starting kawa server on default port (%d)...",
                            defaultPort));
            // NOTE: You can use ssocket.close() to forcefully free port:
            //       that's why we are passing around a ServerSocket instead of an int port
            // NOTE:
            // 1. You can't reuse address if you use the constructor new ServerSocket(int port);
            // 2. That's why we used the following 3 lines here
            ServerSocket ssocket = makeServerSocketWithReusableAddress(defaultPort);
            Thread kawaServerThread = makeThreadForKawaServerWithGeiserSupport(ssocket, scheme);
            kawaServerThread.start();

        } else if (args.length == 1 && args[0].matches("[0-9]+")) {
            // NOTE: You can use ssocket.close() to forcefully free port:
            //       that's why we are passing around a ServerSocket instead of an int port
            int port = Integer.parseInt(args[0]);
            ServerSocket ssocket = makeServerSocketWithReusableAddress(port);
            Thread kawaServerThread = makeThreadForKawaServerWithGeiserSupport(ssocket, scheme);
            kawaServerThread.start();

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

    public static Scheme startKawaReplWithGeiserSupport() {
        String[] interpArgs = new String[]{
                "-e", "(require <kawageiser.Geiser>)",
                "--",
        };
        return runSchemeAsApplication(interpArgs);
    }

    public static Scheme runSchemeAsApplication(String[] args) {
        kawa.standard.Scheme scheme = kawa.standard.Scheme.getInstance();
        scheme.runAsApplication(args);
        return scheme;

    }

    public static Thread
    makeThreadForKawaServerWithGeiserSupport(ServerSocket ssocket, Scheme scheme) {
        return new Thread(() -> {
            try {
                startKawaServerWithGeiserSupport(ssocket, scheme);
            } catch (Throwable throwable) {
                throwable.printStackTrace();
            }
        });
    }

    public static ServerSocket
    makeServerSocketWithReusableAddress(int port) throws IOException {
        ServerSocket ssocket = new ServerSocket();
        ssocket.setReuseAddress(true);
        ssocket.bind(new InetSocketAddress(port));
        return ssocket;
    }

    private static void
    startKawaServerWithGeiserSupport(ServerSocket ssocket,
                                     Scheme scheme) throws Throwable {
        scheme.eval("(require <kawageiser.Geiser>)");
        startKawaServer(ssocket, scheme);
    }

    private static void
    startKawaServer(ServerSocket ssocket, Scheme scheme) throws IOException {
        // Adapted from Kawa's code in kawa/repl.java
        System.err.println("Listening on port " + ssocket.getLocalPort());
        for (; ; ) {
            System.err.print("waiting ... ");
            System.err.flush();
            java.net.Socket client = ssocket.accept();
            System.err.println("got connection from "
                    + client.getInetAddress()
                    + " port:" + client.getPort());
            Language saveLang = Language.getDefaultLanguage();
            try {
                Language.setCurrentLanguage(scheme);
                TelnetRepl.serve(scheme, client);
            } finally {
                Language.setCurrentLanguage(saveLang);
            }
        }
    }

}
