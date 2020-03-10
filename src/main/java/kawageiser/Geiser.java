/*
 * Copyright (C) 2020 spellcard199 <spellcard199@protonmail.com>
 * This is free software;  for terms and warranty disclaimer see ./COPYING.
 */

package kawageiser;

import gnu.expr.Language;

public class Geiser implements Runnable {

    private static boolean prettyPrintResult = true;

    public static boolean isPrettyPrintResult() {
        return prettyPrintResult;
    }

    public static boolean isPrettyPrintOutput() {
        return GeiserEval.evaluator.isPrettyPrintOutput();
    }

    public static void setPrettyPrintResult(boolean v) {
        prettyPrintResult = v;
    }

    public static void setPrettyPrintOutput(boolean v) {
        GeiserEval.evaluator.setPrettyPrintOutput(v);
    }

    @Override
    public void run() {

        Language lang = Language.getDefaultLanguage();

        // In a previous version, geiser's procedures definitions were like this:
        // lang.defineFunction(new GeiserEval("geiser:eval"));
        // That meant you had to extend Procedure1, Procedure2... which can only have
        // arguments of type Object and must be instanced to be used.
        // Not that symbols are bound to static methods we can have:
        // - parameter names in autodoc: I couldn't find a way to get parameter names for instance methods
        // - parameter types in autodoc: because their types can be other than Object
        // - type warnings: for the same reason of the previous point
        // - less boilerplate for checking argument types
        try {
            if (lang.lookup("geiser:eval") == null) {
                // The reason for this if block is that if someone re-imported this module
                // and the following code was executed, this exception would happen:
                // java.lang.IllegalStateException:
                //   prohibited define/redefine of geiser:eval in #<environment kawa-environment>
                lang.defineFunction(
                        "geiser:eval",
                        lang.eval("kawageiser.GeiserEval:evalStr"));
                lang.defineFunction(
                        "geiser:autodoc",
                        lang.eval("kawageiser.GeiserAutodoc:autodoc"));
                lang.defineFunction(
                        "geiser:module-completions",
                        lang.eval("kawageiser.GeiserCompleteModule:completeModule"));
                lang.defineFunction(
                        "geiser:load-file",
                        lang.eval("kawageiser.GeiserLoadFile:loadFile"));
                lang.defineFunction(
                        "geiser:completions",
                        lang.eval("kawageiser.GeiserCompleteSymbol:getCompletions"));
                lang.defineFunction(
                        "geiser:no-values",
                        lang.eval("kawageiser.GeiserNoValues:noValues"));
                lang.defineFunction(
                        "geiser:complete-java",
                        lang.eval("kawageiser.java.GeiserCompleteJava:completeJava"));
                lang.defineFunction(
                        "geiser:manual-epub-unzip-to-tmp-dir",
                        lang.eval("kawageiser.geiserDoc.ManualEpubUnzipToTmpDir:unzipToTmpDir"));
                lang.defineFunction(
                        "geiser:macroexpand",
                        lang.eval("kawageiser.GeiserMacroexpand:expand"));
            }
        } catch (Throwable throwable) {
            throwable.printStackTrace();
        }
    }

}
