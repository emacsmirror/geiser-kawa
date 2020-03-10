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

        // In a previous version definitions were like this:
        // lang.defineFunction(new GeiserEval("geiser:eval"));
        // However, that meant you were forced to extend Procedure1, Procedure2, ...
        // which can only have arguments of type Object and must be instanced to be used.
        // This means that compared to static methods you don't have:
        // - parameter names in autodoc: I couldn't find a way to get parameter names for instance methods
        // - parameter types in autodoc: because their types must be be Object
        // - type warnings: for the same reason of the previous point
        // Another advantage with this new approach is we don't have to add all the boilerplate
        // for checking argument types.
        try {
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
        } catch (Throwable throwable) {
            throwable.printStackTrace();
        }
    }

}
