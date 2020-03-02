/*
 * Copyright (C) 2020 spellcard199 <spellcard199@protonmail.com>
 * This is free software;  for terms and warranty disclaimer see ./COPYING.
 */

package kawageiser;

import gnu.expr.Language;
import kawageiser.geiserDoc.ManualEpubUnzipToTmpDir;

public class Geiser implements Runnable {
    private static boolean prettyPrintResult = true;
    public static boolean isPrettyPrintResult() { return prettyPrintResult; }
    public static boolean isPrettyPrintOutput() { return evaluator.isPrettyPrintOutput(); }
    public static void setPrettyPrintResult(boolean v) { prettyPrintResult = v; }
    public static void setPrettyPrintOutput(boolean v) { evaluator.setPrettyPrintOutput(v); }

    public static kawadevutil.eval.Eval evaluator = new kawadevutil.eval.Eval();

    @Override
    public void run() {
        Language lang = Language.getDefaultLanguage();
        lang.defineFunction(new GeiserEval("geiser:eval"));
        lang.defineFunction(new GeiserNoValues("geiser:no-values"));
        lang.defineFunction(new GeiserLoadFile("geiser:load-file"));
        lang.defineFunction(new GeiserCompleteSymbol("geiser:completions"));
        lang.defineFunction(new GeiserCompleteModule("geiser:module-completions"));
        lang.defineFunction(new GeiserAutodoc("geiser:autodoc", lang));
        lang.defineFunction(new kawageiser.java.Complete("geiser:complete-java"));
        lang.defineFunction(new ManualEpubUnzipToTmpDir("geiser:manual-epub-unzip-to-tmp-dir"));
    }
}
