/*
 * Copyright (C) 2020 spellcard199 <spellcard199@protonmail.com>
 * This is free software;  for terms and warranty disclaimer see ./COPYING.
 */

package kawageiser;

import gnu.kawa.functions.Format;
import gnu.lists.IString;
import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.mapping.Environment;
import gnu.mapping.Procedure2;
import gnu.mapping.Symbol;
import kawadevutil.eval.EvalResult;
import kawadevutil.eval.EvalResultAndOutput;
import kawadevutil.redirect.RedirectedOutErr;

public class GeiserEval extends Procedure2 {
    /*
     *  Actual evaluation happens in kawadevutil.eval.Eval.
     *  Here we are just sending arguments and converting our own
     *  types into the geiser protocol.
     */
    public static kawadevutil.eval.Eval evaluator = new kawadevutil.eval.Eval();

    GeiserEval(String procName) {
        super(procName);
    }

    @Override
    public String
    apply2(Object module, Object codeStr) {
        // The reason this method takes a string instead of a quoted sexpr has been solved
        // on 2019-12-19 in Kawa's master branch:
        // https://gitlab.com/kashell/Kawa/-/commit/537e135c0101194702ebee53faf92b98a4ea8c6b
        // When there is going to be a reason to change the current behavior of GeiserEval,
        // geiser-kawa.el should also be changed to something like this:
        // (case proc
        //   ((eval compile)
        //    (let* ((send-this
        //            (format
        //             "(geiser:eval (interaction-environment) '%s)"
        //             cadr args))))
        //      (print send-this)
        //      send-this))
        //

        String code;
        if (codeStr instanceof IString) {
            code = ((IString) codeStr).toString();
        } else if (codeStr instanceof String) {
            code = (String) codeStr;
        } else {
            throw new IllegalArgumentException(
                    "`codeStr' arg should be either a String or an IString");
        }
        return evalStr((Environment) module, code);
    }

    public static String
    evalStr(Environment module, String codeStr) {
        EvalResultAndOutput resOutErr = evaluator.evalCatchingOutErr(module, codeStr);
        return formatGeiserProtocol(evaluationDataToGeiserProtocol(resOutErr));
    }

    public static String
    evalForm(Environment module, Object sexpr) {
        EvalResultAndOutput resOutErr = evaluator.evalCatchingOutErr(module, sexpr);
        return formatGeiserProtocol(evaluationDataToGeiserProtocol(resOutErr));
    }

    public static LList
    evaluationDataToGeiserProtocol(EvalResultAndOutput resOutErr) {
        EvalResult evalRes = resOutErr.getResultOfSupplier();
        RedirectedOutErr outErr = resOutErr.getOutErr();

        // result
        String geiserResult = evalRes.isSuccess()
                ? evalRes.getResultAsString(Geiser.isPrettyPrintResult())
                : "";

        // output
        String messages = (evalRes.getMessages() != null)
                ? evalRes.getMessages().toString(100000)
                : "";
        messages = (messages != null) ? messages : "";
        String stackTrace = (evalRes.getThrowed() != null)
                ? evaluator.formatStackTrace(evalRes.getThrowed())
                : "";
        String output = outErr.getOutAndErrInPrintOrder();
        // If we wanted, we could include stack traces directly in
        // the output using Eval.setPrintStackTrace(): that would
        // display stack traces of exceptions after the output
        // produced by the code instead of before.
        // Since the Kawa repl prints in order: messages, stacktrace,
        // output, we are doing the same.
        String geiserOutput = messages + stackTrace + output;

        return evaluationDataToGeiserProtocol(
                evalRes.isSuccess(), geiserResult, geiserOutput);
    }

    private static LList
    evaluationDataToGeiserProtocol
            (boolean isSuccess, String geiserResult, String geiserOutput) {
        LList geiserResOrErr =
                isSuccess
                        ? LList.list2(Symbol.valueOf("result"), geiserResult)
                        : LList.list2(Symbol.valueOf("error"), "");
        Pair geiserOut = Pair.make(Symbol.valueOf("output"), geiserOutput);
        return LList.list2(geiserResOrErr, geiserOut);
    }

    public static String formatGeiserProtocol(LList geiserAnswer) {
        return (String) Format.format("~S", geiserAnswer);
    }

}
