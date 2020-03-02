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
     *  Actual evaluation happens in kawadevtools.eval.Eval.
     *  Here we are just sending arguments and converting our own
     *  types into the geiser protocol.
     */

    GeiserEval(String procName) {
        super(procName);
    }

    @Override
    public String
    apply2(Object module, Object codeStr) {
        // Today (2019-12-9) Kawa has still that issue when
        // quoting (this) followed by a double colon. So, to avoid
        // it altogether, geiser:eval default is to accept Strings
        // instead of sexprs.
        // You can still evaluate expressions instead of strings using
        // the other GeiserEval:eval method explicitly.
        String code;
        if (codeStr instanceof IString) {
            code = ((IString) codeStr).toString();
        } else if (codeStr instanceof String) {
            code = (String) codeStr;
        } else {
            throw new IllegalArgumentException(
                    "`codeStr' arg should be either a String or an IString");
        }
        return eval((Environment) module, code);
    }

    public static String
    eval(Environment module, String codeStr) {
        EvalResultAndOutput resOutErr = Geiser.evaluator.evalCatchingOutErr(module, codeStr);
        return formatGeiserProtocol(evaluationDataToGeiserProtocol(resOutErr));
    }

    public static String
    eval(Environment module, Object sexpr) {
        EvalResultAndOutput resOutErr = Geiser.evaluator.evalCatchingOutErr(module, sexpr);
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
                ? Geiser.evaluator.formatStackTrace(evalRes.getThrowed())
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
