/*
 * Copyright (C) 2020 spellcard199 <spellcard199@protonmail.com>
 * This is free software;  for terms and warranty disclaimer see ./COPYING.
 */

package kawageiser.exprtree;

import gnu.expr.Language;
import gnu.mapping.Environment;
import kawadevutil.ast.AstElemWrapper;

import java.io.IOException;

public class ExprTree {

    public static AstElemWrapper
    getExprTree(String codeStr, Language lang, Environment env)
            throws IOException {
        return new kawadevutil.ast.AstElemWrapper(codeStr, lang, env);
    }

    public static AstElemWrapper
    getExprTree(String codeStr) throws IOException {
        return getExprTree(codeStr, Language.getDefaultLanguage(), Environment.user());
    }

    public static String
    getExprTreeFormatted(String codeStr, Language lang, Environment env)
            throws IOException {
        return getExprTree(codeStr, lang, env).formatElem(true);
    }

    public static String
    getExprTreeFormatted(String codeStr) throws IOException {
        return getExprTree(codeStr).formatElem(true);
    }
}
