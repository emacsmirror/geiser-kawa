/*
 * Copyright (C) 2020 spellcard199 <spellcard199@protonmail.com>
 * This is free software;  for terms and warranty disclaimer see ./COPYING.
 */

package kawageiser;

import gnu.expr.Language;
import gnu.lists.IString;
import gnu.lists.LList;
import gnu.mapping.Environment;
import gnu.mapping.Procedure1or2;
import gnu.mapping.Symbol;

import java.util.ArrayList;

public class GeiserCompleteSymbol extends Procedure1or2 {

    GeiserCompleteSymbol(String name) {
        super(name);
    }

    @Override
    public Object apply1(Object prefix) {
        return apply2(
                prefix,
                Language.getDefaultLanguage().getEnvironment());
    }

    @Override
    public Object apply2(Object prefix, Object module) {

        String prefixStr = null;
        if (prefix instanceof String) {
            prefixStr = (String) prefix;
        } else if (prefix instanceof IString) {
            prefixStr = prefix.toString();
        } else {
            throw new IllegalArgumentException(
                    "prefix arg should be either String or IString");
        }

        Environment env = null;
        if (Environment.class.isAssignableFrom(module.getClass())) {
            // already an Environment
            env = (Environment) module;
        } else if (kawa.lib.lists.isList(module)) {
            env = kawa.lib.scheme.eval.environment$V(((LList) module));
        } else {
            throw new IllegalArgumentException(
                    "module argument should be either a proper list or an Environment.");
        }

        return getCompletions(prefixStr, env);
    }

    private LList getCompletions(String prefix, Environment env) {
        ArrayList<Symbol> resultArrList = new ArrayList<>();
        env.enumerateAllLocations().forEachRemaining(
                loc -> {
                    Symbol sym = loc.getKeySymbol();
                    String symName = sym.getName();
                    if (symName.contains(prefix)) {
                        resultArrList.add(sym);
                    }
                }
        );
        return LList.makeList(resultArrList);
    }
}
