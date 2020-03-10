/*
 * Copyright (C) 2020 spellcard199 <spellcard199@protonmail.com>
 * This is free software;  for terms and warranty disclaimer see ./COPYING.
 */

package kawageiser;

import gnu.lists.IString;
import gnu.lists.LList;
import gnu.mapping.Environment;
import gnu.mapping.Symbol;

import java.util.ArrayList;

public class GeiserCompleteSymbol {

    public static LList getCompletions(IString prefix) {
        return getCompletions(prefix, Environment.user());
    }

    public static LList getCompletions(IString prefix, Environment env) {
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
