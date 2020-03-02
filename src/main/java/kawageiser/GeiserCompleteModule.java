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
import kawadevutil.kawa.GnuMappingLocation;

import java.util.ArrayList;


public class GeiserCompleteModule extends Procedure1or2 {

    GeiserCompleteModule(String name) {
        super(name);
    }

    @Override
    public Object apply1(Object prefix) throws Throwable {
        return apply2(
                prefix,
                Language.getDefaultLanguage().getEnvironment());
    }

    @Override
    public Object apply2(Object prefix, Object env) throws Throwable {

        String prefixStr = null;
        if (prefix instanceof String) {
            prefixStr = (String) prefix;
        } else if (prefix instanceof IString) {
            prefixStr = prefix.toString();
        } else {
            throw new IllegalArgumentException(
                    "`prefix' arg should be either a String or an IString");
        }

        Environment castedEnv;
        if (Environment.class.isAssignableFrom(env.getClass())) {
            castedEnv = (Environment) env;
        } else {
            throw new IllegalArgumentException(
                    "`env' arg should be an gnu.mapping.Environment");
        }

        ArrayList<String> moduleCompletions = getCompletions(prefixStr, castedEnv);
        // Geiser protocol wants modules in the result to be printed
        // between double quotes
        // ("(... ... ...)" "(... ...)")
        // Kawa repl doesn't show returned strings with surrounding
        // quotes, so we have to manually surround completions.
        return gnu.kawa.functions.Format.format("~S", LList.makeList(moduleCompletions));
    }

    private ArrayList<String> getCompletions(String prefix, Environment env) {

        ArrayList<String> moduleCompletions = new ArrayList<>();

        // Since this procedure works iterating over locations in the
        // (interaction-environment), if a module does not export any
        // symbol it won't appear in the result.
        // TODO: this is an hack. If it exists, find a way to list
        //       modules directly.
        env.enumerateAllLocations().forEachRemaining(
                loc ->
                {
                    String moduleStrRepr = GnuMappingLocation
                            .baseLocationToModuleName(loc.getBase());
                    if ((!moduleCompletions.contains(moduleStrRepr))
                            && (!(moduleStrRepr.equals("")))
                            && (moduleStrRepr.startsWith(prefix))
                    ) {
                        moduleCompletions.add(moduleStrRepr);
                    }
                }
        );

        return moduleCompletions;
    }
}
