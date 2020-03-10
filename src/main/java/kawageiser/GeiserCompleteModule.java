/*
 * Copyright (C) 2020 spellcard199 <spellcard199@protonmail.com>
 * This is free software;  for terms and warranty disclaimer see ./COPYING.
 */

package kawageiser;

import gnu.expr.Language;
import gnu.lists.LList;
import gnu.mapping.Environment;
import gnu.mapping.NamedLocation;
import kawadevutil.kawa.GnuMappingLocation;

import java.util.ArrayList;

public class GeiserCompleteModule {

    public static String completeModule(String prefix) {
        return completeModule(prefix, Language.getDefaultLanguage().getEnvironment());
    }

    public static String completeModule(String prefix, Environment env) {
        ArrayList<String> moduleCompletions = getCompletions(prefix, env);
        // Geiser protocol wants modules in the result to be printed
        // between double quotes
        // ("(... ... ...)" "(... ...)")
        // Kawa repl doesn't show returned strings with surrounding
        // quotes, so we have to manually surround completions.
        return gnu.kawa.functions.Format.format(
                "~S", LList.makeList(moduleCompletions)).toString();
    }

    public static ArrayList<String> getCompletions(String prefix, Environment env) {

        ArrayList<String> moduleCompletions = new ArrayList<>();

        // Since this procedure works iterating over locations in the
        // (interaction-environment), if a module does not export any
        // symbol it won't appear in the result.
        // TODO: this is an hack. If it exists, find a way to list
        //       modules directly.
        env.enumerateAllLocations().forEachRemaining(
                (NamedLocation loc) -> {
                    String moduleStrRepr = GnuMappingLocation.baseLocationToModuleName(loc.getBase());
                    if (!moduleCompletions.contains(moduleStrRepr)
                            && !moduleStrRepr.equals("")
                            && moduleStrRepr.startsWith(prefix)) {
                        moduleCompletions.add(moduleStrRepr);
                    }
                }
        );

        return moduleCompletions;
    }
}
