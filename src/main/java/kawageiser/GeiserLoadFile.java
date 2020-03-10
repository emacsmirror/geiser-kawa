/*
 * Copyright (C) 2020 spellcard199 <spellcard199@protonmail.com>
 * This is free software;  for terms and warranty disclaimer see ./COPYING.
 */

package kawageiser;

import gnu.expr.Language;
import gnu.lists.LList;
import kawa.standard.load;

public class GeiserLoadFile {

    public static String loadFile(String filepath) {
        return GeiserEval.evalForm(
                Language.getDefaultLanguage().getEnvironment(),
                LList.list2(load.load, filepath));
    }
}
