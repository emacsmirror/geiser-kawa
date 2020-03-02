/*
 * Copyright (C) 2020 spellcard199 <spellcard199@protonmail.com>
 * This is free software;  for terms and warranty disclaimer see ./COPYING.
 */

package kawageiser;

import gnu.expr.Language;
import gnu.lists.IString;
import gnu.lists.LList;
import gnu.mapping.Procedure1;
import kawa.standard.load;

public class GeiserLoadFile extends Procedure1 {

    GeiserLoadFile(String name) {
        super(name);
    }

    @Override
    public Object apply1(Object o) throws Throwable {
        String filepath;
        if (o instanceof String) {
            filepath = (String) o;
        } else if (o instanceof IString) {
            filepath = ((IString) o).toString();
        } else {
            throw new IllegalArgumentException(
                    "geiser:load should take a String or an IString as argument");
        }
        return load(filepath);
    }

    public Object load(String filepath) {
        return GeiserEval.eval(
                Language.getDefaultLanguage().getEnvironment(),
                LList.list2(load.load, filepath));
    }
}
