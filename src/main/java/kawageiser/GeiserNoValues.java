/*
 * Copyright (C) 2020 spellcard199 <spellcard199@protonmail.com>
 * This is free software;  for terms and warranty disclaimer see ./COPYING.
 */

package kawageiser;

import gnu.mapping.Procedure0;
import gnu.mapping.Values;

public class GeiserNoValues extends Procedure0 {

    GeiserNoValues(String name) {
        super(name);
    }

    @Override
    public Object apply0() throws Throwable {
        gnu.kawa.io.InPort.inDefault().setLineNumber(
                gnu.kawa.io.InPort.inDefault().getLineNumber() - 1);
        // apply0 signature doesn't allow us to return void
        return Values.FromArray.make();
    }
}
