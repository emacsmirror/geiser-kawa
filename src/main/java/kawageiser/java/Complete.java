/*
 * Copyright (C) 2020 spellcard199 <spellcard199@protonmail.com>
 * This is free software;  for terms and warranty disclaimer see ./COPYING.
 */

package kawageiser.java;

import gnu.expr.Language;
import gnu.lists.IString;
import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.mapping.Environment;
import gnu.mapping.Procedure1or2;
import gnu.mapping.Procedure3;
import gnu.mapping.Procedure4;
import gnu.math.IntNum;
import kawadevutil.complete.*;

import java.io.File;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Complete extends Procedure4 {

    public static boolean showTypes = true;

    public Complete(String name) {
        super(name);
    }

    @Override
    public Object
    apply4(Object codeStr, Object cursorIndex, Object lang, Object env) throws Throwable {

        String codeStrChecked = null;
        if (codeStr.getClass().equals(IString.class) || codeStr.getClass().equals(String.class)) {
            codeStrChecked = codeStr.toString();
        } else {
            throw new IllegalArgumentException(
                    "`codeStr` must be either String or IString: " + codeStr.getClass().toString());
        }

        Integer cursorIndexChecked = null;
        if (cursorIndex.getClass().equals(Integer.class)) {
            cursorIndexChecked = (Integer) cursorIndex;
        } else if (cursorIndex.getClass().equals(IntNum.class)) {
            cursorIndexChecked = ((IntNum) cursorIndex).intValue();
        } else {
            throw new IllegalArgumentException(
                    "`cursorIndex` must be either Integer or IntNum: " + cursorIndex.getClass().toString());
        }

        // Get Data
        Optional<CompletionDataForJava> complDataMaybe = kawadevutil.complete.Complete.complete(
                codeStrChecked, cursorIndexChecked, (Language) lang, (Environment) env, (String name) -> true);

        // Wrap data of interest in Scheme's LList
        if (!complDataMaybe.isPresent()) {
            return LList.Empty;
        } else {
            CompletionDataForJava complData = complDataMaybe.get();
            if (complData.getClass().equals(CompletionDataForJavaField.class)) {
                CompletionDataForJavaField complDataForField = (CompletionDataForJavaField) complData;
            } else if (complData.getClass().equals(CompletionDataForJavaMethod.class)) {
                CompletionDataForJavaMethod complDataForMethod = (CompletionDataForJavaMethod) complData;
            } else {
                throw new Error("Bug spotted.");
            }

            String completionsForClass = complData.getForClass().getName();
            CompletionDataForJava.FieldOrMethod fieldOrMethod = complData.getFieldOrMethod();
            List<String> names = (List<String>) complData.getNames().stream().distinct().collect(Collectors.toList());
            String beforeCursor = complData.getCursorMatcher().getCursorMatch().getBeforeCursor();
            String afterCursor = complData.getCursorMatcher().getCursorMatch().getAfterCursor();
            // I don't know why it says "unchecked call" when using complData.getRequiredModifiers().stream()
            ArrayList<String> modifiers = new ArrayList<>();
            for (Object modifier : complData.getRequiredModifiers()) {
                modifiers.add(modifier.toString());
            }

            java.util.List<LList> res = Arrays.asList(
                    LList.list2("compl-for-class", completionsForClass),
                    LList.list2("modifiers", LList.makeList(modifiers)),
                    LList.list2("field-or-method", fieldOrMethod.toString()),
                    LList.list2("completions", LList.makeList(names)),
                    LList.list2("before-cursor", beforeCursor),
                    LList.list2("after-cursor", afterCursor)
            );
            LList resLList = LList.makeList(res);
            return gnu.kawa.functions.Format.format("~S", resLList);
        }
    }

}
