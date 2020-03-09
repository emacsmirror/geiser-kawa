/*
 * Copyright (C) 2020 spellcard199 <spellcard199@protonmail.com>
 * This is free software;  for terms and warranty disclaimer see ./COPYING.
 */

package kawageiser.java;

import gnu.expr.Language;
import gnu.lists.IString;
import gnu.lists.LList;
import gnu.mapping.Environment;
import gnu.mapping.Procedure4;
import gnu.math.IntNum;
import kawadevutil.complete.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

public class GeiserCompleteJava extends Procedure4 {

    public static boolean showTypes = true;

    public GeiserCompleteJava(String name) {
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
                    "`cursorIndex` must be either Integer or IntNum: "
                            + cursorIndex.getClass().toString());
        }

        // Get Data
        Optional<CompletionDataForJava> complDataMaybe = kawadevutil.complete.Complete.complete(
                codeStrChecked, cursorIndexChecked, (Language) lang, (Environment) env, (String name) -> true);

        // Wrap data of interest in Scheme's LList
        if (!complDataMaybe.isPresent()) {
            return LList.Empty;
        } else {
            CompletionDataForJava complData = complDataMaybe.get();
            LList res = null;
            if (complData.getClass().equals(CompletionDataForJavaField.class)
                    || complData.getClass().equals(CompletionDataForJavaMethod.class)) {
                res = toLList((CompletionDataForJavaFOM) complData);
            } else if (complData.getClass().equals(CompletionDataForJavaPackage.class)) {
                res = toLList((CompletionDataForJavaPackage) complData);
            } else {
                throw new Error("[BUG SPOTTED] `complData's class is one not expected: "
                        + complData.getClass().toString());
            }
            return gnu.kawa.functions.Format.format("~S", res);
        }
    }

    private static LList toLList(CompletionDataForJavaFOM complData) {
        String completionsForClass = complData.getForClass().getName();
        // I don't know why it says "unchecked call" when using complData.getRequiredModifiers().stream()
        ArrayList<String> modifiers = new ArrayList<>();
        for (Object modifier : complData.getRequiredModifiers()) {
            modifiers.add(modifier.toString());
        }

        ArrayList<LList> res = new ArrayList<>(getCommonData(complData));
        res.addAll(Arrays.asList(
                LList.list2("compl-for-class", completionsForClass),
                LList.list2("modifiers", LList.makeList(modifiers))
        ));
        return LList.makeList(res);

    }

    private static LList toLList(CompletionDataForJavaPackage complData) {
        ArrayList<LList> res = new ArrayList<>(getCommonData(complData));
        res.addAll(Arrays.asList(
                LList.list2("package-name", complData.getPinfo().getName())
        ));
        return LList.makeList(res);
    }

    private static List<LList> getCommonData(CompletionDataForJava complData) {
        CompletionDataForJava.FieldOrMethodOrPackage fieldOrMethod = complData.getFieldOrMethodOrPackage();
        List<String> names = (List<String>) complData.getNames().stream().distinct().collect(Collectors.toList());
        String beforeCursor = complData.getCursorMatcher().getCursorMatch().getBeforeCursor();
        String afterCursor = complData.getCursorMatcher().getCursorMatch().getAfterCursor();

        java.util.List<LList> res = Arrays.asList(
                LList.list2("field-or-method-or-package", fieldOrMethod.toString()),
                LList.list2("names", LList.makeList(names)),
                LList.list2("before-cursor", beforeCursor),
                LList.list2("after-cursor", afterCursor)
        );
        return res;
    }

}
