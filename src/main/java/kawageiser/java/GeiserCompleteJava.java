/*
 * Copyright (C) 2020 spellcard199 <spellcard199@protonmail.com>
 * This is free software;  for terms and warranty disclaimer see ./COPYING.
 */

package kawageiser.java;

import gnu.expr.Language;
import gnu.kawa.functions.Format;
import gnu.lists.IString;
import gnu.lists.LList;
import gnu.mapping.Environment;
import gnu.math.IntNum;
import kawadevutil.ast.AstElemWrapper;
import kawadevutil.complete.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

public class GeiserCompleteJava {

    public static String
    completeJava(
            IString codeStr,
            IntNum cursorIndex)
            throws Throwable {
        return completeJava(
                codeStr,
                cursorIndex,
                Language.getDefaultLanguage(),
                Environment.user());
    }

    public static String
    completeJava(
            IString codeStr,
            IntNum cursorIndex,
            Language lang,
            Environment env)
            throws Throwable {

        // Get Data
        Optional<CompletionDataForJava> complDataMaybe =
                kawadevutil.complete.Complete.complete(
                        codeStr.toString(),
                        Integer.valueOf(cursorIndex.toString()),
                        lang,
                        env);

        return complDataMaybe
                .map(GeiserCompleteJava::complDataForJavaToLList)
                .map(x -> Format.format("~S", x).toString())
                .orElse(Format.format("~A", LList.Empty).toString());
    }

    public static LList
    complDataForJavaToLList(
            CompletionDataForJava complData) {
        // Wrap data of interest in Scheme's LList
        LList complDataAsLList = null;
        if (complData.getClass().equals(CompletionDataForJavaField.class)
                || complData.getClass().equals(CompletionDataForJavaMethod.class)) {
            complDataAsLList = toLList((CompletionDataForJavaFOM) complData);
        } else if (complData.getClass().equals(CompletionDataForJavaPackage.class)) {
            complDataAsLList = toLList((CompletionDataForJavaPackage) complData);
        } else {
            throw new Error("[BUG SPOTTED] `complData's class is one not expected: "
                    + complData.getClass().toString());
        }
        return complDataAsLList;
    }

    private static LList
    toLList(
            CompletionDataForJavaFOM complData) {
        String completionsForClass = complData.getForClass().getName();
        // I don't know why it says "unchecked call" when using complData.getRequiredModifiers().stream()
        ArrayList<String> modifiers = new ArrayList<>();
        for (Object modifier : complData.getModifierMask().getRequired()) {
            modifiers.add(modifier.toString());
        }

        ArrayList<LList> res = new ArrayList<>(getCommonData(complData));
        res.addAll(Arrays.asList(
                LList.list2("compl-for-class", completionsForClass),
                LList.list2("modifiers", LList.makeList(modifiers))
        ));
        return LList.makeList(res);

    }

    private static LList
    toLList(
            CompletionDataForJavaPackage complData) {
        ArrayList<LList> res = new ArrayList<>(getCommonData(complData));
        res.addAll(Arrays.asList(
                LList.list2("package-name", complData.getPinfo().getName())
        ));
        return LList.makeList(res);
    }

    private static List<LList>
    getCommonData(
            CompletionDataForJava complData) {
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

    public static Optional<AstElemWrapper>
    getExprTreeMaybe(
            IString codeStr,
            IntNum cursorIndex,
            Language lang,
            Environment env)
            throws IOException {
        Optional<CompletionDataForJava> complDataMaybe =
                kawadevutil.complete.Complete.complete(
                        codeStr.toString(),
                        Integer.valueOf(cursorIndex.toString()),
                        lang,
                        env
                );
        return complDataMaybe.map(complData ->
                complData.getCursorMatcher().getRootWrapper());
    }

    public static Optional<AstElemWrapper>
    getExprTreeMaybe(
            IString codeStr,
            IntNum cursorIndex)
            throws IOException {
        return getExprTreeMaybe(
                codeStr,
                cursorIndex,
                Language.getDefaultLanguage(),
                Environment.user());

    }

    public static String getExprTreeAndFormat(
            IString codeStr,
            IntNum cursorIndex,
            Language lang,
            Environment env)
            throws IOException {
        return getExprTreeMaybe(codeStr, cursorIndex, lang, env)
                .map(complData -> complData.formatElem(true)).get();
    }

    public static String getExprTreeAndFormat(
            IString codeStr,
            IntNum cursorIndex)
            throws IOException {
        return getExprTreeAndFormat(
                codeStr,
                cursorIndex,
                Language.getDefaultLanguage(),
                Environment.user()
        );
    }
}
