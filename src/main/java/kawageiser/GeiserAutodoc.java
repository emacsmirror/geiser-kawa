/*
 * Copyright (C) 2020 spellcard199 <spellcard199@protonmail.com>
 * This is free software;  for terms and warranty disclaimer see ./COPYING.
 */

package kawageiser;

import gnu.expr.CompiledProc;
import gnu.expr.Language;
import gnu.kawa.functions.Format;
import gnu.lists.LList;
import gnu.mapping.*;
import kawadevutil.data.ParamData;
import kawadevutil.data.ProcDataGeneric;
import kawadevutil.data.ProcDataNonGeneric;
import kawadevutil.kawa.GnuMappingLocation;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class GeiserAutodoc extends Procedure1or2 {

    public static boolean showTypes = true;
    Language lang;

    GeiserAutodoc(String name, Language lang) {
        super(name);
        this.lang = lang;
    }

    // TODO: find the "right" way to get modules for symbols.
    // TODO: support for procedures defined in java, like `append'
    // TODO: support for macros (possible?)
    // TODO: support for getting parameter names for java instance
    //       methods getMethods bytecode with ClassType (not so simple)
    // TODO: support names with special characters, like |a[)|
    //       Maybe we can:
    //       1. keep a list of special chars
    //       2. when `id' contains one: surround with || (e.g. |id|)
    // TODO: consider multiple ids:
    //  - Examples: to get more add (message (format "(geiser:%s %s)" proc form))
    //              in the t clause of the geiser-kawa--geiser-procedure:
    //   - (display [cursor] -> (geiser:autodoc ’(display))
    //   - (display (symbol? (string->symbol [cursor] -> (geiser:autodoc ’(string->symbol symbol? display))


    // At the moment arguments are enclosed in double quotes. The
    // reason is that geiser's output is `read' by elisp, but java types
    // may contain characters that are not valid in elisp symbols
    // (e.g. java arrays contain square brackets). So instead of symbols
    // we use strings. When type annotations are disabled using
    // (geiser:set-autodoc-show-types #f) parameter names displayed as
    // symbols (without double quotes around them).

    // List<Object> idsArr = List.getLocalVarsAttr(ids.toArray());
    // idsArr.stream().map( (Object symId, Object env) -> {
    // return (new SymToAutodoc()).apply2(symId, env);
    // })

    // @Override
    public Object apply1(Object ids) {
        return apply2(ids, Language.getDefaultLanguage().getEnvironment());
    }

    // @Override
    public Object apply2(Object ids, Object env) {

        if (!LList.class.isAssignableFrom(ids.getClass())) {
            throw new IllegalArgumentException(String.format(
                    "GeiserAutodoc's 1st arg should be a gnu.lists.LList"));
        }
        if (!Environment.class.isAssignableFrom(env.getClass())) {
            throw new IllegalArgumentException(String.format(
                    "GeiserAutodoc's 2nd arg should be a gnu.mapping.Environment"));
        }
        try {
            ArrayList<Object> autodocList = new ArrayList<>();
            for (Object symId : (LList) ids) {
                AutodocDataForSymId autodocDataForSymId =
                        new AutodocDataForSymId((Symbol) symId, (Environment) env, this.lang);
                autodocList.add(autodocDataForSymId.toLList());
            }
            String formattedAutodoc =
                    Format
                            .format("~S", LList.makeList(autodocList))
                            .toString();
            return formattedAutodoc;
        } catch (Throwable throwable) {
            throwable.printStackTrace();
            return throwable;
        }
    }

    public static class OperatorArgListData {
        ProcDataGeneric procDataGeneric;

        public OperatorArgListData(ProcDataGeneric procDataGeneric) {
            this.procDataGeneric = procDataGeneric;
        }

        private static String
        formatParam(ParamData param, String formatting) {
            // This method is just to reduce boilerplate in the other `formatParam'
            String paramName = param.getName();
            String formattedParamType =
                    Format
                            .format("::~a",
                                    param.getType().getReflectClass().getName())
                            .toString();
            return Format.format(
                    formatting,
                    paramName,
                    formattedParamType).toString();
        }

        public static String
        formatParam(ParamData param,
                    boolean isOptionalParam) {
            if (isOptionalParam && showTypes) {
                return formatParam(param, "(~a~a)");
            } else if (isOptionalParam && !showTypes) {
                return formatParam(param, "(~a)");
            } else if (!isOptionalParam && showTypes) {
                return formatParam(param, "~a~a");
            } else if (!isOptionalParam && !showTypes) {
                return formatParam(param, "~a");
            } else {
                throw new Error("No worries, can't happen (2 booleans == 4 possibilities)." +
                        "Just silencing the \"Missing return statement\" error.");
            }
        }

        public LList paramListToFormattedParamLList(List<ParamData> params, boolean areOptionalParams) {
            List<String> formattedParamList = new ArrayList<>();
            for (ParamData req : params) {
                String s = formatParam(req, areOptionalParams);
                formattedParamList.add(s);
            }
            return LList.makeList(formattedParamList);
        }

        public LList toLList() {
            ArrayList<Object> genericProcArgList = new ArrayList<>();

            for (ProcDataNonGeneric pd : this.procDataGeneric.getProcDataNonGenericList()) {
                ArrayList<Object> nonGenericProcArgList = new ArrayList<>();
                ArrayList<Object> requiredParamList = new ArrayList<>();
                ArrayList<Object> optionalParamList = new ArrayList<>();

                List<ParamData> requiredParams = pd.getRequiredParams();

                if (!requiredParams.isEmpty()) {
                    LList requiredParamLList = paramListToFormattedParamLList(requiredParams, false);
                    // argList.add(LList.list2("required", requiredParamLList));
                    requiredParamList.add("required");
                    requiredParamList.addAll(requiredParamLList);
                }

                List<ParamData> optionalParams = pd.getOptionalParams();
                Optional<ParamData> restParamMaybe = pd.getRestParam();
                if (optionalParams.size() > 0 || restParamMaybe.isPresent()) {
                    LList optionalOrRestParamLList =
                            optionalParams.size() > 0
                                    ? paramListToFormattedParamLList(optionalParams, true)
                                    : LList.makeList(java.util.Collections.emptyList());
                    if (restParamMaybe.isPresent()) {
                        optionalOrRestParamLList.add(
                                Format.format(
                                        "(... ~a...)",
                                        formatParam(restParamMaybe.get(), false)
                                )
                        );
                    }
                    optionalParamList.add("optional");
                    optionalParamList.addAll(optionalOrRestParamLList);
                }

                if (!requiredParamList.isEmpty()) {
                    nonGenericProcArgList.add(LList.makeList(requiredParamList));
                }
                if (!optionalParamList.isEmpty()) {
                    nonGenericProcArgList.add(LList.makeList(optionalParamList));
                }
                genericProcArgList.add(LList.makeList(nonGenericProcArgList));
            }

            return LList.makeList(genericProcArgList);
        }
    }

    public static class AutodocDataForSymId {
        private boolean symExists;
        private Symbol symId;
        private Object operator;
        private Environment environment;
        private Optional<OperatorArgListData> operatorArgListMaybe;
        // TODO: fix type, write way to get it
        private Object module;


        public AutodocDataForSymId(Symbol symId, Environment env, Language lang) {
            this.symId = symId;
            this.environment = env;

            Optional<OperatorArgListData> operatorArgListMaybe = Optional.empty();
            Object operator = null;
            boolean symExists = false;
            try {
                operator = lang.eval(symId.toString());
                symExists = true;  // If it didn't exist env.get(symId) would have raised UnboundLocationException
                if (!Procedure.class.isAssignableFrom(operator.getClass())) {
                    // Not a procedure
                    // TODO : is it possible to implement autodoc for macros?
                    //        If not: write a comment why.
                    operatorArgListMaybe = Optional.empty();
                } else {
                    ProcDataGeneric procDataGeneric = ProcDataGeneric.makeForProcedure((Procedure) operator);
                    operatorArgListMaybe = Optional.of(new OperatorArgListData(procDataGeneric));
                }
            } catch (Throwable throwable) {
                throwable.printStackTrace();
            }

            this.operatorArgListMaybe = operatorArgListMaybe;
            this.operator = operator;
            this.symExists = symExists;
        }

        public LList toLList() {

            ArrayList<Object> operatorArgListAsList = new ArrayList<>();
            operatorArgListAsList.add("args");
            if (operatorArgListMaybe.isPresent()) {
                operatorArgListAsList.addAll(operatorArgListMaybe.get().toLList());
            } else {
                operatorArgListAsList.add(false);
            }
            LList operatorArgListAsLList = LList.makeList(operatorArgListAsList);

            // TODO: write a procedure that gets the module getMethods
            //       which a symbol comes getMethods using "the right way" (is there one?)
            // TODO: When we find the correct way to do it, refactor moduleValue inside
            //       ProcDataNonGeneric or a generic wrapper for Procedure data
            LList moduleValue = null;
            if (operator.getClass() == CompiledProc.class) {
                CompiledProc compProc = (CompiledProc) operator;
                moduleValue = LList.makeList(
                        java.util.Arrays
                                .asList(compProc
                                        .getModuleClass()
                                        .getName()
                                        .split("\\.")));
            } else {
                try {
                    // If it's not a CompiledProc it does not have a
                    // `getModule' method: fallback to trying to figure
                    // out getMethods GnuMappingLocation in Environment.
                    // TODO: generalize to arbitrary environment
                    moduleValue = (LList) kawa.lib.ports.read(
                            new gnu.kawa.io.CharArrayInPort(
                                    GnuMappingLocation.baseLocationToModuleName(
                                            environment.lookup(symId).getBase()
                                    )
                            )
                    );
                } catch (NullPointerException e) {
                    // If it is not even a sym in the environment, give up.
                    // TODO: should we consider all java classes as modules?
                    moduleValue = LList.makeList(new ArrayList());
                }
            }

            // If you don't convert your symbol to String it may not match with
            // the string seen from the emacs side and when that happens geiser
            // does not considers that a valid autodoc response.
            // Example: as a Symbol, java.lang.String:format is displayed by
            // kawa as:
            //   java.lang.String{$unknown$}:format
            // which does not match:
            //   java.lang.String:format
            // so geiser ignores it.
            String symIdAsStr = symId.toString();
            LList returnMe;
            if (moduleValue.size() > 0) {
                ArrayList<Object> moduleList = new ArrayList<>();
                moduleList.add("module");
                for (Object m : moduleValue) {
                    moduleList.add(Symbol.valueOf(m.toString()));
                }
                returnMe = LList.list3(
                        symIdAsStr,
                        operatorArgListAsLList,
                        LList.makeList(moduleList)
                );
            } else {
                returnMe = LList.list2(
                        symIdAsStr,
                        operatorArgListAsLList
                );
            }

            return returnMe;
        }
    }
}

