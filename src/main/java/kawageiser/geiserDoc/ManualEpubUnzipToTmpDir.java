/*
 * Copyright (C) 2020 spellcard199 <spellcard199@protonmail.com>           
 * This is free software;  for terms and warranty disclaimer see ./COPYING.
 */

package kawageiser.geiserDoc;

import gnu.lists.IString;
import gnu.mapping.Procedure1;

import java.io.File;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;

public class ManualEpubUnzipToTmpDir extends Procedure1 {

    public ManualEpubUnzipToTmpDir(String name) {
        super(name);
    }

    @Override
    public Object apply1(Object kawaEpubManualPath) throws Throwable {
        if (! (kawaEpubManualPath.getClass() == String.class ||
                kawaEpubManualPath.getClass() == IString.class)) {
            throw new IllegalArgumentException(
                    "`kawaEpubManualPath' arg must be either String or IString");
        }
        String systemTmpDir = System.getProperty("java.io.tmpdir");
        String manualUnzippedTmpDir = String.join(
                File.separator,
                systemTmpDir,
                "geiser-kawa",
                "manual-epub-unzipped");

        File zipArchiveFile = new File(kawaEpubManualPath.toString());
        Path destDirPath = new File(manualUnzippedTmpDir).toPath();

        kawadevutil.util.ZipExtractor.unzip(zipArchiveFile, destDirPath);
        return gnu.kawa.functions.Format.format("~S", manualUnzippedTmpDir);
    }
}
