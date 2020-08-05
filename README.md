NoModulePrefices
====

This package provides a GHC plugin which transforms module names so that you don't have to write top-level module prefices (e.g. Control, Data, System) yourselves.

To enable the plugin, pass the following flags to GHC:

```
-fplugin=NoModulePrefices -fplugin-opt=NoModulePrefices:https://raw.githubusercontent.com/fumieval/NoModulePrefices/c07381d49c1c8d617850c5ef0352ea5bcb9a8422/default.yaml@c8982f3bc53c6a4b5bc76d81f151a8379135de657e43c3c04d191eacdcc83dca
```

The mapping from short names to canonical names can be configured via a YAML document. The plugin fetches a YAML document via HTTP, validates its SHA256 and store it to the XDG cache directory.

Known issues
----

Non-existent module imports in a source file seems to cause unnecessary recompilation.

```
[1 of 6] Compiling Lib.Component    ( src\Lib\Component.hs, C:\Users\fumieval\hs\dcg\dist-newstyle\build\x86_64-windows\ghc-8.10.1\dcg-yurudora-0\x\dcg-yurudora\build\dcg-yurudora\dcg-yurudora-tmp\Lib\Component.o ) [Functor.Identity changed]
[3 of 6] Compiling Model            ( src\Model.hs, C:\Users\fumieval\hs\dcg\dist-newstyle\build\x86_64-windows\ghc-8.10.1\dcg-yurudora-0\x\dcg-yurudora\build\dcg-yurudora\dcg-yurudora-tmp\Model.o ) [Monad.Random changed]
```
