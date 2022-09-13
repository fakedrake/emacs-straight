(require 'haskell-modules)
(require 'haskell-navigate-imports)
(require 'dash)

(defconst haskell-language-pragmas
  '("CPP"
    "DerivingVia"
    "OverlappingInstances"
    "UndecidableInstances"
    "IncoherentInstances"
    "UndecidableSuperClasses"
    "MonomorphismRestriction"
    "MonoPatBinds"
    "MonoLocalBinds"
    "RelaxedPolyRec"
    "ExtendedDefaultRules"
    "ForeignFunctionInterface"
    "UnliftedFFITypes"
    "InterruptibleFFI"
    "CApiFFI"
    "GHCForeignImportPrim"
    "JavaScriptFFI"
    "ParallelArrays"
    "Arrows"
    "TemplateHaskell"
    "TemplateHaskellQuotes"
    "QuasiQuotes"
    "ImplicitParams"
    "ImplicitPrelude"
    "ScopedTypeVariables"
    "AllowAmbiguousTypes"
    "UnboxedTuples"
    "UnboxedSums"
    "BangPatterns"
    "TypeFamilies"
    "TypeFamilyDependencies"
    "TypeInType"
    "OverloadedStrings"
    "OverloadedLists"
    "NumDecimals"
    "DisambiguateRecordFields"
    "RecordWildCards"
    "RecordPuns"
    "ViewPatterns"
    "GADTs"
    "GADTSyntax"
    "NPlusKPatterns"
    "DoAndIfThenElse"
    "RebindableSyntax"
    "ConstraintKinds"
    "PolyKinds"
    "DataKinds"
    "InstanceSigs"
    "ApplicativeDo"
    "StandaloneDeriving"
    "DeriveDataTypeable"
    "AutoDeriveTypeable"
    "DeriveFunctor"
    "DeriveTraversable"
    "DeriveFoldable"
    "DeriveGeneric"
    "DefaultSignatures"
    "DeriveAnyClass"
    "DeriveLift"
    "DerivingStrategies"
    "TypeSynonymInstances"
    "FlexibleContexts"
    "FlexibleInstances"
    "ConstrainedClassMethods"
    "MultiParamTypeClasses"
    "NullaryTypeClasses"
    "FunctionalDependencies"
    "UnicodeSyntax"
    "ExistentialQuantification"
    "QuantifiedConstraints"
    "ImpredicativeTypes"
    "MagicHash"
    "EmptyDataDecls"
    "KindSignatures"
    "RoleAnnotations"
    "ParallelListComp"
    "TransformListComp"
    "MonadComprehensions"
    "GeneralizedNewtypeDeriving"
    "RecursiveDo"
    "PostfixOperators"
    "TupleSections"
    "PatternGuards"
    "LiberalTypeSynonyms"
    "RankNTypes"
    "ImpredicativeTypes"
    "TypeOperators"
    "ExplicitNamespaces"
    "PackageImports"
    "ExplicitForAll"
    "AlternativeLayoutRule"
    "AlternativeLayoutRuleTransitional"
    "DatatypeContexts"
    "NondecreasingIndentation"
    "RelaxedLayout"
    "TraditionalRecordSyntax"
    "LambdaCase"
    "MultiWayIf"
    "BinaryLiterals"
    "NegativeLiterals"
    "DuplicateRecordFields"
    "OverloadedLabels"
    "EmptyCase"
    "PatternSynonyms"
    "PartialTypeSignatures"
    "NamedWildCards"
    "StaticPointers"
    "TypeApplications"
    "Strict"
    "StrictData"
    "MonadFailDesugaring"))

(defconst haskell-base-modules
  '("Control.Applicative"
    "Control.Arrow"
    "Control.Category"
    "Control.Concurrent"
    "Control.Concurrent.Chan"
    "Control.Concurrent.MVar"
    "Control.Concurrent.QSem"
    "Control.Concurrent.QSemN"
    "Control.Exception"
    "Control.Exception.Base"
    "Control.Monad"
    "Control.Monad.Fail"
    "Control.Monad.Fix"
    "Control.Monad.IO.Class"
    "Control.Monad.Instances"
    "Control.Monad.ST"
    "Control.Monad.ST.Lazy"
    "Control.Monad.ST.Lazy.Safe"
    "Control.Monad.ST.Lazy.Unsafe"
    "Control.Monad.ST.Safe"
    "Control.Monad.ST.Strict"
    "Control.Monad.ST.Unsafe"
    "Control.Monad.Zip"
    "Data.Bifunctor"
    "Data.Bitraversable"
    "Data.Bits"
    "Data.Bool"
    "Data.Char"
    "Data.Coerce"
    "Data.Complex"
    "Data.Data"
    "Data.Dynamic"
    "Data.Either"
    "Data.Eq"
    "Data.Fixed"
    "Data.Foldable"
    "Data.Function"
    "Data.Functor"
    "Data.Functor.Classes"
    "Data.Functor.Compose"
    "Data.Functor.Const"
    "Data.Functor.Contravariant"
    "Data.Functor.Identity"
    "Data.Functor.Product"
    "Data.Functor.Sum"
    "Data.IORef"
    "Data.Int"
    "Data.Ix"
    "Data.Kind"
    "Data.List"
    "Data.List.NonEmpty"
    "Data.Maybe"
    "Data.Monoid"
    "Data.Ord"
    "Data.Proxy"
    "Data.Ratio"
    "Data.STRef"
    "Data.STRef.Lazy"
    "Data.STRef.Strict"
    "Data.Semigroup"
    "Data.String"
    "Data.Traversable"
    "Data.Tuple"
    "Data.Type.Bool"
    "Data.Type.Coercion"
    "Data.Type.Equality"
    "Data.Typeable"
    "Data.Unique"
    "Data.Version"
    "Data.Void"
    "Data.Word"
    "Foreign.C.Error"
    "Foreign.C.String"
    "Foreign.C.Types"
    "Foreign.Concurrent"
    "Foreign.ForeignPtr"
    "Foreign.ForeignPtr.Safe"
    "Foreign.ForeignPtr.Unsafe"
    "Foreign.Marshal"
    "Foreign.Marshal.Alloc"
    "Foreign.Marshal.Array"
    "Foreign.Marshal.Error"
    "Foreign.Marshal.Pool"
    "Foreign.Marshal.Safe"
    "Foreign.Marshal.Unsafe"
    "Foreign.Marshal.Utils"
    "Foreign.Ptr"
    "Foreign.Safe"
    "Foreign.StablePtr"
    "Foreign.Storable"
    "GHC.Base"
    "GHC.ByteOrder"
    "GHC.Char"
    "GHC.Clock"
    "GHC.Conc"
    "GHC.Conc.IO"
    "GHC.Conc.Signal"
    "GHC.Conc.Sync"
    "GHC.Conc.Windows"
    "GHC.ConsoleHandler"
    "GHC.Constants"
    "GHC.Desugar"
    "GHC.Enum"
    "GHC.Environment"
    "GHC.Err"
    "GHC.Event"
    "GHC.Exception"
    "GHC.Exception.Type"
    "GHC.ExecutionStack"
    "GHC.ExecutionStack.Internal"
    "GHC.Exts"
    "GHC.Fingerprint"
    "GHC.Fingerprint.Type"
    "GHC.Float"
    "GHC.Float.ConversionUtils"
    "GHC.Float.RealFracMethods"
    "GHC.Foreign"
    "GHC.ForeignPtr"
    "GHC.GHCi"
    "GHC.Generics"
    "GHC.IO"
    "GHC.IO.Buffer"
    "GHC.IO.BufferedIO"
    "GHC.IO.Device"
    "GHC.IO.Encoding"
    "GHC.IO.Encoding.CodePage"
    "GHC.IO.Encoding.CodePage.API"
    "GHC.IO.Encoding.CodePage.Table"
    "GHC.IO.Encoding.Failure"
    "GHC.IO.Encoding.Iconv"
    "GHC.IO.Encoding.Latin1"
    "GHC.IO.Encoding.Types"
    "GHC.IO.Encoding.UTF16"
    "GHC.IO.Encoding.UTF32"
    "GHC.IO.Encoding.UTF8"
    "GHC.IO.Exception"
    "GHC.IO.FD"
    "GHC.IO.Handle"
    "GHC.IO.Handle.FD"
    "GHC.IO.Handle.Internals"
    "GHC.IO.Handle.Lock"
    "GHC.IO.Handle.Text"
    "GHC.IO.Handle.Types"
    "GHC.IO.IOMode"
    "GHC.IO.Unsafe"
    "GHC.IOArray"
    "GHC.IORef"
    "GHC.Int"
    "GHC.List"
    "GHC.MVar"
    "GHC.Maybe"
    "GHC.Natural"
    "GHC.Num"
    "GHC.OldList"
    "GHC.OverloadedLabels"
    "GHC.Pack"
    "GHC.Profiling"
    "GHC.Ptr"
    "GHC.RTS.Flags"
    "GHC.Read"
    "GHC.Real"
    "GHC.Records"
    "GHC.ResponseFile"
    "GHC.ST"
    "GHC.STRef"
    "GHC.Show"
    "GHC.Stable"
    "GHC.StableName"
    "GHC.Stack"
    "GHC.Stack.CCS"
    "GHC.Stack.Types"
    "GHC.StaticPtr"
    "GHC.Stats"
    "GHC.Storable"
    "GHC.TopHandler"
    "GHC.TypeLits"
    "GHC.TypeNats"
    "GHC.Unicode"
    "GHC.Weak"
    "GHC.Windows"
    "GHC.Word"
    "System.Console.GetOpt"
    "System.Environment"
    "System.Environment.Blank"
    "System.Exit"
    "System.IO"
    "System.IO.Error"
    "System.IO.Unsafe"
    "System.Info"
    "System.Mem"
    "System.Mem.StableName"
    "System.Mem.Weak"
    "System.Posix.Internals"
    "System.Posix.Types"
    "System.Timeout"
    "Text.ParserCombinators.ReadP"
    "Text.ParserCombinators.ReadPrec"
    "Text.Printf"
    "Text.Read"
    "Text.Read.Lex"
    "Text.Show"
    "Text.Show.Functions"
    "Type.Reflection.Unsafe"
    "Unsafe.Coerce"
    "Debug.Trace"))

(defun fd-haskell-add-language-pragma (&optional pragma)
  (interactive (list (completing-read
                      "Insert language pragma: "
                       haskell-language-pragmas)))
  (save-restriction
    (save-excursion
      (goto-char (point-min))
      (insert (format "{-# LANGUAGE %s #-}\n" pragma)))))

(defun haskell-check-module-name-and-highlight ()
  (interactive)
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward flycheck-haskell-module-re nil t)
        (when-let ((decl (match-string-no-properties 1))
                   (decl-pos (match-beginning 1))
                   (inf (haskell-infer-module-name)))
          (unless (string= inf decl)
            (goto-char decl-pos)
            (let ((flycheck-highlighting-mode 'symbols)
                  (line (current-line))
                  (col (current-column)))
              (flycheck-add-overlay
               (flycheck-error-new-at line col 'error
                                      (format "Expected module name %s" inf))))))))))

(defun haskell-cabal-get-key (key)
  (haskell-cabal-subsection-entry-list (haskell-cabal-section) key))

(defun haskell-path-as-module-name (fname &optional src-dir)
  (replace-regexp-in-string
   "\\.hs$" ""
   (if (null src-dir)
       (file-name-base fname)
     (replace-regexp-in-string
      "/" "."
      (substring fname (+ 1 (length src-dir)))))))

(defun haskell-infer-module-name (&optional file-name)
  "Infer the module name. (used in yasnippet)"
  (save-match-data
    (let ((cabal-file (haskell-cabal-find-file))
          (hs-file (or file-name (buffer-file-name))))
      (if (null cabal-file) (file-name-base file-name)
        (with-current-buffer (find-file-noselect cabal-file)
          (let* ((source-dirs
                  (mapcar #'expand-file-name (haskell-cabal-get-key "hs-source-dirs")))
                 (valid-source-dirs
                  (delete-if-not (lambda (x) (string-prefix-p x hs-file)) source-dirs)))
            (haskell-path-as-module-name hs-file (car valid-source-dirs))))))))

(defun haskell-kill-ring-save-declared-module-name ()
  (interactive)
  (when-let ((mn (haskell-declared-module-name)))
    (kill-new mn)
    (message "In kill ring: '%s'" mn)))

(defun haskell-declared-module-name ()
  "Get the declared module name for the current module."
  (save-excursion
    (save-match-data
      (save-restriction
        (widen)
        (goto-char (point-min))
        (re-search-forward "^module[[:space:]]*\\([a-zA-Z.0-9]*\\)")
        (match-string-no-properties 1)))))

(defun fd-haskell--stack-root (dir)
  (locate-dominating-file "stack.yaml"))

(defun fd-haskell--module-list-command (path)
  (let* ((regex "^(module|import)[[:space:]]*((qualified[[:space:]]*)?[A-Za-z.1-9]*([[:space:]]*as[[:space:]]*[A-Za-z.1-9]*)?).*")
         (sed-cmd (format "sed -n -E 's/%s/\\2/p'" regex))
         (find-filter "\\( -name '.#*' -prune \\) -o -name '*.hs'")
         (find-cmd (format "find '%s' %s -exec %s {} \\+" path find-filter sed-cmd)))
      (format "%s | tr -s '[:space:]' | sort | uniq" find-cmd)))

(defun fd-haskell-all-project-modules ()
  (let ((stack-path (locate-dominating-file default-directory "stack.yaml")))
    (split-string
       (shell-command-to-string
        (fd-haskell--module-list-command (expand-file-name stack-path)))
       "\n")))

;;;###autoload
(defun fd-haskell-add-import (module)
  "Look for imports in the project and make an autocomplete
prompt to insert one."
  (interactive (list (completing-read
                      "Module: "
                      (fd-haskell-all-project-modules))))
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-max))
      (haskell-navigate-imports)
      (insert (concat "import " module "\n")))))

(defun haskell-check-module-name ()
  (interactive)
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward flycheck-haskell-module-re nil t)
        (when-let ((decl (match-string-no-properties 1))
                   (decl-pos (match-beginning 1))
                   (inf (haskell-infer-module-name)))
          (unless (string= inf decl)
            (goto-char decl-pos)
            (let ((flycheck-highlighting-mode 'symbols)
                  (line (current-line))
                  (col (current-column)))
              (flycheck-add-overlay
               (flycheck-error-new-at line col 'error
                                      (format "Expected module name %s" inf))))))))))

(provide 'fd-haskell-modules)
