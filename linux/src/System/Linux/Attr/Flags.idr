module System.Linux.Attr.Flags

import Data.C.Integer
import Derive.Prelude

%default total
%language ElabReflection


public export
data Flags =
           ||| Always set the attribute.
           XATTR_DEFAULT |
           ||| Set the attribute only if it doesn't already exist.
           XATTR_CREATE |
           ||| Set the attribute only if it already exists.
           XATTR_REPLACE

%runElab derive "Flags" [Show,Eq]

export
fg : Flags -> CInt
fg XATTR_DEFAULT = 0
fg XATTR_CREATE  = 1
fg XATTR_REPLACE = 2
