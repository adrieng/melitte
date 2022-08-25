type encoding =
  | ASCII
  | UTF8

val encoding_of_string : string -> encoding option

val set_encoding : encoding -> unit

type t

val string : t -> string

val doc : t -> PPrint.document

val pp : Format.formatter -> t -> unit

val lambda : t

val forall : t

val srarrow : t

val drarrow : t

val slarrow : t

val dlarrow : t

val typ : t

val nat : t

val tripleq : t

val checkmark : t

val langle : t

val rangle : t

val omega : t

val times : t

val sigma : t

val unit : t
