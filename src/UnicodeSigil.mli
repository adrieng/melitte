type encoding =
  | ASCII
  | UTF8

val encoding_of_string : string -> encoding option

val set_encoding : encoding -> unit

type t

val string : t -> string

val doc : t -> PPrint.document

val lambda : t

val forall : t

val sarrow : t

val darrow : t

val typ : t

val nat : t
