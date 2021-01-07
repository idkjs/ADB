open! Core_kernel

module type Storable = sig
  type t

  type encoding

  val encode : t -> (encoding, string) result

  val decode : encoding -> (t, string) result

  val t : t Caqti_type.t
end
