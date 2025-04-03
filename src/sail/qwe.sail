default Order dec

$include <prelude.sail>
$include <generic_equality.sail>

/* Test constant propagation part of monomorphisation involving
   functions.  We should reduce a function application when the
   arguments are suitable values, the function is pure and the result
   is a value.
 */

enum AnEnum = One | Two | Three

val canReduce : AnEnum -> AnEnum
function canReduce (x) = {
  match (x) {
  One => Two,
  x => x
  }
}

val test : AnEnum -> AnEnum
function test (x) = {
  let a = canReduce(x) in
  a
}

val canReduce1 : int -> int
function canReduce1 (x) = {
  match (x) {
  1 => 2,
  x => x
  }
}

val test1 : int -> int
function test1 (x) = {
  let a = canReduce1(x) in
  a
}

val run : int -> unit
function run (x) = {
  let a = test1(x) in ()
}
