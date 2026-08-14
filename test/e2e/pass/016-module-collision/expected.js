function helper(x) {
  return x + 1;
}
function fromA(x) {
  return helper(x);
}
function helper_2(x) {
  return x * 2;
}
function fromB(x) {
  return helper_2(x);
}
console.log(fromA(10));
console.log(fromB(10));
