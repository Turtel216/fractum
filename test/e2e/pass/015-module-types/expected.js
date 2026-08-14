const p = { x: 1, y: 2 };
console.log(p.x + p.y);
const s = { _tag: "Circle", _0: 5 };
const area = (() => {
  const _m = s;
  if (_m._tag === "Circle") {
    const r = _m._0;
    return r * r * 3;
  }
  if (_m._tag === "Square") {
    const side = _m._0;
    return side * side;
  }
})();
console.log(area);
