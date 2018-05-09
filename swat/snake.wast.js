var Snake =
(function () {
var TO=TypedObject;
var self = {
module:
new WebAssembly.Module(Snake_bytes),
desc:
{
'Object': {id:1, ids:[1], vtbl:[]},
'Board': {id:2, ids:[2,1], vtbl:[]},
'Tile': {id:3, ids:[3,1], vtbl:[0,0,0,0]},
'Empty': {id:4, ids:[4,3,1], vtbl:[0,1,0,2]},
'Wall': {id:5, ids:[5,3,1], vtbl:[1,0,0,5]},
'Body': {id:6, ids:[6,3,1], vtbl:[1,0,0,3]},
'Food': {id:7, ids:[7,3,1], vtbl:[0,0,1,4]},

},
types:
{
'Object': new TO.StructType({_desc_:TO.Object}),
'Board': new TO.StructType({_desc_:TO.Object,'height':TO.int32,'width':TO.int32,'tiles':TO.Object}),
'Tile': new TO.StructType({_desc_:TO.Object,'element':TO.Object}),
'Empty': new TO.StructType({_desc_:TO.Object,'element':TO.Object}),
'Wall': new TO.StructType({_desc_:TO.Object,'element':TO.Object,'rendering':TO.string}),
'Body': new TO.StructType({_desc_:TO.Object,'element':TO.Object,'younger_y':TO.int32,'younger_x':TO.int32}),
'Food': new TO.StructType({_desc_:TO.Object,'element':TO.Object}),

},
strings:
[
"-",
"|",
"+",
" ",
"#",
"*",
"*** CRASHED ***",
"Paused",
"Can't restart yet",
"Going",

],
buffer:[],
lib:
{
_test: function(x,ys) {
let i=ys.length;
while (i > 0) {
  --i;
  if (ys[i] == x)
    return true;
}
return false;
},
'_upcast_vector_Tile_to_anyref': function (p) { return p },
'_new_vector_Tile': function (n,init) { let a=new Array(n); for (let i=0; i < n; i++) a[i]=init; return a; },
'_vector_length_Tile': function (p) { return p.length },
'_vector_ref_Tile': function (p,i) { return p[i] },
'_vector_set_Tile': function (p,i,v) { p[i] = v },
'_isnull': function (x) { return x === null },
'_null': function () { return null },
'_upcast_class_to_anyref': function (p) { return p },
'_upcast_string_to_anyref': function (p) { return p },
'_anyref_is_string': function (p) { return p instanceof String },
'_downcast_anyref_to_string': 
function (p) {
  if (!(p instanceof String))
    throw new Error('Failed to narrow to string' + p);
  return p;
},
'_resolve_virtual': function(obj,vid) { return obj._desc_.vtbl[vid] },
'_new_Object': function () { return new self.types.Object({_desc_:self.desc.Object}) },
'_upcast_class_to_Object': function (p) { return p },
'_class_is_Object': function (p) { return self.lib._test(1, p._desc_.ids) },
'_anyref_is_Object': function (p) { return p !== null && typeof p._desc_ === 'object' && self.lib._test(1, p._desc_.ids) },
'_downcast_class_to_Object': 
function (p) {
  if (!self.lib._test(1, p._desc_.ids))
    throw new Error('Failed to narrow to Object' + p);
  return p;
},
'_downcast_anyref_to_Object': 
function (p) {
  if (!(p !== null && typeof p._desc_ === 'object' && self.lib._test(1, p._desc_.ids)))
    throw new Error('Failed to narrow to Object' + p);
  return p;
},
'_new_Board': function (height,width,tiles) { return new self.types.Board({_desc_:self.desc.Board,height,width,tiles}) },
'_upcast_class_to_Board': function (p) { return p },
'_class_is_Board': function (p) { return self.lib._test(2, p._desc_.ids) },
'_anyref_is_Board': function (p) { return p !== null && typeof p._desc_ === 'object' && self.lib._test(2, p._desc_.ids) },
'_downcast_class_to_Board': 
function (p) {
  if (!self.lib._test(2, p._desc_.ids))
    throw new Error('Failed to narrow to Board' + p);
  return p;
},
'_downcast_anyref_to_Board': 
function (p) {
  if (!(p !== null && typeof p._desc_ === 'object' && self.lib._test(2, p._desc_.ids)))
    throw new Error('Failed to narrow to Board' + p);
  return p;
},
'_get_Board_height': function (p) { return p.height },
'_set_Board_height': function (p, v) { p.height = v },
'_get_Board_width': function (p) { return p.width },
'_set_Board_width': function (p, v) { p.width = v },
'_get_Board_tiles': function (p) { return p.tiles },
'_set_Board_tiles': function (p, v) { p.tiles = v },
'_new_Tile': function (element) { return new self.types.Tile({_desc_:self.desc.Tile,element}) },
'_upcast_class_to_Tile': function (p) { return p },
'_class_is_Tile': function (p) { return self.lib._test(3, p._desc_.ids) },
'_anyref_is_Tile': function (p) { return p !== null && typeof p._desc_ === 'object' && self.lib._test(3, p._desc_.ids) },
'_downcast_class_to_Tile': 
function (p) {
  if (!self.lib._test(3, p._desc_.ids))
    throw new Error('Failed to narrow to Tile' + p);
  return p;
},
'_downcast_anyref_to_Tile': 
function (p) {
  if (!(p !== null && typeof p._desc_ === 'object' && self.lib._test(3, p._desc_.ids)))
    throw new Error('Failed to narrow to Tile' + p);
  return p;
},
'_get_Tile_element': function (p) { return p.element },
'_set_Tile_element': function (p, v) { p.element = v },
'_new_Empty': function (element) { return new self.types.Empty({_desc_:self.desc.Empty,element}) },
'_upcast_class_to_Empty': function (p) { return p },
'_class_is_Empty': function (p) { return self.lib._test(4, p._desc_.ids) },
'_anyref_is_Empty': function (p) { return p !== null && typeof p._desc_ === 'object' && self.lib._test(4, p._desc_.ids) },
'_downcast_class_to_Empty': 
function (p) {
  if (!self.lib._test(4, p._desc_.ids))
    throw new Error('Failed to narrow to Empty' + p);
  return p;
},
'_downcast_anyref_to_Empty': 
function (p) {
  if (!(p !== null && typeof p._desc_ === 'object' && self.lib._test(4, p._desc_.ids)))
    throw new Error('Failed to narrow to Empty' + p);
  return p;
},
'_get_Empty_element': function (p) { return p.element },
'_set_Empty_element': function (p, v) { p.element = v },
'_new_Wall': function (element,rendering) { return new self.types.Wall({_desc_:self.desc.Wall,element,rendering}) },
'_upcast_class_to_Wall': function (p) { return p },
'_class_is_Wall': function (p) { return self.lib._test(5, p._desc_.ids) },
'_anyref_is_Wall': function (p) { return p !== null && typeof p._desc_ === 'object' && self.lib._test(5, p._desc_.ids) },
'_downcast_class_to_Wall': 
function (p) {
  if (!self.lib._test(5, p._desc_.ids))
    throw new Error('Failed to narrow to Wall' + p);
  return p;
},
'_downcast_anyref_to_Wall': 
function (p) {
  if (!(p !== null && typeof p._desc_ === 'object' && self.lib._test(5, p._desc_.ids)))
    throw new Error('Failed to narrow to Wall' + p);
  return p;
},
'_get_Wall_element': function (p) { return p.element },
'_set_Wall_element': function (p, v) { p.element = v },
'_get_Wall_rendering': function (p) { return p.rendering },
'_set_Wall_rendering': function (p, v) { p.rendering = v },
'_new_Body': function (element,younger_y,younger_x) { return new self.types.Body({_desc_:self.desc.Body,element,younger_y,younger_x}) },
'_upcast_class_to_Body': function (p) { return p },
'_class_is_Body': function (p) { return self.lib._test(6, p._desc_.ids) },
'_anyref_is_Body': function (p) { return p !== null && typeof p._desc_ === 'object' && self.lib._test(6, p._desc_.ids) },
'_downcast_class_to_Body': 
function (p) {
  if (!self.lib._test(6, p._desc_.ids))
    throw new Error('Failed to narrow to Body' + p);
  return p;
},
'_downcast_anyref_to_Body': 
function (p) {
  if (!(p !== null && typeof p._desc_ === 'object' && self.lib._test(6, p._desc_.ids)))
    throw new Error('Failed to narrow to Body' + p);
  return p;
},
'_get_Body_element': function (p) { return p.element },
'_set_Body_element': function (p, v) { p.element = v },
'_get_Body_younger_y': function (p) { return p.younger_y },
'_set_Body_younger_y': function (p, v) { p.younger_y = v },
'_get_Body_younger_x': function (p) { return p.younger_x },
'_set_Body_younger_x': function (p, v) { p.younger_x = v },
'_new_Food': function (element) { return new self.types.Food({_desc_:self.desc.Food,element}) },
'_upcast_class_to_Food': function (p) { return p },
'_class_is_Food': function (p) { return self.lib._test(7, p._desc_.ids) },
'_anyref_is_Food': function (p) { return p !== null && typeof p._desc_ === 'object' && self.lib._test(7, p._desc_.ids) },
'_downcast_class_to_Food': 
function (p) {
  if (!self.lib._test(7, p._desc_.ids))
    throw new Error('Failed to narrow to Food' + p);
  return p;
},
'_downcast_anyref_to_Food': 
function (p) {
  if (!(p !== null && typeof p._desc_ === 'object' && self.lib._test(7, p._desc_.ids)))
    throw new Error('Failed to narrow to Food' + p);
  return p;
},
'_get_Food_element': function (p) { return p.element },
'_set_Food_element': function (p, v) { p.element = v },
'_string_literal': function (n) { return self.strings[n] },
'_string_length': function (p) { return p.length },
'_string_ref': function (p,n) { return p.charCodeAt(n) },
'_string_append': function (p,q) { return p + q },
'_substring': function (p,n,m) { return p.substring(m,n) },
'_upcast_vector_i32_to_anyref': function (p) { return p },
'_new_vector_i32': function (n,init) { let a=new Array(n); for (let i=0; i < n; i++) a[i]=init; return a; },
'_vector_length_i32': function (p) { return p.length },
'_vector_ref_i32': function (p,i) { return p[i] },
'_vector_set_i32': function (p,i,v) { p[i] = v },
'_vector_to_string': function (x) { return String.fromCharCode.apply(null, x) },
'_string_to_vector': function (x) { let a=[]; for(let i=0; i<x.length; i++) a.push(x.charCodeAt(i)); return a },
'_string_compare': 
function (p,q) {
  let a = p.length;
  let b = q.length;
  let l = a < b ? a : b;
  for ( let i=0; i < l; i++ ) {
    let x = p.charCodeAt(i);
    let y = q.charCodeAt(i);
    if (x != y) return x - y;
  }
  return a - b;
},
'_string_10chars': function (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) { self.buffer.push(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10); },
'_make_string': 
function (n,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) {
  self.buffer.push(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10);
  let s = String.fromCharCode.apply(null, self.buffer.slice(0,self.buffer.length-10+n));
  self.buffer.length = 0;
  return s;
},

}};
return self })();
 {
   let board = null;
   let row   = null
   let clock = null;
   let snake = new WebAssembly.Instance(
     Snake.module,
     { lib   : Snake.lib,
       debug : { prs : console.log, pri: console.log },
       Math  : Math,
       hacks : { 'stash-board'   : function (b) { board = b; return board },
                 'unstash-board' : function () { return board }
               },
       dom   : { setText(element, text) { element.textContent = text },
                 newRow() { row = document.getElementById('grid').appendChild(document.createElement('div')) },
                 newTile() { return row.appendChild(document.createElement('span')) },
                 startClock() { interval = setInterval(snake.ontick, 20) },
                 stopClock() { clearInterval(interval) },
                 setState(s) { document.getElementById('state').textContent = s },
                 focus() { /* what to do? */ }
               }
      }).exports;
   window.addEventListener('load', () => snake.setup(24, 80));
   window.addEventListener('keypress', (ev) => snake.onkey(ev.charCode));
 }

