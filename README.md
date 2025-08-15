# OPL

Oliver's Programming Language is based on scopes. Value scopes (essentially lambdas) and type scopes (structs, etc.).
Examples so far found below:

```
// Fundamental types
u8, u16, u32, u64,
i8, i16, i32, i64,
f32, f64, bool, void

// Names can be either a value (fundamental type), or a value scope (lambda)
a: f32 = 5; // value
b := () {}; // empty lambda

// Anonymous lambda
() {};

// Calling anonymous lambda
() {} ();

// Naming anonymous lambda
my_name := () {};

// Calling named lambda
my_name(); // equivalent to () {} ();

// 0 parameter lambdas can have their brackets omitted
my_name := {};
my_name;

// Called lambdas can have their local names accesed
num: f32 = { x: f32 = 5; }().x;

// Named lambdas therefore support this
length := (x: f32, y: f32) {
    result := std.sqrt(x * x, y * y);
};
len := length(2, 5).result;

// So far only value scopes (lambdas) have been discussed
// Type scopes are used instead of fundamental types or the "value_scope" type
Point: {
    x: f32 = 0;
    y: f32 = 0;
};
p: Point = {}; // Equivalent to p: {x: f32 = 0; y: f32 = 0;} = {};
// = {} is required otherwise p is an alias of the "Point" type scope instead of a value scope name
// the empty value scope makes p use Point's default values

// Type scopes act like an array of different types
// Their local names are available in their assigned value scope
p: Point = {
    x = 55; // x and y are available from the Point type scope
    y = x * 2;
};

// Type scopes cannot have their local names accessed like value scopes allow
Point.x; // error!
p.x = 5; // OK

// Only fields which come from a type scope are mutable outside a value scope
maths := {
    pi := 3.1415;
};
maths.pi = 5; // error! pi is const!

// Type scopes can have other type scopes defined within
Box: {
    pos: Point = {}; // = {} is required otherwise "pos" is a type scope instead of a value scope name
    size: {
        w: f32 = 0;
        h: f32 = 0;
    } = {};
    
    bottom_right := {
        result: Point = {
            x = pos.x + size.w;
            y = pos.y + size.h;
        };
    };
};
b: Box = {
    size.w = 24;
    size.h = 24;
    // could do value scope initialisation - size = {w = 24; h = 24;};
};
br := b.bottom_right.result; // br == Point {x = 24; y = 24;};
```
