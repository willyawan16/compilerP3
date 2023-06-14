{%
 % Example with Functions
 %}

% global variables
const a := 10
const asd := "asdasd"
var c: int := 4+34
var d :bool := true and false or true
var e :int :=1+3+4+2+a

function add(a :int, b: int) :int
	result a+b
end add

procedure printAdd(a :int, b: int)
	put a+b
	put not 1<3
	return
end printAdd

c := add(a, 10)
printAdd(a, add(a, 100))

