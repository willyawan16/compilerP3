{%
 % Example with Nested IF
 %}

% global variables
const a := 10
const asd := "asdasd"
var c: int := 4+34
var d :bool := true and false or true
var e :int :=1+3+4+2+a
var total :int := 0
var mul := 1

function smaller(a :int, b: int) :int
	if a<b then
		result a 
	else
		result b
	end if
	result 0
end smaller

function bigger(a :int, b: int) :int
	if (a>b) then
		result a
	else
		result b
	end if
	result 0
end bigger

for e: 1..a 
	
	total := total + e
	var i := 1
	put e
	%put "("
	loop 
		exit when i > e
		mul := mul * i
		i := i + 1
		%put mul
		%put "."
	end loop
	%put ")"
end for

put "<>"
for e: 1..a 		
	var i := 1
	total := total + e
	put i
end for

put total

begin
	var i :int := 200+200
	var j :int
	put i
	begin
		var k :int := a
		var l := 1+3+2*3
		put asd
	end
	begin
		var i :bool := (true and true or false) and 1<4
		const ok := false
		var j :int
		var k :int
		i:= ok and true
		put i
	end
	put i
end


