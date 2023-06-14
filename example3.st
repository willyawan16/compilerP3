{%
 % Example with Functions
 %}

% global variables
const a := 10
const asd := "asdasd"
var c: int := 4+34
var d :bool := true and false or true
var e :int :=1+3+4+2+a

begin
	var i :int
	i:=1 
	loop 
		exit when (i>10) 
		i:=i+1 
		loop 
			exit when (i>10) 
			i:=i+1 
		end loop
	end loop

	for i : 1..10 
		put i
		i:=i+1 
	end for
end

