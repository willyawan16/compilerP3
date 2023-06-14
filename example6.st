{%
 % Example of nested IF, nested FOR loops, nested LOOP loops
 %}

% global variables
var a :int := 6
var b := 4
var c :int
var i :int
var j :int
var k :int
const n := 5

c := a - b -1
if a>b then
	put "a is greater than b\n"
	
	if (b>c) then
		put "b is greater than c\n"
	else 
		put "b is not greater than c\n"
	end if
else
	put "a is not greater than b\n"
	
	if (a>c) then
		put "a is greater than c\n"
	else 
		put "a is not greater than c\n"
	end if
end if

if a>b then
	put "a is greater than b\n"
	
	if (b>c) then
		put "b is greater than c\n"
		if a>c then
			put "a is greater than c\n"
		else
			put "a is not greater than c\n"
		end if
	else 
		put "b is not greater than c\n"
	end if
else
	put "a is not greater than b\n"
	
	if (a>c) then
		put "a is greater than c\n"
		if c>b then
			put "c is greater than b\n"
		else
			put "c is not greater than b\n"
		end if
	else 
		put "a is not greater than c\n"
	end if
end if

skip

if (a > b and b > c) then
    put ("a is greater than b and b is greater than c.\n")
        
    if (a mod 2 = 0) then
        put ("a is an even number.\n")
            
        if (b mod 2 = 0) then
            put ("b is also an even number.\n")
                
            if (c mod 2 = 0) then
                put ("c is also an even number.\n")
            else
                put ("c is not an even number.\n")
            end if
        else
                put ("b is not an even number.\n")
        end if
    else
        put ("a is not an even number.\n")
    end if
        
else
        put ("The condition a > b && b > c is not satisfied.\n")
end if

skip
for decreasing i : n..1
	put i
	skip
	for j : 1..n
		put ">"
		put j
		skip
		for k : 1..n
			put ">>"
			put k	
		end for
		skip
	end for
	skip
	skip
end for

skip
i:= 1
loop
	exit when i > n
	var j := 1
	loop 		
		exit when j > i
		var k := 1
		loop
			exit when k > j
			put k
			put " "
			k := k + 1
		end loop
		j := j + 1
		skip
	end loop
	i := i + 1
	skip
end loop



