{%
 % Example Diamond to test procedure and nested LOOP loops
 %}

% global variables
var i :int
const n :int := 3
var space :int := n

procedure printSpace()
	var j :int
	j := 1
	loop
		exit when j >= space
		put " "
		j := j + 1
	end loop
	return
end printSpace

procedure printStar()
	var j :int
	j := 1
	loop
		exit when j > i*2-1
		put "*"
		j := j + 1
	end loop
	return 
end printStar

% main program
put("This is Diamond with n: ")
put n
skip

% upper triangle
i := 1
loop
	exit when i > n
	printSpace()
	printStar()
	skip
	i := i + 1
	space := space - 1
end loop

% lower triangle
space := space + 2
i := n - 1
loop
	exit when i < 1
	var j:int
	printSpace()
	printStar()
	skip
	i := i - 1
	space := space + 1
end loop


