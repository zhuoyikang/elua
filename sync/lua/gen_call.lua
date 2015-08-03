function test(i)
   print(name)
end

function test_int(i)
   return i
end


function test_int2(a,b)
   return a+b
end

function test_int3(a,b,c)
   return a+b+c
end

function test_int4(a,b,c,d)
   return a+b+c+d
end

function test_int2_ret2(a,b)
   return a,b
end

function test_int3_ret3(a,b,c)
   return a,b,c
end


function test_int_no_ret(i)
   i=1
end

function test_int_input0_ret1()
   return 23
end


function test_string1(a)
   return a
end

function test_string_no_ret(a)
end


function test_string2(a,b)
   return a..b
end

function test_string2_ret2(a,b)
   return a,b
end

function test_string_string0_ret1()
   return "nice"
end

function test_int_string(input,b)
   for i=1,input do
      b=b..b
   end
   return b
end
