package.cpath=package.cpath..";../priv/?.so;"
require "pack"

bpack=string.pack
bunpack=string.unpack

function hex(s)
   s=string.gsub(s,"(.)",function (x) return string.format("%02X",string.byte(x)) end)
   return s
end

a=bpack("Ab8","\027Lua",5*16+1,0,1,4,4,4,8,0)
print(hex(a),string.len(a))
