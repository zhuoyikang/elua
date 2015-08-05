function Sleep(n)
   os.execute("sleep " .. n)
end

print "lua sleep begin"
Sleep(2)
print "lua sleep end"
