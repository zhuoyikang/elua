package.cpath=package.cpath..";../priv/?.so;priv/?.so;"
-- require "emt"

-- emt.sleep(2,1000)

local startTime = os.clock();
local strs = {};
for i = 1, 80000, 1 do
   strs[i] = "helloworld";
end
local endTime = os.clock();
local useTime = endTime - startTime;
-- print("消耗时间：" .. useTime .. "s");
