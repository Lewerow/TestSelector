local functional = require('functional')

local suspensions = {
  "share/lua",
  "share\\lua",
  "rocks/systree",
  "rocks\\systree",
  "lua/luarocks",
  "lua\\luarocks",
  "_spec"
}

local function is_suspended(luacov_line)
  for _, suspension in ipairs(suspensions) do
    if string.find(luacov_line, suspension) then  
      return true 
    end
  end
  
  return false
end

return is_suspended