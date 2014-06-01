local lfs = require('lfs')
local is_suspended = require('suspensions')

local expected_indices_count = 0
local expected_indices = {}

local function assert_positive(var, message)
  if var then return end
  
  io.stderr:write(message,'\n')
  io.stderr:flush()
  os.exit(1)    
end

local function path_to_output_formatter()
  local formatterpath = arg[0]
  if not string.find(formatterpath,"/") and not string.find(formatterpath, "\\") then
    formatterpath = lfs.currentdir()
  else
    formatterpath = string.gsub(formatterpath, '(.*/)*[/\\].*', "%1")
  end
  
  assert_positive(formatterpath, "Couldn't find path to output formatter")
  
  return formatterpath    
end

local function trim(s)
  return s:match'^%s*(.*%S)' or ''
end

local function load_file_as_lines(filename)
  local lines = {}
  for line in io.lines(filename) do
    lines[#lines+1] = (line ~= '' and line) or nil
  end
  
  return lines
end

local function coverage()  
  local data = load_file_as_lines("luacov.stats.out")
  local outputs = {}
  for i=1,#data,2 do
    if not is_suspended(data[i]) then
      outputs[#outputs + 1] = {data[i],trim(data[i+1])}
    end
  end
  
  for k, v in ipairs(outputs) do
    local covs = {}
    for cov in string.gmatch(v[2], "%w+") do
      covs[#covs + 1] = (cov == '0' and '0') or '1'
    end
    outputs[k] = {v[1], table.concat(covs)} 
  end
  
  assert(#outputs > 0, "Why in heavens run a test that checks only libraries and no SUT?")  
  return outputs
end

local function calculate_coverage(coverage_data)
  assert(coverage_data)
  local function add_to_all(index, value)
    for _, v in ipairs(coverage_data) do
      local found = false
      for _, l in ipairs(v.coverage) do
        if l[1] == index then found = true end
      end
      
      if not found then
        v.coverage[#v.coverage + 1] = {index, value}
      end
    end
  end
  
  local verified = {}
  for _, v in ipairs(coverage_data) do
    assert(v.coverage)
    for _, opt in ipairs(v.coverage) do
      if not verified[opt[1]] then add_to_all(opt[1], string.rep('0', #opt[2])) end
      verified[opt[1]] = true
    end
  end
  
  for _, v in ipairs(coverage_data) do
    table.sort(v.coverage, function(node1, node2) return node1[1] > node2[1] end)
    for k, u in ipairs(v.coverage) do
      v.coverage[k] = u[2]
    end
    v.coverage = table.concat(v.coverage)
  end
  
  return coverage_data
end

local chosen_tag = arg[1] or ""
local chosen_path = arg[2] or "."

io.stdout:write("Test directory: " .. chosen_path, '\n')
if chosen_tag == "" then
  io.stdout:write("Executing only tests that match tag: " .. chosen_tag .. '\n')
else
  io.stdout:write("Executing all tests \n")
end

local luapath = os.getenv("LUA_PATH")
assert_positive(luapath, "LUA_PATH not set, busted tests won't work")

local filename = os.tmpname()
local formatter = path_to_output_formatter() .. "/listing.lua"

local invocation ='busted --output=listing --tags="' .. chosen_tag .. '" --lpath=' .. formatter .. ";" .. luapath .. " " .. chosen_path .." >" .. filename

io.stdout:write("Calling busted using invocation: \n" .. invocation, '\n')

local res = os.execute(invocation)
assert_positive(res ~= 0, "Could not execute busted tests, or at least one of the tests failed.")

local lines = load_file_as_lines(filename)
io.stdout:write("Finished preparing environment, executing tests one by one\n")
os.remove(filename)
filename = nil

local coverage_data = {}
local per_testcase_formatter = path_to_output_formatter() .. "/per_testcase.lua"
for k, name in pairs(lines) do
  os.remove("luacov.stats.out")
  local filename = os.tmpname()
--  local invocation = "busted --coverage -o per_testcase.lua" ..' --tags="' .. name .. '" . >' .. filename
  local invocation = 'busted --coverage --tags="' .. name .. '" . >' .. filename
  io.stdout:write("Gathering results (" .. k .. "/" .. #lines .. "): ", name, '\n')
  assert_positive(os.execute(invocation), "Failed to generate coverage data. Aborting")
  
  coverage_data[k] = {}
  coverage_data[k].id = k
  coverage_data[k].name = name
  coverage_data[k].filename = filename
  coverage_data[k].coverage = coverage()
end

coverage_data = calculate_coverage(coverage_data)
local results = io.open("data.txt", "w")
local mappings = io.open("mappings.txt", "w")
for _, v in ipairs(coverage_data) do
  local f = io.open(v.filename)
  local text = f:read("*a")
  f:close()
  os.remove(v.filename)
  local time = string.match(text, "%d%.%d*"),
  tostring(time)
  results:write(v.id, ';', trim(time),';0;', v.coverage,'\n')
  mappings:write(v.id, " --> ", v.name, '\n')
end

results:flush()
results:close()
mappings:flush()
mappings:close()

io.stdout:write("Completed generating input data, starting selection\n")
os.execute("Main.exe >selected.txt")