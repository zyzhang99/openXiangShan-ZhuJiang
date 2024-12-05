local sim, dut = sim, dut
local await_posedge = await_posedge
local setmetatable = setmetatable
local setfenv = setfenv
local assert = assert
local print = print
local type = type
local format = string.format

local clock = dut.clock:chdl()

local function posedge(cycles, cycle_action_func)
    if cycles == nil then
        clock:posedge()
    else
        local func = cycle_action_func or function (cycle) end
        assert(cycles > 0)
        clock:posedge(cycles, func)
    end
end

local function negedge(cycles, cycle_action_func)
    if cycles == nil then
        clock:negedge()
    else
        local func = cycle_action_func or function (cycle) end
        assert(cycles > 0)
        clock:negedge(cycles, func)
    end
end

local function dut_reset()
    -- clock:posedge()
    dut.reset = 1
    clock:negedge(10)
    dut.reset = 0
    -- clock:negedge()
end

-- 
-- Test case manage
-- 
local test_count = 0
local last_test_name = "Unknown"
local is_start_test = false
local function __start_case_test__(name)
    assert(is_start_test == false, "[start_case_test] last test => " .. last_test_name .. " is not finished!")

    print(format([[
-----------------------------------------------------------------
| [%d] start test case ==> %s
-----------------------------------------------------------------]], test_count, name))
    last_test_name = name
    is_start_test = true
end

local function __end_case_test__(name)
    assert(is_start_test == true, "[end_case_test] no test has been started!")

    print(format([[
-----------------------------------------------------------------
| [%d] end test case ==> %s
-----------------------------------------------------------------
]], test_count, name))
    is_start_test = false
    test_count = test_count + 1
end

local function register_test_case(case_name)
    assert(type(case_name) == "string")

    return function(func_table)
        assert(type(func_table) == "table")
        assert(#func_table == 1)
        assert(type(func_table[1]) == "function")
        local func = func_table[1]

        local old_print = print
        local new_env = {
            print = function(...)
                print("|", ...)
            end
        }

        setmetatable(new_env, { __index = _G })
        setfenv(func, new_env)

        return function (...)
            __start_case_test__(case_name)
            func(...)    
            __end_case_test__(case_name)
        end
        
    end
end

local function TEST_SUCCESS()
    print("total_test_cases: <" .. test_count .. ">\n")
    print(">>>TEST_SUCCESS!<<<")
    print(colors.green .. [[
  _____         _____ _____ 
 |  __ \ /\    / ____/ ____|
 | |__) /  \  | (___| (___  
 |  ___/ /\ \  \___ \\___ \ 
 | |  / ____ \ ____) |___) |
 |_| /_/    \_\_____/_____/ 
]] .. colors.reset)
    io.flush()
    sim.finish()
end

local function expect_happen_until(limit_cycles, func)
    assert(type(limit_cycles) == "number")
    assert(type(func) == "function")
    local ok = dut.clock:posedge_until(limit_cycles, func)
    assert(ok)
end

local function expect_not_happen_until(limit_cycles, func)
    assert(type(limit_cycles) == "number")
    assert(type(func) == "function")
    local ok = dut.clock:posedge_until(limit_cycles, func)
    assert(not ok)
end

return {
    posedge                 = posedge,
    negedge                 = negedge,
    dut_reset               = dut_reset,
    TEST_SUCCESS            = TEST_SUCCESS,
    register_test_case      = register_test_case,
    expect_happen_until     = expect_happen_until,
    expect_not_happen_until = expect_not_happen_until
}
